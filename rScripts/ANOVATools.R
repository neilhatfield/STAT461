# Check, Install, and Load Required Packages
req.packages <- c("DescTools", "sjstats", "dunn.test", "dplyr", "purrr")
new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {install.packages(new.packages)}
lapply(req.packages, require, character.only = TRUE)

# String Splitter (Helper Function) ----
.strsplitN <- function(x, N){
  temp0 <- strsplit(as.character(x), " - ")
  return(temp0[[1]][N])
}

# Probability of Superiority ----
## Calculate the Probability of Superiority from Cohen's D
probSup <- function(d){
  return(
    pnorm(-0.7071067 * d, mean = 0, sd = 1, lower.tail = FALSE)
  )
}

# Hodges-Lehmann Estimator ----
## Given two vectors, return the HL estimate
hodgesLehmann <- function(x, y){
  hl <- median(outer(x, y, "-"))
  return(hl)
}

# ANOVA Post Hoc ----
## Return a data frame of Post Hoc Results and Effect Sizes
anova.PostHoc <- function(aov.obj, response = NULL, mainEffect = NULL){
  df <- aov.obj$model
  if (missing(response) && missing(mainEffect)) {
    unDF <- unstack(df)
  }
  if (missing(response) && !missing(mainEffect)) {
    stop("You need to enter the response column name.")
  }
  if (!missing(response) && missing(mainEffect)) {
    stop("You need to enter the main effect of interest column name.")
  }
  if (!missing(response) && !missing(mainEffect)) {
    df <- subset(df, select = c(response, mainEffect))
    unDF <- unstack(df)
  }
  n <- (factorial(length(unDF)) / (2 * factorial(length(unDF) - 2)))
  temp0a <- rep(NA, n)
  temp0b <- rep(NA, n)
  temp0c <- rep(NA, n)
  k <- 1
  for (i in 1:(length(unDF) - 1)) {
    for (j in (i + 1):length(unDF)) {
      temp0a[k] <- paste0(names(unDF)[i], " vs. ", names(unDF)[j])
      temp0b[k] <- as.numeric(
        DescTools::CohenD(
          x = unlist(unDF[i]),
          y = unlist(unDF[j]),
          correct = FALSE,
          conf.level = NA,
          na.rm = TRUE
        )
      )
      temp0c[k] <- as.numeric(
        DescTools::CohenD(
          x = unlist(unDF[i]),
          y = unlist(unDF[j]),
          correct = TRUE,
          conf.level = NA,
          na.rm = TRUE
        )
      )
      k <- k + 1
    }
  }
  temp0 <- data.frame(
    Pair = temp0a,
    Cohens.d = temp0b,
    Hedges.g = temp0c
  )
  temp0$Cohens.d <- as.numeric(temp0$Cohens.d)
  temp0$'Prob.Super' <- probSup(temp0$Cohens.d)
  return(temp0)
}

# Post Hoc for Kruskal-Wallis ----
## Given data vector and group vector, return data frame of post hoc results
kw.PostHoc <- function(response, treatments){
  temp0 <- data.frame(x = response, g = treatments)
  sizes <- temp0 %>%
    dplyr::group_by(g) %>%
    dplyr::summarize(n = n())

  temp1 <- purrr::quietly(dunn.test::dunn.test)(
    x = response,
    g = treatments,
    kw = FALSE,
    table = FALSE
  )$result

  output <- data.frame(comp = temp1$comparison)
  output$A <- sapply(
    X = output$comp,
    FUN = .strsplitN,
    N = 1
  )
  output$B <- sapply(
    X = output$comp,
    FUN = .strsplitN,
    N = 2
  )
  output$z <- temp1$Z
  output$pbs <- temp1$Z
  output$hl <- NA
  output$PS <- NA

  output$sizeA <- sapply(
    X = 1:nrow(output),
    FUN = function(x){
      return(sizes$n[which(sizes$g == output$A[x])])
    }
  )
  output$sizeB <- sapply(
    X = 1:nrow(output),
    FUN = function(x){
      return(sizes$n[which(sizes$g == output$B[x])])
    }
  )

  output <- output %>%
    dplyr::mutate(
      pbs = pbs / sqrt(sizeA + sizeB)
    )

  for (i in 1:nrow(output)) {
    temp2 <- temp0 %>%
      dplyr::filter(g == output$A[i] | g == output$B[i])
    temp3 <- purrr::compact(unstack(temp2))
    output$hl[i] <- hodgesLehmann(temp3[[1]], temp3[[2]])
  }

  output$pbs <- sapply(
    X = output$pbs,
    FUN = function(x){
      return(
        ifelse(
          test = x <= -1,
          yes = -0.9999,
          no = ifelse(
            test = x >= 1,
            yes = 0.9999,
            no = x
          )
        )
      )
    }
  )

  output$PS <- sapply(
    X = output$pbs,
    FUN = function(x){
      return(
        probSup((2 * x) / sqrt(1 - (x)^2))
      )
    }
  )

  output <- dplyr::select(output, c("comp", "hl", "PS"))
  names(output) <- c("Comparison", "Hodges.Lehman","Prob. Super")

  return(output)
}

# Relative Efficiency for Using Blocks
## Given model object and names of blocks/factors, return relative efficiency
block.RelEff <- function(aov.obj, blockName, trtName){
  temp0 <- anova(aov.obj)
  g <- temp0[trtName, "Df"]
  r <- temp0[blockName, "Df"]
  rcb <- r * g
  crd <- (g + 1) * r
  MSE <- temp0$`Mean Sq`[3]
  MSblk <- temp0[blockName, "Mean Sq"]
  sCRD <- (r * MSblk + (g + rcb) * MSE) / (r + g + rcb)
  eff <- (((rcb + 1) * (crd + 3)) / ((rcb + 3) * (crd + 1))) * (sCRD / MSE)
  return(
    paste0(
      "The relative efficiency of the Block, ",
      blockName, ", is ", round(eff, 3), "."
    )
  )
}

# Fix ANOVA Tables for Multi-factor Models with Random Effects
anovaFixer <- function(aov.obj, fixed, random, type = "unrestricted"){
  if (is.null(fixed) && is.null(random)) {
    print("You are missing arguments")
    return(aov.obj)
  }
  if ((is.null(fixed) && length(random) == 1) ||
      (length(fixed) == 1 && is.null(random))) {
    print("One-way design detected; there is nothing to fix.")
    return(aov.obj)
  }
  if (length(fixed) + length(random) == 2) {
    if (length(fixed) == 2) {
      print("Two-way Fixed Effect design detected; there is nothing to fix.")
      return(aov.obj)
    } else if (length(random) == 2 ||
               (length(random) == 1 && type == "unrestricted")) {
      print("Using the interaction term for F Ratios...")
      temp0 <- anova(aov.obj)
      temp0[1, "F value"] <- temp0[1, "Mean Sq"] / temp0[3, "Mean Sq"]
      temp0[2, "F value"] <- temp0[2, "Mean Sq"] / temp0[3, "Mean Sq"]
      temp0[1, "Pr(>F)"] <- pf(temp0[1, "F value"], df1 = temp0[1, "Df"],
                               df2 = temp0[3, "Df"], lower.tail = FALSE)
      temp0[2, "Pr(>F)"] <- pf(temp0[2, "F value"], df1 = temp0[2, "Df"],
                               df2 = temp0[3, "Df"], lower.tail = FALSE)
      return(temp0)
    } else {
      print("Using a restricted model...")
      temp0 <- anova(aov.obj)
      temp0[random, "F value"] <- temp0[random, "Mean Sq"] / temp0[3, "Mean Sq"]
      temp0[random, "Pr(>F)"] <- pf(temp0[random, "F value"], df1 = temp0[random, "Df"],
                               df2 = temp0[3, "Df"], lower.tail = FALSE)
      return(temp0)
    }
  }
  if (length(fixed) + length(random) > 2 ) {
    if (length(random) == 0) {
      print("No random effects listed. There is nothing to fix.")
      return(aov.obj)
    } else {
      print("This function has not been updated for more than twoway fixes.")
      return(aov.obj)
    }
  }
}