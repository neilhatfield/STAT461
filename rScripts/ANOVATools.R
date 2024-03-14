# Check, Install, and Load Required Packages
req.packages <- c("DescTools", "sjstats", "dunn.test", "dplyr", "purrr", "NSM3")
new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {install.packages(new.packages)}
lapply(req.packages, require, character.only = TRUE, quietly = TRUE)

# P Value Rounding ----
pvalRound <- function(x, digits = 4){
  checkVal <- 1 * 10^(-1*digits)
    ifelse(
      test = x < checkVal,
      yes = paste("<", format(checkVal, scientific = FALSE)),
      no = format(round(x, digits = digits), scientific = FALSE)
    )
  }

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
      "The relative efficiency of the block, ",
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
      (length(fixed) == 1 && is.null(random))
  ) {
    print("One-way design detected; there is nothing to fix.")
    return(aov.obj)
  }
  if (length(fixed) + length(random) == 2) {
    if (length(fixed) == 2) {
      print("Two-way Fixed Effect design detected; there is nothing to fix.")
      return(aov.obj)
    } else if (length(random) == 2 ||
               (length(random) == 1 && type == "unrestricted")
    ) {
      print("Using the interaction term for F Ratios...")
      temp0 <- anova(aov.obj)
      temp0[1, "F value"] <- temp0[1, "Mean Sq"] / temp0[3, "Mean Sq"]
      temp0[2, "F value"] <- temp0[2, "Mean Sq"] / temp0[3, "Mean Sq"]
      temp0[1, "Pr(>F)"] <- pf(
        q = temp0[1, "F value"],
        df1 = temp0[1, "Df"],
        df2 = temp0[3, "Df"],
        lower.tail = FALSE
      )
      temp0[2, "Pr(>F)"] <- pf(
        q = temp0[2, "F value"],
        df1 = temp0[2, "Df"],
        df2 = temp0[3, "Df"],
        lower.tail = FALSE
      )
      return(temp0)
    } else {
      print("Using a restricted model...")
      temp0 <- anova(aov.obj)
      temp0[random, "F value"] <- temp0[random, "Mean Sq"] / temp0[3, "Mean Sq"]
      temp0[random, "Pr(>F)"] <- pf(
        q = temp0[random, "F value"],
        df1 = temp0[random, "Df"],
        df2 = temp0[3, "Df"],
        lower.tail = FALSE
      )
      return(temp0)
    }
  }
  if (length(fixed) + length(random) > 2 ) {
    if (length(random) == 0) {
      print("No random effects listed. There is nothing to fix.")
      return(aov.obj)
    } else {
      print("This function has not been updated for more than two-way fixes.")
      return(aov.obj)
    }
  }
}

# Sphericity Plot ----
## Make a plot of pairwise comparisons of treatments for each subject
sphericityPlot <- function(dataWide, subjectID, colsIgnore = NULL, colors = "default"){
  require(tidyverse)
  if (colors == "boast" | colors == "psu") {
    if ("boastUtils" %in% installed.packages()[,"Package"]) {
      require(boastUtils)
      palette <- colors
    } else {
      warning("boastUtils package not installed; changing colors to default")
      palette <- "default"
    }
  } else {
    palette <- colors
  }

  if (!is.null(colsIgnore)) {
    dataWide <- dataWide %>%
      dplyr::select(!all_of(colsIgnore))
  }

  if (length(subjectID) == 1) {
    temp1 <- tibble::column_to_rownames(
      dataWide,
      var = subjectID
    )
  } else if (length(subjectID) > 1) {
    dataWide <- dataWide %>%
      unite(
        col = "id",
        all_of(subjectID),
        remove = TRUE
      )
    temp1 <- tibble::column_to_rownames(
      dataWide,
      var = "id"
    )
  }

  temp2 <- data.frame(row.names = row.names(temp1))
  for (i in 1:(ncol(temp1) - 1)) {
    for (j in (i + 1):ncol(temp1)) {
      temp2[,paste(names(temp1)[i], names(temp1)[j], sep = " vs. ")] <- temp1[,i] - temp1[,j]
    }
  }
  temp2 <- tibble::rownames_to_column(
    .data = temp2,
    var = paste(subjectID, collapse = ":")
  )

  temp2 <- pivot_longer(
    data = temp2,
    cols = !all_of(paste(subjectID, collapse = ":")),
    names_to = "comparison",
    values_to = "difference"
  )

  plot <- ggplot(
    data = temp2,
    mapping = aes(
      x = comparison,
      y = difference,
      color = comparison
    )
  ) +
    geom_jitter(size = 2, width = 0.2, height = 0.2) +
    theme_bw() +
    xlab("Comparison") +
    ylab("Difference") +
    theme(
      legend.position = "none"
    ) +
    scale_x_discrete(
      labels = label_wrap_gen(width = 10),
      guide = guide_axis(angle = 45)
    )

  if (palette == "boast") {
    plot <- plot +
      scale_color_manual(values = boastUtils::boastPalette)
  } else if (palette == "psu") {
    plot <- plot +
      scale_color_manual(values = boastUtils::psuPalette)
  }

  return(plot)
}

# DSCF Test ----
## Given an apparent issue with the PMCMRplus package, I'm writing
## a function for the DSCF test
dscfTest <- function(response, factor){
  temp1 <- NSM3::pSDCFlig(
    x = response,
    g = factor,
    method = "Asymptotic"
  )
  newLabels <- gsub(
    pattern = " - ",
    replacement = " vs. ",
    x = temp1$labels
  )
  temp2 <- data.frame(
    Comparison = newLabels,
    w = temp1$obs.stat,
    `p-value` = temp1$p.val
  )
  return(temp2)
}

# pValue Adjustments ----
## A wrapper to apply the p.adjust function to contrasts without applying the
## adjustment to the omnibus test.
adjustPValues <- function(contrastObject, method = "bonferroni"){
  temp1 <- contrastObject[[1]] # Grab the appropriate output
  temp1$contrastIndicator <- stringr::str_detect(row.names(temp1), pattern = ":")
  colnames(temp1)[which(colnames(temp1) == "Pr(>F)")] <- "rawP"
  psNeedingAdj <- temp1$rawP[which(temp1$contrastIndicator == TRUE)]
  adjPs <- p.adjust(p = psNeedingAdj, method = method)
  temp1$adjP <- sapply(
    X = 1:nrow(temp1),
    FUN = function(x){
      i <- 1
      while (i <= length(adjPs)) {
        if (temp1$contrastIndicator[x] == TRUE) {
          return(adjPs[i])
        } else {
          return(NA)
        }
        i <- i + 1
      }
    }
  )

  temp1 <- temp1[,-which(colnames(temp1) %in% c("contrastIndicator", "method", "m"))]

  return(temp1)
}

# Anova Screens Function ----
## A function that takes a data frame, and two strings to return the
## Oneway ANOVA screens
anovaScreensT <- function(dataFrame, response, factor, block = NULL) {
  require(dplyr)
  require(rlang)
  
  ## Error Checking
  errorMessages <- c()
  if (length(factor) <= 0 | is.na(factor) | is.null(factor)) {
    stop("You need to specify the name of your factor")
  } else if (length(factor) > 1) {
    stop("This function is currently only built for Oneway ANOVA")
  }

  if (length(response) <= 0 | is.na(response) | is.null(response)) {
    stop("You need to specify the name of your response")
  }

  if (!(response %in% names(dataFrame))) {
    stop(paste("The response you gave,", response, "was not found in the data frame supplied."))
  } else if (!(factor %in% names(dataFrame))) {
    stop(paste("The factor you gave,", factor, "was not found in the data frame supplied."))
  } 
  
  if (!is.null(block) && (block %in% names(dataFrame))) {
    stop(paste("The block you gave,", block, "was not found in the data frame supplied."))
  }

  ## Build Screens
  if (!is.null(block)) {
    screens <- dataFrame %>%
      dplyr::select(!!sym(response), !!sym(block), !!sym(factor)) %>%
      mutate(
        Screen1.Action = mean(!!sym(response), na.rm = TRUE)
      )
  } else {
    screens <- dataFrame %>%
      dplyr::select(!!sym(response), !!sym(factor)) %>%
      mutate(
        Screen1.Action = mean(!!sym(response), na.rm = TRUE)
      )
  }
  
  screens <- na.omit(screens)
  
  ## Get Block Means
  if (!is.null(block)) {
    blockMeans <- screens %>%
      group_by(!!sym(block)) %>%
      summarize(
        blockMean = mean(!!sym(response), na.rm = TRUE)
      )
  } 
  
  ## Get Factor Means
  factorMeans <- screens %>%
    group_by(!!sym(factor)) %>%
    summarize(
      factorMean = mean(!!sym(response), na.rm = TRUE)
    )
  
  ## Build Resulting Data Frame
  if (is.null(block)) {
    screens <- screens %>%
      left_join(
        y = factorMeans,
        by = dplyr::join_by(!!sym(factor))
      ) %>%
      mutate(
        Screen2.Factor = factorMean - Screen1.Action,
        Screen3.Residuals = !!sym(response) - Screen1.Action - Screen2.Factor
      ) %>%
      dplyr::select(-factorMean)
    
    return(screens)
  } else {
    screens <- screens %>%
      left_join(
        y = blockMeans,
        by = dplyr::join_by(!!sym(block))
      ) %>%
      mutate(
        Screen2.Block = blockMean - Screen1.Action
      ) %>%
      left_join(
        y = factorMeans,
        by = dplyr::join_by(!!sym(factor))
      ) %>%
      mutate(
        Screen3.Factor = factorMean - Screen1.Action - Screen2.Block,
        Screen4.Residuals = !!sym(response) - Screen1.Action - Screen2.Block - Screen3.Factor
      ) %>%
      dplyr::select(-factorMean, -blockMean)
    
    return(screens)
  }
}

# Type I Risk Per Inference Act----
## A function which returns the Type 1 Risk at the per inference act
## given a particular methodology
perActRisk <- function(type1Risk, numActs, method = "Bonferroni", outType = "num"){
  stopifnot(
    "Specify an overall Type I Risk between 0 and 1" = type1Risk <= 1,
    "Specify an overall Type I Risk between 0 and 1" = type1Risk > 0,
    "Specify a positive integer for the number of inference acts" = all(numActs > 0)#,
    # "Specify a positive integer for the number of inference acts" = !is.integer(numActs)
  )
  
  if (method %in% c("Bonferroni", "bonferroni", "Bon", "bon")) {
    ind <- type1Risk / numActs
    reportMethod <- "Bonferroni"
  } else if (method %in% c("Sidak", "sidak", "Sid", "sid")) {
    ind <- 1 - (1 - type1Risk)^(1 / numActs)
    reportMethod <- "Sidak"
  }
  
  if (outType %in% c("words")) {
    return(paste("The individual Type 1 Risk using the", reportMethod, "method is", round(ind, digits = 4)))
  } else if (outType %in% c("num", "number", "numbers", "Num", "Number")) {
    return(round(ind, digits = 3))
  }
}
perActRisk <- Vectorize(perActRisk)

