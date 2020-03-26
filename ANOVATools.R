req.packages <- c("DescTools", "sjstats", "dunn.test")
new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(req.packages, require, character.only = TRUE)

anova.PostHoc <- function(aov.obj){
  df <- aov.obj$model
  unDF <- unstack(df)
  n <- (factorial(length(unDF))/(2*factorial(length(unDF)-2)))
  temp0a <- rep(NA, n)
  temp0b <- rep(NA, n)
  temp0c <- rep(NA, n)
  k <- 1
  for (i in 1:(length(unDF)-1)){
    for (j in (i+1):length(unDF)){
      temp0a[k] <- paste0(names(unDF)[i], " vs. ", names(unDF)[j])
      temp0b[k] <- as.numeric(DescTools::CohenD(x = unlist(unDF[i]),
                                                 y = unlist(unDF[j]),
                                                 correct = FALSE,
                                                 conf.level = NA,
                                                 na.rm = TRUE))
      temp0c[k] <- as.numeric(DescTools::CohenD(x = unlist(unDF[i]),
                                                 y = unlist(unDF[j]),
                                                 correct = TRUE,
                                                 conf.level = NA,
                                                 na.rm = TRUE))
      k <- k + 1
    }
  }
  temp0 <- data.frame(Pair = temp0a, Cohens.d = temp0b, Hedges.g = temp0c)
  temp0$Cohens.d <- as.numeric(temp0$Cohens.d)
  temp0$'Prob.Super' <- .probSup(temp0$Cohens.d)
  return(temp0)
}

.probSup <- function(d){
	pnorm(-0.7071067*d, mean = 0, sd = 1, lower.tail = FALSE)
}

.hodgesLehmann <- function(x, y){
  hl <- median(outer(x, y, "-"))
  return(hl)
}

.strsplitN <- function(x, N){
  temp0 <- strsplit(as.character(x), " - ")
  return(temp0[[1]][N])
}

kw.PostHoc <- function(x, g){
  temp0 <- data.frame(x, g)
  us <- unstack(temp0)
  sink("/dev/null")
  temp1 <- dunn.test::dunn.test(x, g)
  sink()

  output <- data.frame(comp = temp1$comparison)
  output$A <- sapply(output$comp, .strsplitN, N = 1)
  output$B <- sapply(output$comp, .strsplitN, N = 2)
  output$z <- temp1$Z
  output$pbs <- temp1$Z
  output$hl <- NA
  output$PS <- NA

  for (i in 1:nrow(output)){
    tempA <- unlist(us[output[i,"A"]], use.names = FALSE)
    tempB <- unlist(us[output[i,"B"]], use.names = FALSE)
    output[i, "pbs"] <- output[i, "pbs"] / (sqrt(length(tempA) + length(tempB)))
    output[i, "hl"] <- .hodgesLehmann(tempA, tempB)
  }

  output$pbs <- sapply(output$pbs, function(x){
    ifelse(x <= -1, -0.9999, ifelse(x >= 1, 0.9999, x))})

  output$PS <- sapply(output$pbs, function(x){
    .probSup((2 * x) / sqrt(1 - (x)^2))})

  output <- output[, -c("A","B","z","pbs")]
  names(output) <- c("Pair", "Hodges.Lehman","Prob. Super")

  return(output)
}

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
      blockName, ", is ", round(eff, 3)
    )
  )
}
