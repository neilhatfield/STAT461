req.packages <- c("DescTools", "sjstats")
new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(req.packages, require, character.only = TRUE)

anova.PostHoc <- function(aov.obj){
  df <- aov.obj$model
  unDF <- unstack(df)
  n <- (factorial(length(unDF))/(2*factorial(length(unDF)-2)))
  temp0a <- rep(NA, n)
  temp0b <- rep(NA,n)
  temp0c <- rep(NA,n)
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

.probSup<- function(d){
	pnorm(-0.7071067*d, mean = 0, sd = 1, lower.tail = FALSE)
}
