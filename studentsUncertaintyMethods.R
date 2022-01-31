# Students measurement uncertainty methods ----

## Kwitkowski-Conrad Method ----
##  Sample Arithmetic (?) Mean +- 1/2 * SASD
kwitkowskiConrad <- function(x){
  lB <- mean(x, na.rm = TRUE) - 0.5 * sd(x, na.rm = TRUE)
  uB <- mean(x, na.rm = TRUE) + 0.5 * sd(x, na.rm = TRUE)
  return(list("lower" = lB, "upper" = uB))
}

## M&M Method ----
## Sample Arithmetic (?) Mean +- MAD
## Multiple Groups: Range Revolving Around Mean
mM <- function(x){
  lB <- mean(x, na.rm = TRUE) - mad(x, na.rm = TRUE)
  uB <- mean(x, na.rm = TRUE) + mad(x, na.rm = TRUE)
  return(list("lower" = lB, "upper" = uB))
}

## Single Standard Deviation Method ----
## Sample Arithmetic (?) Mean +- SASD
## Multiple Groups: Rainbow Kitty Mu Method,
singleSD <- function(x){
  lB <- mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE)
  uB <- mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)
  return(list("lower" = lB, "upper" = uB))
}

## IQR Method ---
## Use the first and third quartiles
## Multiple Groups
iqrMethod <- function(x){
  lB <- quantile(x, probs = 0.25, na.rm = TRUE)
  uB <- quantile(x, probs = 0.75, na.rm = TRUE)
  return(list("lower" = lB, "upper" = uB))
}

## Method X ---
## Median +- MAD
xMethod <- function(x){
  lB <- median(x, na.rm = TRUE) - mad(x, na.rm = TRUE)
  uB <- median(x, na.rm = TRUE) + mad(x, na.rm = TRUE)
  return(list("lower" = lB, "upper" = uB))
}

## 461 Terminator ----
## Sample Arithmetic (?) Mean +- SE
terminator461 <- function(x){
  lB <- mean(x, na.rm = TRUE) - (sd(x, na.rm = TRUE)/sqrt(na.omit(length(x))))
  uB <- mean(x, na.rm = TRUE) + (sd(x, na.rm = TRUE)/sqrt(na.omit(length(x))))
  return(list("lower" = lB, "upper" = uB))
}

## Mean-Variance Interval ----
### Sample Arithmetic Mean +- 2 * SE
meanVarianceInterval <- function(x){
  lB <- mean(x, na.rm = TRUE) - 2 * (sd(x, na.rm = TRUE)/sqrt(na.omit(length(x))))
  uB <- mean(x, na.rm = TRUE) + 2 * (sd(x, na.rm = TRUE)/sqrt(na.omit(length(x))))
  return(list("lower" = lB, "upper" = uB))
}

## Median Manipulation ----
## Sample Median +- 1/2 * IQR
medianManipulation <- function(x){
  lB <- median(x, na.rm = TRUE) - 0.5 * IQR(x, na.rm = TRUE)
  uB <- median(x, na.rm = TRUE) + 0.5 * IQR(x, na.rm = TRUE)
  return(list("lower" = lB, "upper" = uB))
}

## CLT Based ----
## Z for 99% is 2.576
cltCI <- function(x){
  lB <- mean(x, na.rm = TRUE) - 2.576 * sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))
  uB <- mean(x, na.rm = TRUE) + 2.576 * sd(x, na.rm = TRUE)/swrt(length(na.omit(x)))
  return(list("lower" = lB, "upper" = uB))
}


# Testing the Methods ----
set.seed(461)
samples <- list()
samples <- lapply(
  X = 1:10000,
  FUN = function(x){
    samples[x] <- round(rlnorm(25, meanlog = log(4), sdlog = 1), 1) + 12
  }
)

target <- exp(log(4) + 0.5 * 1^2) + 12

studentResults <- data.frame(
  name = c("drop"),
  successRate = as.numeric(0),
  samWidth = as.numeric(0)
)

temp0 <- rep(0, 10000)
temp1 <- rep(0, 10000)
temp0 <- sapply(
  X = 1:10000,
  FUN = function(x){
    lB <- kwitkowskiConrad(samples[[x]])$lower
    uB <- kwitkowskiConrad(samples[[x]])$upper
    ifelse(
      test = lB < target & uB > target,
      yes = 1,
      no = 0
    )
  }
)

temp1 <- sapply(
  X = 1:10000,
  FUN = function(x){
    lB <- kwitkowskiConrad(samples[[x]])$lower
    uB <- kwitkowskiConrad(samples[[x]])$upper
    uB - lB
  }
)
studentResults <- rbind(
  studentResults,
  c(name = "kwitkowskiConrand", successRate = mean(temp0), samWidth = mean(temp1))
)


# Distance Data ----
garminData <- c(
  3.49, 3.49, 3.47, 3.49, 3.51,
  3.50, 3.49, 3.48, 3.49, 3.50,
  3.50, 3.48, 3.48, 3.42, 3.49,
  3.48, 3.49, 3.47, 3.48, 3.47,
  3.51, 3.51, 3.47, 3.49, 3.50
)