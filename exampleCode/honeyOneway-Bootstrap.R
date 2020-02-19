# Honey Demo 3-Bootstrap
# We will use a Significance Level/Unusualness Threshold of 0.03

# Packages needed
packages <- c("tidyverse", "knitr", "kableExtra",
              "boot", "lmboot")
lapply(packages, library, character.only = TRUE)

# Get Data
honey <- data.frame(
  Surplus = c(150, 50, 100, 85, 90, 95, 130, 50, 80),
  Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
)

bootF <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(anova(fit)$'F value'[1])
}
set.seed(461)
boot.out <- boot::boot(
  data = honey,
  statistic = bootF,
  stype = "i",
  R = 2500,
  formula = Surplus ~ Varietal
)

boot::boot.ci(boot.out, conf = 0.93, type = "BCa")

out1 <- lmboot::ANOVA.boot(Surplus ~ Varietal,
                           data = honey,
                           B = 2500,
                           type = "residual",
                           seed = 461)

############################################################
data("InsectSprays")
set.seed(461)
boot.outS <- boot::boot(
  data = InsectSprays,
  statistic = bootF,
  stype = "i",
  R = 2500,
  formula = count ~ spray
)

boot::boot.ci(boot.outS, conf = 0.93, type = "bca")