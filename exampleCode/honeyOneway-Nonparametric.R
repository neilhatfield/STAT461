# Honey Demo 2-Nonparametric
# We will use a Significance Level/Unusualness Threshold of 0.03

# Packages needed
packages <- c("tidyverse", "knitr", "kableExtra", "coin",
              "rcompanion")
lapply(packages, library, character.only = TRUE)

# Get Data
honey <- data.frame(
  Surplus = c(150, 50, 100, 85, 90, 95, 130, 50, 80),
  Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
)

## Kruskal-Wallis Test
kruskal.test(formula = Surplus ~ Varietal,
             data = honey,
             na.action = "na.omit")

coin::kruskal_test(
  formula = Surplus ~ Varietal,
  data = honey,
  ties.method = "mid-ranks")

## Effect Size
rcompanion::epsilonSquared(
  x = honey$Surplus,
  g = honey$Varietal)