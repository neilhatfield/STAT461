# Honey Demo 2-Permutation
# We will use a Significance Level/Unusualness Threshold of 0.03

# Packages needed
packages <- c("tidyverse", "knitr", "kableExtra",
              "sjstats", "multicool")
lapply(packages, library, character.only = TRUE)

# Get Data
honey <- data.frame(
  Surplus = c(150, 50, 100, 85, 90, 95, 130, 50, 80),
  Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
)

# Get values of observed model
options(contrasts = c("contr.sum","contr.poly"))
model1 <- aov(Surplus ~ Varietal, data = honey)

# Set up the Permutation
honeyGroups <- multicool::initMC(honey$Varietal)
honeyPerms <- multicool::allPerm(honeyGroups)
size <- nrow(honeyPerms)
honeyPerms <- as.data.frame(t(honeyPerms))
honeyP <- cbind(honey, honeyPerms)
for(k in 1:size){
  permDist[k] <- anova(aov(
    honeyP$Surplus ~ honeyP[, k + 2]
  ))$'F value'[1]
}
obs.F <- anova(model1)$'F value'[1]
pvalue <- mean(permDist >= obs.F)

## Effect Sizes
eta2 <- sjstats::eta_sq(model2)
w2 <- sjstats::omega_sq(model2)
eps2 <- sjstats::epsilon_sq(model2)