# Load Packages
packages <- c("tidyverse", "hasseDiagram", "car",
              "knitr", "kableExtra", "parameters",
              "multicool", "ggplot2")
lapply(packages, library, character.only = TRUE)

# Set Options
options(contrasts = c("contr.sum","contr.poly"))
options(knitr.kable.NA= "")

# Get Data
toys <- data.frame(
  practice = c(rep("Null", 3), rep("SPC", 3), rep("SPC+EPC", 3)),
  reduction = c(1.1, 0.5, -2.1, 4.2, 3.7, 0.8, 3.2, 2.8, 6.3)
)
View(toys)
str(toys)

# Build Hasse Diagram
toysLabel <- c("1 Grand Mean 1", "3 QC Process 2", "9 (Plant/Error) 6")
toysMat <- matrix(data = F, nrow = 3, ncol = 3)
toysMat[1, c(2:3)] = toysMat[2, 3] = TRUE
hasseDiagram::hasse(data = toysMat, labels = toysLabel)

# Run ANOVA model to get residuals
toyModel <- aov(reduction ~ practice, data = toys, na.action = "na.omit")

# QQ Plot on Residuals
a <- car::qqPlot(
  toyModel$residuals,
  distribution = "norm",
  envelope = 0.9,
  pch = 20,
  ylab = "Residual Defect Rate Deduction"
)

# Stripchart/scatterplot for Homoscedasticity
stripchart(
  reduction ~ practice,
  data = toys,
  vertical = TRUE,
  pch = 20,
  ylab = "Defect Rate Deduction"
)

# Make a nice, modern ANOVA Table
knitr::kable(
  parameters::model_parameters(
    toyModel,
    omega_squared = "raw",
    eta_squared = "raw",
    epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Omega Sq.", "Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for Toy Manufacturing Experiment",
  align = 'cccccccc'
) %>%
kableExtra::kable_styling(
  bootstrap_options = c("condensed", "boardered"),
  font_size = 12, latex_options = "HOLD_position")

# Make a nice table for GSAM and Treatment Effects
toyMainEffect <- data.frame(
  effects = c("Grand Mean", "Null", "SPC", "SPC+EPC"),
  value = c(dummy.coef(toyModel)$`(Intercept`[1],
            dummy.coef(toyModel)$practice[1],
            dummy.coef(toyModel)$practice[2],
            dummy.coef(toyModel)$practice[3])
)
## Set row names to empty
row.names(toyMainEffect) <- NULL

knitr::kable(
  toyMainEffect,
  digits = 3,
  col.names = c("Term", "Effect (Defect Rate Deduction/Plant)"),
  caption = "GSAM and Main (Treatment) Effects for Toy Manufacturing",
  align = 'lc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")


# Permutation Simulation
## Create the Permutations of Treatments
toyGroups <- multicool::initMC(toys$practice)
toyPerms <- multicool::allPerm(toyGroups)

## Get the number of permutations
size <- nrow(toyPerms)

## Set up a data frame to get F ratio values
### This orients the permuations the right way and helps R
### know how to join permutations
toyPerms <- as.data.frame(t(toyPerms))

### Joins the original data frame with our permutations
toyP <- cbind(toys, toyPerms)

## Build the Sampling Distribution for the F Ratio
permDist <- rep(NA, size)
for(k in 1:size){
  permDist[k] <- anova(aov(
    toyP$reduction ~ toyP[, k + 2]
  ))$'F value'[1]
}
obs.F <- anova(toyModel)$'F value'[1]

## Visualize the results of the permutation simulation
hist(permDist,
     main = "Histogram of Toys Permutation Simulation",
     xlab = "Simulated Value of F Ratio")
### Adds the observed values as a reference line
abline(v=obs.F, col="blue", lwd = 2)

## Calculate the p-value
pvalue <- mean(permDist >= obs.F)