# Random Effects with Loom/Fabric Strength Situation

# Load Packages
packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "parameters",
              "DescTools", "ggplot2", "lattice",
              "psych", "lme4")
lapply(packages, library, character.only = TRUE)

# Import the Data
path <- url(
  "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/onewayExplorations.Rdata"
)
load(path)
close(path)

# Data are in endCaps data frame
# Renaming design values to add clarity
fabric$loom <- dplyr::recode_factor(fabric$loom,
                                    `1` = "Loom 1",
                                    `2` = "Loom 2",
                                    `3` = "Loom 3",
                                    `4` = "Loom 4")

# Hasse Diagram
loomLabels <- c("1 Grand Mean 1", "4 (Loom) 3", "16 (Squares/Error) 12")
loomMat <- matrix(data = F, nrow = 3, ncol = 3)
loomMat[1, c(2:3)] = loomMat[2, 3] = T
hasseDiagram::hasse(loomMat, loomLabels)

# Fit Models
# Fit the Fixed Effects Model
options("contrasts" = c("contr.sum","contr.poly"))
loomFEModel <- aov(strength ~ loom, data = fabric)

# Fit the Random Effects Model
# Don't forget to [install and] load the lme4 package
loomREModel <- lme4::lmer(
  strength ~ (1|loom),
  data = fabric)


# Check Assumptions
# We now have two QQ Plots to check (residuals and Treatments)
# Use the Random Effects Model
a <- car::qqPlot(
  x = resid(loomREModel),
  distribution = "norm",
  envelope = 0.92,
  ylab = "Strength",
  pch = 20,
  main = "Residuals"
)
b <- car::qqPlot(
  x = lme4::ranef(loomREModel)$loom[, "(Intercept)"],
  distribution = "norm",
  envelope = 0.92,
  ylab = "Strength",
  pch = 20,
  main = "Random Effects"
)

# Homoscedasticity MUST be checked through the
# Tukey-Anscombe Plot (Residual vs. fit)
plot(loomREModel)


# Look at Omnibus Test
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    loomFEModel, omega_squared = "raw",
    eta_squared = "raw", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Omega Sq.", "Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for Fabric Strength Study",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

# Look at Random Effects Model
summary(loomREModel)

# Comparing Treatment (Fixed) and Conditional (Random) Means
## Fixed Effects
dummy.coef(loomFEModel)

## Random Effects
lme4::fixef(loomREModel)
lme4::ranef(loomREModel)

# Compare Confidence Intervals
## Fixed Effects
confint(loomFEModel, level = 0.975, oldNames = FALSE)

## Random Effects
confint(loomREModel, level = 0.975, oldNames = FALSE)

# Example Code for a nice looking table
intervals <-confint(loomREModel, level = 0.975, oldNames = FALSE)
row.names(intervals) <- c("Treatment Standard Deviation",
                          "Residual Standard Deviation",
                          "Grand Mean")
knitr::kable(
  intervals,
  digits = 3,
  caption = "Upper and Lower Confidence Bounds-90% SCI, Bonferroni Adj.",
  align = c('l',rep('c',2))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

