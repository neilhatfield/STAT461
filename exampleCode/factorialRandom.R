# Two-way Random Effects Example

# Packages
packages <- c("tidyverse", "hasseDiagram", "lme4", "emmeans", "parameters")
lapply(packages, library, character.only = TRUE)

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Set Global Options
options("contrasts" = c("contr.sum","contr.poly"))

# Set up data frame
mSystem <- data.frame(
  partNum = as.factor(rep(c(1:20), 6)),
  operator = sort(rep(LETTERS[1:3], 40)),
  measure = c(
  21, 24, 20, 27, 19, 23, 22, 19, 24, 25, 21, 18, 23, 24, 29, 26, 20, 19, 25, 19,
  20, 23, 21, 27, 18, 21, 21, 17, 23, 23, 20, 19, 25, 24, 30, 26, 20, 21, 26, 19,
  20, 24, 19, 28, 19, 24, 22, 18, 25, 26, 20, 17, 25, 23, 30, 25, 19, 19, 25, 18,
  20, 24, 21, 26, 18, 21, 24, 20, 23, 25, 20, 19, 25, 25, 28, 26, 20, 19, 24, 17,
  19, 23, 20, 27, 18, 23, 22, 19, 24, 24, 21, 18, 25, 24, 31, 25, 20, 21, 25, 19,
  21, 24, 22, 28, 21, 22, 20, 18, 24, 25, 20, 19, 25, 25, 30, 27, 20, 23, 25, 17
  )
)

# Create Hasse Diagram
sysLabels <- c("1 Grand Mean 1", "3 (Operator) 2",
               "20 (Part) 19", " 60 (Operator X Part) 38",
               "120 (Readings/Error) 60")
sysMat <- matrix(data = F, nrow = 5, ncol = 5)
sysMat[1, c(2:5)] = sysMat[c(2:3), c(4:5)] = sysMat[4, 5] = T
hasseDiagram::hasse(sysMat, sysLabels)

# Fit Models
## Fixed Effect for Omnibus Tests
sysFE <- aov(measure ~ partNum*operator, data = mSystem)

## Random Effects for Estimation
## Can also be used for Omnibus Test via Conf. Intervals

## Automated Expansion (A*B) is not available in lmer
sysRE <- lme4::lmer(
  measure ~ (1|operator) + (1|partNum) + (1|operator:partNum),
  data = mSystem,
  REML = TRUE)

# Interaction Plots
emmeans::emmip(sysFE, operator ~ partNum)

# Omnibus Table

## Table
options(knitr.kable.NA= "")
knitr::kable(
  fixedOmni,
  digits = 3,
  col.names = c("Df", "SS", "MS", "F", "p-value"),
  caption = "ANOVA Table for a Two-way Random",
  align = c('l',rep('c',5))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

## New Helper Function from my ANOVATools.R File
fixedOmni <- anovaFixer(sysFE, fixed = NULL,
                        random = c("partNum", "operator"),
                        type = "unrestricted")


# Random Effects Summary
summary(sysRE)
confint(sysRE, level = 0.9, oldNames = FALSE)


# Because of the sigularity issues (Var Interaction is 0)
# we should fit a reduced model.
reducedFE <- aov(measure ~ partNum + operator, data = mSystem)

reducedRE <- lme4::lmer(
  measure ~ (1|operator) + (1|partNum),
  data = mSystem,
  REML = TRUE)

anova(reducedFE)

summary(reducedRE)
confint(reducedRE, level = 0.9, oldNames = FALSE)