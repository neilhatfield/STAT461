# ANCOVA Example--Keyboarding

# Packages
packages <- c("tidyverse", "hasseDiagram", "ggplot2",
              "rstatix", "car", "emmeans")
lapply(packages, library, character.only = TRUE)

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Set Global Options
options("contrasts" = c("contr.sum","contr.poly"))

# Hasse Diagram
kbLab <- c("1 Grand Mean 1", "3 Keyboard 2", "Cov: Usage Time 1",
           "12 (Subjects/Error) 8")
kbMat <- matrix(data = F, nrow = 4, ncol = 4)
kbMat[1, c(2:4)] = kbMat[2, c(3:4)] = T
kbMat[3, 4] = T
hasseDiagram::hasse(kbMat, kbLab)

# Data File
keyboarding <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/keyboarding.dat",
  header = TRUE, sep = ""
)
keyboarding$kbd.type <- as.factor(keyboarding$kbd.type)

# Explore the data--On your own
## Data Visualizations
boxplot(hrs.kbd ~ kbd.type, data = keyboarding)
## Descriptive Statistics

# First Assumption Check-Linearity of Covariate Relationship
ggplot2::ggplot(data = keyboarding,
                mapping = ggplot2::aes(
                  y = hrs.pain,
                  x = hrs.kbd,
                  group = kbd.type,
                  color = kbd.type
                )) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_bw() +
  xlab("Hours Spent Keyboarding") +
  ylab("Hours of Pain") +
  labs(color = "Keyboard Type")

# Second Assumption Check-Potential Outliers
## Univariate--On Your Own

## Multivariate-Mahalanobis Distances
key2 <- rstatix::mahalanobis_distance(keyboarding)
### Optional-Reattach keyboarding type
key2 <- cbind(key2, factor = keyboarding$kbd.type)
### Graph
ggplot2::ggplot(data = key2,
                mapping = ggplot2::aes(
                  y = hrs.pain,
                  x = hrs.kbd,
                  shape = is.outlier,
                  color = factor
                )) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_bw() +
  xlab("Hours Spent Keyboarding") +
  ylab("Hours of Pain") +
  labs(color = "Keyboard", shape = "Potential Outlier")

# Fit the model
kbModel <- aov(hrs.pain ~ kbd.type + hrs.kbd, data = keyboarding)
intCheck <- aov(hrs.pain ~ kbd.type*hrs.kbd, data = keyboarding)

# Check Assumptions
## Normality of Residuals--On Your Own
## Homoscedasticity--On Your Own
## Independence of Observations--On Your Own
## Linearity-Already Checked (see above)
## Potential Outliers--Already Checked (see above)

## Homogeneity
### Raw Output
car::Anova(intCheck, type = "III")
### Additional Visualization
ggplot2::ggplot(data = keyboarding,
                mapping = ggplot2::aes(
                  y = hrs.pain,
                  x = hrs.kbd,
                  group = kbd.type,
                  color = kbd.type
                )) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_smooth(method = "lm", se = FALSE) +
  ggplot2::theme_bw() +
  xlab("Hours Spent Keyboarding") +
  ylab("Hours of Pain") +
  labs(color = "Keyboard Type")

# Omnibus Test
## Raw Output
car::Anova(kbModel, type = "III")

# Post Hoc
emmeans::emmeans(kbModel, pairwise ~ kbd.type,
                 adjust = "tukey",
                 level = 0.9)