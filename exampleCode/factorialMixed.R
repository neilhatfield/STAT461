# Three-way Mixed Effects Example

# Packages
packages <- c("tidyverse", "hasseDiagram", "lme4", "emmeans")
lapply(packages, library, character.only = TRUE)

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Set Global Options
options("contrasts" = c("contr.sum","contr.poly"))


# Set up data frame
pressure <- data.frame(
  gasTemp = as.factor(rep(c(rep(60, 4), rep(75, 4), rep(90, 4)), 6)),
  gauge = sort(rep(letters[24:26], 24)),
  operator = rep(LETTERS[1:4], 18),
  coded = c(
    -2, 0, -1, 4, 14, 6, 1, -7, -8, -2, -1, -2,
    -3, -9, -8, 4, 14, 0, 2, 6, -8, 20, -2, 1,
    -6, -5, -8, -3, 22, 8, 6, -5, -8, 1, -9, -8,
    4, -1, -2, -7, 24, 6, 2, 2, 3, -7, -8, 3,
    -1, -4, 0, -2, 20, 2, 3, -5, -2, -1, -4, 1,
    -2, -8, -7, 4, 16, 0, 0, -1, -1, -2, -7, 3
  )
)

# Create Hasse Diagram
pressureLab <- c("1 Grand Mean 1", "3 Gas Temp 2",
               "4 (Operator) 3", "3 (Gauge) 2",
               "12 (Temp X Operator) 6", "9 (Temp X Gauge) 4",
               "12 (Operator X Gauge) 6", "36 (Temp X Operator X Gauge) 12",
               "72 (Measurements/Error) 36")
pressureMat <- matrix(data = F, nrow = 9, ncol = 9)
pressureMat[1, c(2:9)] = pressureMat[2, c(5, 6, 8, 9)] = T
pressureMat[3, c(5, 7, 8, 9)] = pressureMat[4, c(6, 7, 8, 9)] = T
pressureMat[c(5:7), c(8:9)] = pressureMat[8, 9] = T
hasseDiagram::hasse(pressureMat, pressureLab)

# Fit Models
## Fixed Effect for Omnibus Tests
pressureFE <- aov(coded ~ gasTemp*operator*gauge, data = pressure)

## Random Effects for Estimation
## Can also be used for Omnibus Test via Conf. Intervals

pressureRE <- lme4::lmer(
  coded ~ gasTemp + (1|operator) + (1|gauge) +
    (1|gasTemp:operator) + (1|gasTemp:gauge) + (1|operator:gauge) +
    (1|gasTemp:operator:gauge),
  data = pressure,
  REML = TRUE)

# Interaction Plots
emmeans::emmip(pressureFE, gasTemp ~ operator|gauge)
emmeans::emmip(pressureFE, gasTemp:operator ~ gasTemp)
emmeans::emmip(pressureFE, gasTemp:gauge ~ gasTemp)


# Omnibus Table--Warning
## Helper Function from my ANOVATools.R File--Warning
## Warning: anovaFixer is not currently set up for 3-way+
#fixedOmni <- anovaFixer(sysFE, fixed = NULL, random = c("partNum", "operator"))

## Table
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    pressureFE, omega_squared = "partial",
    eta_squared = "partial", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Partial Omega Sq.", "Partial Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Three-way Mixed Effect-Table is NOT correct",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

# Alternate?
afex::mixed(coded ~ gasTemp + (1|operator) + (1|gauge) +
              (1|gasTemp:operator) + (1|gasTemp:gauge) + (1|operator:gauge) +
              (1|gasTemp:operator:gauge),
            data = pressure,
            method = "S",
            control = lme4::lmerControl(optCtrl = list(maxfun = 1e6)))