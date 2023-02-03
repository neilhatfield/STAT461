# Within-Subjects Repeated Measures Example
# State College Beer Ratings

# Packages
packages <- c("tidyverse", "hasseDiagram", "lme4",
              "car", "ggplot2", "viridis",
              "knitr", "kableExtra", "parameters",
              "emmeans")
lapply(packages, library, character.only = TRUE)

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Set Global Options
options("contrasts" = c("contr.sum","contr.poly"))

# Hasse Diagram
beerLabs <- c("1 Grand Mean 1", "4 Beer 3",
              "6 (Judge) 5", "24 (Error) 15")
beerMat <- matrix(data = F, nrow = 4, ncol = 4)
beerMat[1, c(2:4)] = beerMat[c(2:3), 4] = T
hasseDiagram::hasse(beerMat, beerLabs)

beer <- data.frame(
  judge = sort(rep(LETTERS[1:6],4)),
  beer = rep(c("Barnstormer", "King Richard Red",
               "Craftsman", "Red Mo"), 6),
  score = c(50, 60, 70, 70,
            38, 45, 58, 60,
            45, 48, 60, 58,
            65, 65, 75, 75,
            55, 60, 70, 65,
            48, 53, 68, 63)
)
# Please note I've made up these data as I could not get a hold of actual scores

# Explore the Data
## Data Visualizations
## Descriptive Statistics

# Fit models
beerM1 <- lme4::lmer(score ~ beer + (1|judge), data = beer)
beerM2 <- aov(score ~ beer + Error(judge/beer), data = beer)

# Check Assumptions
## Normality
### Residuals
a <- car::qqPlot(
  x = residuals(beerM1),
  distribution = "norm",
  envelope = 0.9,
  ylab = "Score",
  pch = 19
)

### Random Effect
b <- car::qqPlot(
  x = lme4::ranef(beerM1)$judge[, "(Intercept)"],
  distribution = "norm",
  envelope = 0.9,
  ylab = "score",
  pch = 20
)

## Homoscedasticity on Residuals
plot(beerM1, which = 1, pch = 19)

## Interaction
ggplot2::ggplot(data = beer,
                mapping = aes(x = beer,
                              y = score,
                              color = judge,
                              group = judge)) +
  ggplot2::geom_point(size=2) +
  ggplot2::geom_line(size=1) +
  ggplot2::theme_bw() +
  viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
  xlab("Beer") +
  ylab("Score") +
  labs(color = "Judge")

# Omnibus Test
## Method 1-The aov call
### Raw Output Table Object
summary(beerM2)

### Modern Table--Not the most organized looking table
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    beerM2, omega_squared = "partial",
    eta_squared = "partial", epsilon_squared = "partial"),
  digits = 3,
  col.names = c("Term Group", "Source", "SS", "df", "MS", "F", "p-value",
                "Partial Omega Sq.", "Partial Eta Sq.", "Partial Epsilon Sq."),
  caption = "ANOVA Table Within-Subjects: Beer Scores",
  align = c(rep('l',2),rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

## Method 2-The Multivariate Approach
### Rearrange the Data
beerW <- tidyr::pivot_wider(beer,id_cols = judge,
                         names_from = beer,
                         values_from = score)

### Create a matrix of just the responses from the wide format
scoresW <- as.matrix(beerW[ , 2:5]) # LOOK AT DATA FOR THESE NUMBERS

### Fit the model
beerM3 <- lm(scoresW ~ 1)

### Create the Design--What is the Repeated Factor?
rfactor <- factor(levels(beer$beer))

### Create the table object
tabBeer <- car::Anova(beerM3, idata = data.frame(rfactor),
                   idesign = ~rfactor, type = "III")

### Raw Output
outRM <- summary(tabBeer, multivariate=FALSE)

### Nice Tables--There are three tables
#### Omnibus
options(knitr.kable.NA= "")
knitr::kable(
  data.frame(unclass(outRM$univariate.tests)),
  digits = 3,
  col.names = c("Term SS", "Numerator DF", "Error SS", "Denominator DF", "F", "p-value"),
  caption = "ANOVA Table Within-Subjects: Beer Scores",
  align = rep('c',6)
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position") %>%
  kableExtra::footnote(
    general = "Computer rounding has made the p-values look like zero.",
    footnote_as_chunk = T)

#### Mauchly Test for Sphericity
options(knitr.kable.NA= "")
knitr::kable(
  data.frame(unclass(outRM$sphericity.tests)),
  digits = 3,
  col.names = c("Test Statistic", "p-value"),
  caption = "Mauchly Test for Sphericity",
  align = rep('c',2)
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

#### Adjustments if Sphericity is Violated
options(knitr.kable.NA= "")
knitr::kable(
  data.frame(unclass(outRM$pval.adjustments)),
  digits = 3,
  col.names = c("Greenhouse-Geiser", "p-value", "Huynh-Feldt", "p-value"),
  caption = "Adjustments for Sphericity Violations",
  align = rep('c',4)
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position") %>%
  kableExtra::footnote(
    general = "Computer rounding has made the p-values look like zero.",
    footnote_as_chunk = T)

# Post Hoc
postBeer<- emmeans::emmeans(beerM1, pairwise ~ beer,
                              adjust = "tukey",
                              level = 0.90)
## Treatment Means
### Get Values
postBeerMeans <- postBeer$emmeans
### Make a Nice Table
options(knitr.kable.NA= "")
knitr::kable(
  postBeerMeans,
  digits = 3,
  col.names = c("Beer", "Estimate", "SE", "DF", "Lower Bound", "Upper Bound"),
  caption = "Beer Score Means-Sidak's Adjustment 95\\%",
  align = c('l',rep('c',5))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

## Pairwise Differences
### Get Values
postBeerPairs <- confint(postBeer, level = 0.90)$contrast
### Make Nice Table
options(knitr.kable.NA= "")
knitr::kable(
  postBeerPairs,
  digits = 3,
  col.names = c("Pair", "Estimate", "SE", "DF", "Lower Bound", "Upper Bound"),
  caption = "Pairwise Differences-Tukey Method 95\\%",
  align = c('l',rep('c',5))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")