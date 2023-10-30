# Load Packages and Set Options ----
packages <- c("tidyverse", "car", "psych", "parameters",
              "emmeans", "knitr", "kableExtra")
invisible(
  lapply(
    X = packages,
    FUN = library,
    character.only = TRUE,
    quietly = TRUE
  )
)
source("https://raw.github.com/neilhatfield/STAT461/main/rScripts/ANOVATools.R")
options(knitr.kable.NA = "")
options(contrasts = c("contr.sum", "contr.poly"))

# Load Data ----
airplaneData <- read.table(
  file = "https://raw.githubusercontent.com/neilhatfield/STAT461/main/dataFiles/Airplanes_Fall23.csv",
  header = TRUE,
  sep = ","
)

## Wrangle Data ----
## Data needs to be in long format with thrower and design as factors
airplaneLong <- airplaneData %>%
  pivot_longer(
    cols = !Thrower,
    names_to = "design",
    names_transform = list(design = as.factor),
    values_to = "distance"
  )

airplaneLong$Thrower <- as.factor(airplaneLong$Thrower)


# Fit ANOVA Model Object ----
airplaneModel <- aov(
  formula = distance ~ design,
  data = airplaneLong
)

# Assumption Checking ----
## Gaussian Residuals ----
qqPlot(
  x = residuals(airplaneModel),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (inches)"
)

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(airplaneModel),
    fitted = fitted(airplaneModel)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
   geom_point(size = 2) +
  theme_bw() +
  labs(
    x = "Fitted values (inches)",
    y = "Residuals (inches)"
  )

# Modern ANOVA Table ----
parameters::model_parameters(
  model = airplaneModel,
  effectsize_type = c("eta", "omega", "epsilon")
) %>%
  mutate(
    p = ifelse(
      test = is.na(p),
      yes = NA,
      no = pvalRound(p, digits = 4)
    )
  ) %>%
  kable(
    digits = 4,
    format.args = list(big.mark = ","),
    col.names = c(
      "Source", "SS", "df", "MS", "F", "p-value",
      "Eta Sq.", "Omega Sq.", "Epsilon Sq."
    ),
    caption = "Paper Airplane Study (Fall '23)",
    booktabs = TRUE,
    align = c("l", rep("c", 8)),
  ) %>%
  kable_classic()

# Post Hoc Analysis ----
## Comparisons ----
planePairs <- emmeans(
  object = airplaneModel,
  specs = pairwise ~ design,
  adjust = "sidak",
  level = 0.87
)

as.data.frame(planePairs$contrasts) %>%
  mutate(
    p.value = ifelse(
      test = is.na(p.value),
      yes = NA,
      no = pvalRound(p.value, digits = 3)
    )
  ) %>%
  kable(
    digits = 3,
    caption = "Post Hoc Pairwise Comparisons with Sidak Correction",
    col.names = c("Pair", "Difference", "SE", "DF", "t", "p-value"),
    booktabs = TRUE,
    align = c("l", rep("c", 5))
  ) %>%
  kable_classic()

## Effect Sizes ----
anova.PostHoc(airplaneModel) %>%
  kable(
    digits = 3,
    caption = "Post Hoc Pairwise Comparison Effect Sizes",
    col.names = c("Pair", "Cohen's d", "Hedge's g", "Prob. Superiority"),
    align = "lccc",
    booktabs = TRUE
  ) %>%
  kable_classic()


# Log Transformation ----
airplaneLong$sqrtDist <- sqrt(airplaneLong$distance)

airplaneModel3 <- aov(
  formula = sqrtDist ~ design,
  data = airplaneLong
)

# Check Assumptions ---- 
## Gaussian Residuals ----
qqPlot(
  x = residuals(airplaneModel3),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (sq. root inches)"
)

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(airplaneModel3),
    fitted = fitted(airplaneModel3)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  theme_bw() +
  labs(
    x = "Fitted values (sq. root inches)",
    y = "Residuals (sq. root inches)"
  )

dummy.coef(airplaneModel)


# Impact of Block ----
ggplot(
  data = airplaneLong,
  mapping = aes(x = design, y = distance)
) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Design",
    y = "Distance flown (in)"
  ) +
  facet_wrap(facets = vars(Thrower))
