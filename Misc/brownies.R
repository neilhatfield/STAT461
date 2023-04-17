# Brownie Mix Study
options(contrasts = c("contr.sum", "contr.poly"))
# Load Packages ----
library(tidyverse)
library(googlesheets4)
library(lme4)
library(rstatix)

# Load Data ----
googlesheets4::gs4_deauth()
tasteDataWide <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1INycYSmi3nvC8-kUZCWLEs7HjEo6e5GFwbLfI-_GqgM/edit?usp=sharing",
  col_names = TRUE
)

tasteSec2 <- tasteDataWide[10:18, ]
tasteSec3 <- tasteDataWide[1, ]

## Make a long version of the data
tasteDataLong <- tasteDataWide %>%
  pivot_longer(
    cols = starts_with("Mix"),
    names_to = "Mix",
    names_transform = list(Mix = as.factor),
    values_to = "score"
  )
tasteDataLong$adjScore <- tasteDataLong$score + rnorm(nrow(tasteDataLong), mean = 0, sd = 0.25)

taste2Long <- tasteSec2 %>%
  pivot_longer(
    cols = starts_with("Mix"),
    names_to = "Mix",
    names_transform = list(Mix = as.factor),
    values_to = "score"
  )

taste3Long <- tasteSec3 %>%
  pivot_longer(
    cols = starts_with("Mix"),
    names_to = "Mix",
    names_transform = list(Mix = as.factor),
    values_to = "score"
  )

# Fit Models ----
## Omnibus Model ----
brownieOmni <- aov(
  formula = score ~ Taster + Mix,
  data = tasteDataLong
)

## Random Effects ----
brownieMixed <- lme4::lmer(
  formula = score ~ (1|Taster) + Mix,
  data = tasteDataLong
)
### Due to singularity, using a Bayesian approach for estimation
bayesianMixed <- rstanarm::stan_glmer(
  formula = score ~ (1|Taster) + Mix,
  data = tasteDataLong
)

## Spherecity ----
brownieSphere <- rstatix::anova_test(
  data = tasteDataLong,
  formula = score ~ Mix + Error(Taster %in% Mix)
)

# Assumption Checking -----
## Gaussian Residuals ----
car::qqPlot(
  x = residuals(brownieMixed),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (score)"
)

## Gaussian Taster Effect ----
car::qqPlot(
  x = unlist( 
    lme4::ranef(
      object = bayesianMixed, ## Notice which model is getting used
      whichel = c("Taster")
    )
  ),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Taster Effects"
)

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(brownieMixed), # Notice which model
    fitted = fitted.values(brownieMixed)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_smooth(
    formula = y ~ x,
    method = stats::loess,
    method.args = list(degree = 1),
    se = FALSE,
    linewidth = 0.5
  ) +
  theme_bw() +
  xlab("Fitted values (score)") +
  ylab("Residuals (score)")

## Interaction ----
tasteDataLong %>%
  mutate(
    tempGroup = case_when(
      Taster %in% c("Kenny", "The terminator (Joseph)") ~ "G1",
      Taster %in% c("Karen", "SpaceJam") ~ "G2",
      Taster %in% c("Todd Curtis", "Jisoo Kim") ~ "G3",
      Taster %in% c("BEARBERRY", "Gordon Ramsay") ~ "G4",
      Taster %in% c("Sara", "CoconutHead") ~ "G5",
      Taster %in% c("Antonio Brown", "One") ~ "G6",
      Taster %in% c("Bob", "silly-hedgehog-69") ~ "G7",
      Taster %in% c("Jenn", "Mike") ~ "G8",
      Taster %in% c("Taylor", "Tester") ~ "G9",
    )
  ) %>%
ggplot(
  mapping = aes(
    x = Taster,
    y = score,
    color = Mix,
    group = Mix
  )
) +
  geom_point(size = 2) +
  geom_line() +
  ggplot2::theme_bw() +
  xlab("Taster") +
  ylab("Score") +
  labs(
    color = "Mix"
  ) +
  scale_color_manual(values = boastUtils::boastPalette) +
  facet_wrap(
    facets = vars(tempGroup),
    nrow = 3,
    ncol = 3,
    scales = "free_x"
  )


## Sphericity ----
sphericityPlot(
  dataWide = tasteDataWide, # Data needs to be in wide format
  subjectID = "Taster", 
  colors = "deafult", 
)

brownieSphere$`Mauchly's Test for Sphericity` %>%
  dplyr::select(Effect, W, p) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Effect", "Mauchly's W", "p"),
    caption = "Mauchly's Sphericity Test",
    align = c('l',"c","c"),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )
