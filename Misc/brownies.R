# Brownie Mix Study
options(contrasts = c("contr.sum", "contr.poly"))
# Load Packages ----
library(tidyverse)
library(googlesheets4)
library(lme4)
library(rstatix)
library(blme)

# Load Data ----
googlesheets4::gs4_deauth()
tasteDataWide <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1INycYSmi3nvC8-kUZCWLEs7HjEo6e5GFwbLfI-_GqgM/edit?usp=sharing",
  col_names = TRUE
)

## Make a long version of the data
tasteDataLong <- tasteDataWide %>%
  pivot_longer(
    cols = starts_with("Mix"),
    names_to = "Mix",
    names_transform = list(Mix = as.factor),
    values_to = "score"
  ) %>%
  rowwise() %>%
  mutate(
    Order = str_locate(
      string = Order,
      pattern = gsub(
        pattern = "Mix ",
        replacement = "",
        x = Mix)
    )[1]
  )

# Data Exploration ----
## Univariate Plots ----
### Histogram of Scores ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = score)
) +
  geom_histogram(
    color = "black",
    fill = boastUtils::psuPalette[1],
    bins = 9,
    boundary = 0,
    closed = "left"
  ) +
  theme_bw() +
  scale_y_continuous(
    expand = expansion(mult = 0, add = c(0, 2))
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  theme(
    text = element_text(size = 18)
  )

### Shadowgram of Scores ----
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/shadowgram.R")
shadowgram(
  dataVec = tasteDataLong$score,
  label = "Hedonic Score",
  color = "darkblue"
) +
  theme(
    text = element_text(size = 18)
  )

## Bivariate Plots ----
### Box plot of Score by Mix ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = Mix, y = score)
) +
  geom_boxplot() +
  theme_bw() +
  theme(
    text = element_text(size = 18)
  )

### Box plot of Score by Taster ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = Taster, y = score)
) +
  geom_boxplot() +
  theme_bw() +
  theme(
    text = element_text(size = 18)
  ) +
  scale_x_discrete(
    labels = label_wrap_gen(width = 10),
    guide = guide_axis(angle = 45)
  )

## Multivariate Plots ----
### Heat Map ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = Taster, y = Mix, weight = score)
) +
  geom_bin_2d() +
  theme_bw() +
  scale_x_discrete(
    labels = label_wrap_gen(width = 10),
    guide = guide_axis(angle = 45)
  ) +
  scale_fill_gradient2(name = "Score") +
  theme(
    text = element_text(size = 18)
  )

### Alluvial Plot ----
# EXPERIMENTAL
library(ggalluvial)
ggplot(
  data = tasteDataLong,
  mapping = aes(y = score, axis1 = Mix, axis2 = Taster, axis3 = score)
) +
  geom_alluvium(aes(fill = Mix)) +
  geom_stratum(fill = "white", color = "gray") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  scale_x_discrete(
    limits = c("Mix", "Taster", "Score"),
    expand = expansion()
    ) +
  theme(
    text = element_text(size = 18),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

# Fit Models ----
## Omnibus Model ----
brownieOmni <- aov(
  formula = score ~ Taster + Mix,
  data = tasteDataLong
)

## Random Effects (Assumption Checking) ----
brownieMixed <- lme4::lmer(
  formula = score ~ (1|Taster) + Mix,
  data = tasteDataLong
)

### Due to singularity, using a Bayesian approach for estimation
bayesMixed <- blme::blmer(
  formula = score ~ (1|Taster) + Mix,
  data = tasteDataLong,
  cov.prior = wishart
)

## Model for checking Spherecity ----
brownieSphere <- rstatix::anova_test(
  data = tasteDataLong,
  formula = score ~ Mix + Error(Taster %in% Mix)
)

## Informal Interaction Check
# Note: you can't fit an interaction model to do an informal
# test of the interaction term like we can in ANCOVA as
# we run out of Degrees of Freedom and can't estimate terms

# Assumption Checking -----
## Gaussian Residuals ----
car::qqPlot(
  x = residuals(bayesMixed), ## Notice which model is getting used
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
      object = bayesMixed, ## Notice which model is getting used
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
    residuals = residuals(bayesMixed), ## Notice which model is getting used
    fitted = fitted.values(bayesMixed)
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
sec2 <- c("Mike", "One", "SpaceJam", "Tester", "silly-hedgehog-69",
          "Gordon Ramsay", "CoconutHead", "The terminator (Joseph)", "Jisoo Kim")
sec3 <- c("Kenny", "Bob", "Taylor", "Jenn", "BEARBERRY", "Karen", "Todd Curtis",
          "Sara", "Antonio Brown")
tasteDataLong %>%
  mutate(
    section = case_when(
      Taster %in% sec2 ~ "Sec. 2",
      Taster %in% sec3 ~ "Sec. 3",
      .default = "Error"
    )
  ) %>%
  ggplot(
    mapping = aes(x = Mix, y = score, group = Taster, color = Taster)
  ) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(facets = vars(section)) +
  theme(
    legend.position = "none"
  )

## Sphericity ----
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")
sphericityPlot(
  dataWide = tasteDataWide, # Data needs to be in wide format
  colsIgnore = c("Order"),
  subjectID = "Taster",
  colors = "default"
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

# Interference Checks ----
cbind(
  tasteDataLong,
  residuals = residuals(bayesMixed) ## Notice which model is getting used
) %>%
  ggplot(
    mapping = aes(x = Order, y = residuals)
  ) +
  geom_point(
    # mapping = aes(shape = Mix, color = Mix),
    size = 5
  ) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  theme_bw() +
  xlab("Tasting Order") +
  ylab("Residuals (score)") +
  facet_wrap(facets = vars(Taster)) +
  scale_color_manual(values = boastUtils::psuPalette[c(1:4,6)]) +
  theme(text = element_text(size = 18))
