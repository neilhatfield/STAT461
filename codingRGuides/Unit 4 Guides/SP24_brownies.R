# Brownie Study
options(contrasts = c("contr.sum", "contr.poly"))
options(knitr.kable.NA = "")
# Load Packages ----
library(tidyverse)
library(googlesheets4)
library(lme4)
library(rstatix)
library(blme)
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

# Load Data ----
googlesheets4::gs4_deauth()
tasteDataWide <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1UGnlzaJ312BuqLUShNF_UGwn7Ys3v_gzx4ouY3a2ZCM/edit?usp=sharing",
  col_names = TRUE
)

# Clean data ----
## Add a taster id column ----
tasteDataWide <- tasteDataWide %>%
  mutate(
    taster = paste("Student", row_number()),
    .after = Section
  )

## Set section and taster as factors ----
tasteDataWide$Section <- as.factor(tasteDataWide$Section)
tasteDataWide$taster <- as.factor(tasteDataWide$taster)

## Make a long version of the data ----
tasteDataLong <- tasteDataWide %>%
  pivot_longer(
    cols = starts_with("Brownie"),
    names_to = "recipe",
    names_transform = list(recipe = as.factor),
    values_to = "score"
  ) %>%
  rowwise() %>%
  mutate(
    tasteSeq = gsub(
      pattern = ",* *",
      replacement = "",
      x = Order
    ),
    order = str_locate(
      string = tasteSeq,
      pattern = gsub(
        pattern = "Brownie_",
        replacement = "",
        x = recipe)
    )[1]
  ) %>%
  dplyr::select(!Order)

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
  mapping = aes(x = recipe, y = score)
) +
  geom_boxplot() +
  theme_bw() +
  theme(
    text = element_text(size = 18)
  )

### Box plot of Score by Taster ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = taster, y = score)
) +
  geom_boxplot() +
  theme_bw() +
  theme(
    text = element_text(size = 12)
  ) +
  scale_x_discrete(
    # labels = label_wrap_gen(width = 10),
    guide = guide_axis(angle = 45)
  )

## Multivariate Plots ----
### Heat Map ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = taster, y = recipe, weight = score)
) +
  geom_bin_2d() +
  theme_bw() +
  scale_x_discrete(
    # labels = label_wrap_gen(width = 10),
    guide = guide_axis(angle = 45)
  ) +
  scale_fill_gradient2(name = "recipe") +
  theme(
    text = element_text(size = 12)
  )

### Alluvial Plot ----
# EXPERIMENTAL
library(ggalluvial)
ggplot(
  data = tasteDataLong,
  mapping = aes(y = score, axis1 = recipe, axis2 = taster, axis3 = score)
) +
  geom_alluvium(aes(fill = recipe)) +
  geom_stratum(fill = "white", color = "gray") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  scale_x_discrete(
    limits = c("recipe", "taster", "score"),
    expand = expansion()
  ) +
  theme(
    text = element_text(size = 18),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

# Random Sample ----
set.seed(461)

tasterSubWide <- tasteDataWide %>%
  group_by(Section) %>%
  slice_sample(n = 5)

tasterSubLong <- tasteDataLong %>%
  filter(taster %in% tasterSubWide$taster)

# Fit Models ----
## Omnibus Model ----
brownieOmni <- aov(
  formula = score ~ taster + recipe,
  data = tasteDataLong
)

## Random Effects (Assumption Checking) ----
brownieMixed <- lme4::lmer(
  formula = score ~ (1|taster) + recipe,
  data = tasteDataLong
)

### Due to singularity, using a Bayesian approach for estimation
bayesMixed <- blme::blmer(
  formula = score ~ (1|taster) + recipe,
  data = tasteDataLong,
  cov.prior = wishart
)

library(nlme)
brownieMix2 <- nlme::lme(
  fixed = score ~ recipe,
  random = ~1|taster,
  data = tasteDataLong
)

## Model for checking Spherecity ----
brownieSphere <- rstatix::anova_test(
  data = tasteDataLong,
  formula = score ~ recipe + Error(taster %in% recipe)
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

car::qqPlot(
  x = residuals(brownieMixed), ## Notice which model is getting used
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
      whichel = c("taster")
    )
  ),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Subject Effects"
)

car::qqPlot(
  x = unlist(
    lme4::ranef(
      object = brownieMix2, ## Notice which model is getting used
      whichel = c("taster")
    )
  ),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Subject Effects"
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

ggplot(
  data = data.frame(
    residuals = residuals(brownieMix2), ## Notice which model is getting used
    fitted = fitted.values(brownieMix2)
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
  ggplot(
    mapping = aes(x = recipe, y = score, group = taster, color = taster)
  ) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  )

## Sphericity ----
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")
sphericityPlot(
  dataWide = tasteDataWide, # Data needs to be in wide format
  colsIgnore = c("Section", "Order", "Box_Guess", "ChatGPT"),
  subjectID = "taster",
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
  residuals = residuals(brownieMix2) ## Notice which model is getting used
) %>%
  ggplot(
    mapping = aes(x = order, y = residuals)
  ) +
  geom_point(
    mapping = aes(shape = recipe, color = recipe),
    size = 2
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
  facet_wrap(facets = vars(taster), nrow = 6) +
  scale_color_manual(values = boastUtils::psuPalette) +
  theme(
    text = element_text(size = 10),
    legend.position = "bottom"
  )

# Omnibus Results ----
parameters::model_parameters(
  model = brownieOmni, # Notice which model we're using here
  effectsize_type = c("eta", "omega", "epsilon")
) %>%
  dplyr::mutate(
    p = ifelse(
      test = is.na(p),
      yes = NA,
      no = pvalRound(p)
    )
  ) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Beer Judging Study",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

## Corrected Table ----
correctedTable <- brownieSphere$`Sphericity Corrections` %>%
  dplyr::select(GGe, `p[GG]`, HFe, `p[HF]`)
correctedTable$`p[GG]` <- lapply( X = correctedTable$`p[GG]`, FUN = pvalRound)
correctedTable$`p[HF]` <- lapply( X = correctedTable$`p[HF]`, FUN = pvalRound)

knitr::kable(
  x = correctedTable,
  digits = 4,
  col.names = c("Greenhouse-Geisser", "p-value", "Huynh-Feldt", "p-value"),
  caption = "Sphericity Corrections",
  align = "c",
  booktab = TRUE
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )
