# Snickerdoodle Study
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
  ss = "https://docs.google.com/spreadsheets/d/1Fbe8uDnQNr-ye3TXasJfFRgW36InhYZc1QiqrcgpTLk/edit?usp=sharing",
  col_names = TRUE
)

# Clean data ----
## Drop extra column
tasteDataWide <- tasteDataWide %>%
  dplyr::select(!Timestamp)

## Clean column names
names(tasteDataWide) <- c("subject", "order", "Type A", "Type B", "Type C")
tasteDataWide$subject <- as.factor(tasteDataWide$subject)

## Make a long version of the data
tasteDataLong <- tasteDataWide %>%
  pivot_longer(
    cols = starts_with("Type"),
    names_to = "type",
    names_transform = list(Mix = as.factor),
    values_to = "score"
  ) %>%
  rowwise() %>%
  mutate(
    tasteSeq = order,
    order = str_locate(
      string = order,
      pattern = gsub(
        pattern = "Type ",
        replacement = "",
        x = type)
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
  mapping = aes(x = type, y = score)
) +
  geom_boxplot() +
  theme_bw() +
  theme(
    text = element_text(size = 18)
  )

### Box plot of Score by Subject ----
ggplot(
  data = tasteDataLong,
  mapping = aes(x = subject, y = score)
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
  mapping = aes(x = subject, y = type, weight = score)
) +
  geom_bin_2d() +
  theme_bw() +
  scale_x_discrete(
    # labels = label_wrap_gen(width = 10),
    guide = guide_axis(angle = 45)
  ) +
  scale_fill_gradient2(name = "type") +
  theme(
    text = element_text(size = 12)
  )

### Alluvial Plot ----
# EXPERIMENTAL
library(ggalluvial)
ggplot(
  data = tasteDataLong,
  mapping = aes(y = score, axis1 = type, axis2 = subject, axis3 = score)
) +
  geom_alluvium(aes(fill = type)) +
  geom_stratum(fill = "white", color = "gray") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  scale_x_discrete(
    limits = c("type", "subject", "score"),
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
cookieOmni <- aov(
  formula = score ~ subject + type,
  data = tasteDataLong
)

## Random Effects (Assumption Checking) ----
cookieMixed <- lme4::lmer(
  formula = score ~ (1|subject) + type,
  data = tasteDataLong
)

### Due to singularity, using a Bayesian approach for estimation
bayesMixed <- blme::blmer(
  formula = score ~ (1|subject) + type,
  data = tasteDataLong,
  cov.prior = wishart
)

library(nlme)
cookieMix2 <- nlme::lme(
  fixed = score ~ type,
  random = ~1|subject,
  data = tasteDataLong
)

## Model for checking Spherecity ----
cookieSphere <- rstatix::anova_test(
  data = tasteDataLong,
  formula = score ~ type + Error(subject %in% type)
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
  x = residuals(cookieMixed), ## Notice which model is getting used
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
      whichel = c("subject")
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
      object = cookieMix2, ## Notice which model is getting used
      whichel = c("subject")
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
    residuals = residuals(cookieMix2), ## Notice which model is getting used
    fitted = fitted.values(cookieMix2)
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
    mapping = aes(x = type, y = score, group = subject, color = subject)
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
  colsIgnore = c("order"),
  subjectID = "subject",
  colors = "default"
)

cookieSphere$`Mauchly's Test for Sphericity` %>%
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
  residuals = residuals(cookieMix2) ## Notice which model is getting used
) %>%
  ggplot(
    mapping = aes(x = order, y = residuals)
  ) +
  geom_point(
    mapping = aes(shape = type, color = type),
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
  facet_wrap(facets = vars(subject), nrow = 6) +
  scale_color_manual(values = boastUtils::psuPalette) +
  theme(
    text = element_text(size = 10),
    legend.position = "bottom"
  )

# Incorporate Order Effects ----
## Omnibus Model ----
tasteDataLong$order <- as.factor(tasteDataLong$order)
cookieOmni2 <- aov(
  formula = score ~ subject + order*type,
  data = tasteDataLong
)

## Random Effects (Assumption Checking) ----
cookieMixed2a <- lme4::lmer(
  formula = score ~ (1|subject) + order*type,
  data = tasteDataLong
)
### Still Singular

cookieMixed2b <- nlme::lme(
  fixed = score ~ order*type,
  random = ~1|subject,
  data = tasteDataLong
)

## Assumption Checks ----
car::qqPlot(
  x = residuals(cookieMixed2b), ## Notice which model is getting used
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (score)"
)

car::qqPlot(
  x = unlist(
    lme4::ranef(
      object = cookieMixed2b, ## Notice which model is getting used
      whichel = c("subject")
    )
  ),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Subject Effects"
)

ggplot(
  data = data.frame(
    residuals = residuals(cookieMixed2b), ## Notice which model is getting used
    fitted = fitted.values(cookieMixed2b)
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

## Omnibus Table ----
parameters::model_parameters(
  model = cookieOmni2, # Notice which model we're using here
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
    caption = "ANOVA Table for Snickerdoodle Study",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

# Incorporate Tasting Sequence ----
## Omnibus ----
cookieSeqModel <- aov(
  formula = score ~ type + tasteSeq + Error(subject %in% tasteSeq),
  data = tasteDataLong
)

## Random effects ----
cookieSeqAssumptions <- nlme::lme(
  data = tasteDataLong,
  fixed = score ~ type + tasteSeq,
  random = ~ 1|subject
)

## Assumption Checks ----
car::qqPlot(
  x = residuals(cookieSeqAssumptions), ## Notice which model is getting used
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (score)"
)

car::qqPlot(
  x = unlist(
    lme4::ranef(
      object = cookieSeqAssumptions, ## Notice which model is getting used
      whichel = c("subject")
    )
  ),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Subject Effects"
)

ggplot(
  data = data.frame(
    residuals = residuals(cookieSeqAssumptions), ## Notice which model is getting used
    fitted = fitted.values(cookieSeqAssumptions)
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

## Omnibus Table ----
cookieSeqTemp <- summary(cookieSeqModel)
cookieSeqOmni <- rbind(
  cookieSeqTemp$`Error: subject:tasteSeq`[[1]],
  cookieSeqTemp$`Error: Within`[[1]]
)

row.names(cookieSeqOmni) <- c("Taste Seq.", "Subject", "Type","Type x Subject")

cookieSeqOmni["Subject", "F value"] <- cookieSeqOmni["Subject", "Mean Sq"] /
  cookieSeqOmni["Type x Subject", "Mean Sq"]
cookieSeqOmni["Subject", "Pr(>F)"] <- pf(
  q = cookieSeqOmni["Subject", "F value"],
  df1 = cookieSeqOmni["Subject", "Df"],
  df2 = cookieSeqOmni["Type x Subject", "Df"],
  lower.tail = FALSE
)

cookieSeqOmni %>%
  tibble::rownames_to_column(
    var = "Source"
  ) %>%
  dplyr::mutate(
    `Pr(>F)` = ifelse(
      test = is.na(`Pr(>F)`),
      yes = NA,
      no = pvalRound(`Pr(>F)`)
    )
  ) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Source", "df", "SS", "MS", "F", "p-value"),
    caption = "ANOVA Table for Snickerdoodle Study-Nested",
    align = c('l',rep('c',5)),
    booktab = TRUE,
    format.args = list(big.mark = ",")
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )
