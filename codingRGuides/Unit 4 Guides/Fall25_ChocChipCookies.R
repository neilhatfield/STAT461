# Load Wrangling Packages ----
library(tidyverse)
library(googlesheets4)
library(BrailleR)
library(openxlsx)
library(car)
library(lme4)
library(rstatix)
library(blme)
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

# Set Global Options ----
options(contrasts = c("contr.sum", "contr.poly"))
options(knitr.kable.NA = "")

psuPalette <- c("#1E407C", "#BC204B", "#3EA39E", "#E98300",
                "#999999", "#AC8DCE", "#F2665E", "#99CC00")

# Load Data ----
cookieLong <- read.xlsx(
  xlsxFile = "~/Desktop/fall2025_cookies.xlsx",
  sheet = 1
) %>%
  mutate(
    cookie = case_match(
      .x = cookie,
      "A" ~ "Low price",
      "B" ~ "High price",
      "C" ~ "Mid price"
    ),
    across(
      .cols = c(Subject, assigned),
      .fns = ~as.factor(.x)
    ),
    cookie = factor(cookie, levels = c("Low price", "Mid price", "High price"))
  )

cookieWide <- cookieLong %>%
  dplyr::select(Subject, assigned, cookie, score) %>%
  pivot_wider(
    names_from = cookie,
    values_from = score
  ) %>%
  dplyr::select(
    Subject, assigned,
    Low.price = `Low price`,
    Mid.price = `Mid price`,
    High.price = `High price`
  )


# Visualize Data ----
ggplot(
  data = cookieLong,
  mapping = aes(x = score)
) +
  geom_histogram(
    color = "black",
    fill = psuPalette[1],
    closed = "left",
    boundary = 1,
    binwidth = 1,
    pad = TRUE
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
  ) +
  theme_bw() +
  labs(
    x = "Enjoyment score (Likert)",
    y = "Frequency",
    title = "Enjoyment Scores-Fall 2025"
  )

ggplot(
  data = cookieLong,
  mapping = aes(x = score, fill = cookie)
) +
  geom_histogram(
    color = "black",
    closed = "left",
    boundary = 1,
    binwidth = 1,
    pad = TRUE
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
  ) +
  scale_fill_manual(values = psuPalette[2:4]) +
  theme_bw() +
  labs(
    x = "Enjoyment score (Likert)",
    y = "Frequency",
    fill = "Ingredient price point",
    title = "Enjoyment Scores-Fall 2025"
  ) +
  theme(legend.position = "bottom")

ggplot(
  data = cookieLong,
  mapping = aes(y = score, x = cookie, fill = cookie)
) +
  geom_boxplot() +
  scale_fill_manual(values = psuPalette[2:4]) +
  theme_bw() +
  labs(
    y = "Enjoyment score (Likert)",
    x = "Ingredient price point",
    fill = "Ingredient price point",
    title = "Enjoyment Scores-Fall 2025"
  ) +
  theme(
    legend.position = "bottom"
  )

ggplot(
  data = cookieLong,
  mapping = aes(
    x = order,
    y = score,
    shape = cookie,
    color = cookie
  )
) +
  geom_jitter(size = 3, width = 0.1, height = 0.1) +
  theme_bw() +
  scale_color_manual(values = psuPalette[2:4]) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "Tasting order",
    y = "Enjoyment score (Likert)",
    color = "Ingredient price point",
    shape = "Ingredient price point",
    title = "Enjoyment Scores-Fall 2025"
  )

## Omnibus Model ----
cookieOmni <- aov(
  formula = score ~ Subject + cookie,
  data = cookieLong
)

## Random Effects (Assumption Checking) ----
cookieMixed <- lme4::lmer(
  formula = score ~ (1|Subject) + cookie,
  data = cookieLong
)

## Partial Bayesian ----
cookieBayes <- blmer(
  formula = score ~ (1|Subject) + cookie,
  data = cookieLong,
  cov.prior = wishart
)

## Model for checking Spherecity ----
cookieSphere <- rstatix::anova_test(
  formula = score ~ cookie + Error(Subject %in% cookie),
  data = cookieLong
)

# Assumption Checking -----
## Gaussian Residuals ----
car::qqPlot(
  x = residuals(cookieBayes), ## Notice which model is getting used
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (rating)",
  main = "QQ Plot of Residuals for Fall 2025 Cookie Study"
)

psych::skew(residuals(cookieBayes))
psych::kurtosi(residuals(cookieBayes))


subEffects <- unlist(
  lme4::ranef(
    object = cookieBayes, ## Notice which model is getting used
    whichel = c("Subject")
  )
)
## Gaussian Taster Effect ----
car::qqPlot(
  x = subEffects,
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Subject Effects",
  main = "QQ Plot for Subject Effects in Fall 2025"
)

psych::skew(subEffects)
psych::kurtosi(subEffects)

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(cookieBayes), ## Notice which model is getting used
    fitted = fitted.values(cookieBayes)
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
    method = "loess",
    method.args = list(degree = 1),
    se = FALSE,
    linewidth = 0.5
  ) +
  theme_bw() +
  labs(
    x = "Fitted values (rating)",
    y = "Residuals (rating)",
    title = "Fall 2025 Cookie Study"
  )

## Interaction ----
ggplot(
  data = cookieLong,
  mapping = aes(
    x = order,
    y = score,
    color = cookie,
    shape = cookie,
    group = Subject
  )
) +
  geom_point(size = 3) +
  geom_path(
    color = "grey50"
  ) +
  labs(
    x = "Tasting order",
    y = "Enjoyment score (Likert)",
    color = "Ingredient price point",
    shape = "Ingredient price point",
    title = "Enjoyment Scores-Fall 2025"
  ) +
  scale_color_manual(values = psuPalette[2:4]) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(facets = vars(assigned))


## Sphericity ----
# source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")
sphericityPlot(
  dataWide = cookieWide, # Data needs to be in wide format
  colsIgnore = c("assigned"),
  subjectID = "Subject",
  colors = "psu"
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
    font_size = 24,
    latex_options = c("HOLD_position")
  )

# Omnibus Results ----
## No correction
parameters::model_parameters(
  model = cookieOmni, # Notice which model we're using here
  es_type = c("eta", "omega", "epsilon")
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
    caption = "ANOVA Table for Sugar Cookie Study (Fall '24)",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

## Correction
correctedTable <- cookieSphere$`Sphericity Corrections` %>%
  dplyr::select(GGe, `p[GG]`, HFe, `p[HF]`)

correctedTable$`p[GG]` <- lapply(
  X = correctedTable$`p[GG]`,
  FUN = pvalRound
)
correctedTable$`p[HF]` <- lapply(
  X = correctedTable$`p[HF]`,
  FUN = pvalRound
)

kable(
  x = correctedTable,
  digits = 4,
  col.names = c("Greenhouse-Geisser", "p-value", "Huynh-Feldt", "p-value"),
  # caption = "Sphericity Corrections",
  align = "c",
  booktab = TRUE
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )


# Post Hoc Pairwise ----
cookiePH <- emmeans::emmeans(
  object = cookieMixed,
  specs = pairwise ~ Recipe,
  adjust = "FDR",
  level = 0.90
)

as.data.frame(cookiePH$contrasts) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Comparison", "Difference","SE", "DF",
                  "t Statistic","p-value"),
    caption = "Marginal Means-FDR 90\\% Adjustment",
    align = c("l", rep("c", 5)),
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )

## Effect Sizes ----
tempEMM <- emmeans::emmeans(
  object = cookieMixed,
  specs = "Recipe"
)

# Pass the stored marginals into the effect size function
cohenTemp <- emmeans::eff_size(
  object = tempEMM,
  sigma = sigma(cookieMixed),
  edf = df.residual(cookieMixed)
)

# Make a nice table
as.data.frame(cohenTemp) %>%
  dplyr::mutate(
    ps = probSup(effect.size),
    .after = effect.size
  ) %>%
  dplyr::select(contrast, effect.size, ps) %>%
  knitr::kable(
    digits = 3,
    col.names = c("Comparison", "Cohen's d", "Probability of Superiority"),
    align = "lcc",
    caption = "Effect Sizes for Sugar Cookie Study",
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = "HOLD_position"
  )
