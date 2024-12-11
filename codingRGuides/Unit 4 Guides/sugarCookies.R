# Load Wrangling Packages ----
library(tidyverse)
library(googlesheets4)
library(BrailleR)

psuPalette <- c("#1E407C", "#BC204B", "#3EA39E", "#E98300",
                "#999999", "#AC8DCE", "#F2665E", "#99CC00")

# Load Data ----
gs4_deauth()
cookieRaw <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1qaODkQjYPFLRW8fKNRfoeuA5J3qO5Qp8QqvLsshgMTc/edit?usp=sharing"
)

names(cookieRaw) <- c("Timestamp", "Taster", "Cookie.1_Recipe", "Cookie.1_Rating",
                       "Cookie.2_Recipe", "Cookie.2_Rating", "Cookie.3_Recipe", "Cookie.3_Rating")

cookieOrg <- cookieRaw %>%
  dplyr::select(!Timestamp) %>%
  mutate(
    order = paste0(Cookie.1_Recipe, Cookie.2_Recipe, Cookie.3_Recipe),
    .after = Taster
  ) %>%
  mutate(
    order = gsub(pattern = "Cookie", replacement = "", x = order),
    order = gsub(pattern = "[[:blank:]]", replacement = "", x = order)
  )

cookieLong <- cookieOrg %>%
  pivot_longer(
    cols = -c(Taster, order),
    names_to = c("Position", ".value"),
    names_sep = "_",
  ) %>%
  mutate(
    Position = gsub(pattern = "Cookie\\.", replacement = "", x = Position),
  ) %>%
  filter(
    order %in% c("ABC", "ACB", "BAC", "BCA", "CAB", "CBA") &
    !grepl(pattern = "X_", x = Taster))
  )

cookieLong$order <- as.factor(cookieLong$order)
cookieLong$Position <- as.integer(cookieLong$Position)
cookieLong$Taster <- as.factor(cookieLong$Taster)
cookieLong$Recipe <- as.factor(cookieLong$Type)

cookieWide <- cookieLong %>%
  dplyr::select(Taster, Recipe, Rating) %>%
  pivot_wider(
    id_cols = c(Taster),
    names_from = Recipe,
    values_from = Rating
  )

# Set Global Options ----
options(contrasts = c("contr.sum", "contr.poly"))
options(knitr.kable.NA = "")

# Load Analysis Packages ----
library(car)
library(lme4)
library(rstatix)
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

# Visualize Data ----
ggplot(
  data = cookieLong,
  mapping = aes(
    x = Position,
    y = Rating,
    shape = Recipe,
    color = Recipe
  )
) +
  geom_point(size = 3) +
  theme_bw() +
  facet_wrap(facets = vars(order), nrow = 3) +
  scale_color_manual(values = psuPalette) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16)
  )

## Omnibus Model ----
cookieOmni <- aov(
  formula = Rating ~ Taster + Recipe,
  data = cookieLong
)

## Random Effects (Assumption Checking) ----
cookieMixed <- lme4::lmer(
  formula = Rating ~ (1|Taster) + Recipe,
  data = cookieLong
)

## Model for checking Spherecity ----
cookieSphere <- rstatix::anova_test(
  formula = Rating ~ Recipe + Error(Taster %in% Recipe),
  data = cookieLong
)

# Assumption Checking -----
## Gaussian Residuals ----
car::qqPlot(
  x = residuals(cookieMixed), ## Notice which model is getting used
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (rating)"
)

## Gaussian Taster Effect ----
car::qqPlot(
  x = unlist(
    lme4::ranef(
      object = cookieMixed, ## Notice which model is getting used
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
    residuals = residuals(cookieMixed), ## Notice which model is getting used
    fitted = fitted.values(cookieMixed)
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
  xlab("Fitted values (rating)") +
  ylab("Residuals (rating)")

## Interaction ----
### Line plot ----
ggplot(
  data = cookieLong,
  mapping = aes(x = Recipe, y = Rating, group = Taster, color = Taster)
) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "none"
  )

### Heat Map ----
ggplot(
  data = cookieLong,
  mapping = aes(x = Taster, y = Recipe, weight = Rating)
) +
  geom_bin_2d() +
  theme_bw() +
  labs(
    x = "Taster",
    y = "Recipe",
    fill = "Score"
  ) +
  scale_x_discrete(
    labels = label_wrap_gen(width = 10)
  ) +
  scale_fill_gradient(low = "#D8EAFD", high = "#8E4194") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Sphericity ----
# source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")
sphericityPlot(
  dataWide = cookieWide, # Data needs to be in wide format
  colsIgnore = NULL,
  subjectID = "Taster",
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
