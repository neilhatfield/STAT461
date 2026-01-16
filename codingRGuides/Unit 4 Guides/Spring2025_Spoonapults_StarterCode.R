# Data Wrangling Spring 2025 Spoonapult Data ----
#' This code is for loading and wrangling the data from the Spring 2025
#' Spoonapult study from Stat 461.

# Install Packages (if missing) ----
## Delete the hashtag at the start of each line and then submit the commands in R
# install.packages("openxlsx")
# install.package("tidyverse")

# Load Packages ----
library(openxlsx)
library(tidyverse)

# Load raw data ----
rawSpoons <- read.xlsx(
  xlsxFile = "https://raw.githubusercontent.com/neilhatfield/STAT461/main/dataFiles/spoonapults_Spring2025.xlsx"
)

# Wrangle Spoonapult Data ----
#' Data are not tidy; treatments are embedded in a single column
#' instead of separate columns for each factor.
#' Factor levels have mixture of capital and lower case first letters
#' The order of the entries in the treatment cells is not consistent
#' The two launches have some cells with unit of measurement included.
#' The two launches need to be aggregated to a single value for each spoonapult

spoonapultData <- rawSpoons %>%
  mutate(
    Treatment = str_squish(str_to_lower(Treatment))
  ) %>%
  mutate(
    position = case_when(
      grepl(pattern = "back", Treatment) ~ "back",
      grepl(pattern = "front", Treatment) ~ "front",
      .default = "error"
    ),
    angle = case_when(
      grepl(pattern = "flat", Treatment) ~ "flat",
      grepl(pattern = "low", Treatment) ~ "low",
      grepl(pattern = "high", Treatment) ~ "high",
      .default = "error"
    ),
    .after = Treatment
  ) %>%
  mutate(
    Group = paste("Team", Group),
    first = parse_number(first),
    second = parse_number(second)
  ) %>%
  dplyr::select(!Treatment) %>%
  pivot_longer(
    cols = c(first, second),
    names_to = "shot",
    values_to = "distance"
  ) %>%
  group_by(Spoonapult, Group, position, angle) %>%
  summarize(
    sam_distance = mean(distance),
    .groups = "drop"
  )

## Set Factor Data Types ----
spoonapultData$Group <- as.factor(spoonapultData$Group)
spoonapultData$position <- as.factor(spoonapultData$position)
spoonapultData$angle <- factor(x = spoonapultData$angle, levels = c("flat", "low", "high"))

# Load Additional Packages & Set Options ----
packages <- c("tidyverse", "hasseDiagram", "knitr", "kableExtra",
              "car", "psych", "parameters", "emmeans", "DescTools", "openxlsx")
lapply(
  X = packages,
  FUN = library,
  character.only = TRUE,
  quietly = TRUE
)
options(contrasts = c("contr.sum", "contr.poly"))
options(knitr.kable.NA = "")

# Load additional tools ----
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

# Custom Color Palette ----
psuPalette <- c("#1E407C", "#BC204B", "#3EA39E", "#E98300",
                "#999999", "#AC8DCE", "#F2665E", "#99CC00")

# Hasse Diagram ----
modelLabels <- c("1 Launch 1", "4 Team 3", "3 L. Angle 2", "2 L. Position 1", "6 L. Angle × L. Position 2", "24 (Spoonapult) 15")
modelMatrix <- matrix(
  data = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  nrow = 6,
  ncol = 6,
  byrow = FALSE
)
hasseDiagram::hasse(
  data = modelMatrix,
  labels = modelLabels
)

# Factor Interactions ----
ggplot(
  data = spoonapultData,
  mapping = aes(
    x = angle,
    y = sam_distance,
    shape = position,
    color = position,
    linetype = position,
    group = position
  )
) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1) +
  geom_jitter(
    width = 0.1,
    height = 0.1,
    alpha = 0.4,
    size = 1
  ) +
  theme_bw() +
  labs(
    title = "Interactions of Factors",
    x = "Launch angle",
    y = "SAM of distance flung (cm/bear)",
    shape = "Launch position",
    color = "Launch position",
    linetype = "Launch position"
  ) +
  scale_color_manual(values = psuPalette) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )

# Fit Model ----
spoonModel <- aov(
  formula = sam_distance ~ Group + position*angle,
  data = spoonapultData
)

spoonModel2 <- aov(
  formula = sam_distance ~ Group + position + angle,
  data = spoonapultData
)

# Assess Assumptions ----
## Gaussian Residuals ----
car::qqPlot(
  x = residuals(spoonModel),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm/bear/pult)",
  main = "QQ Plot for Spoonapult Study (Sp '25)"
)

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(spoonModel),
    fitted = fitted.values(spoonModel)
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
    title = "Tukey-Anscombe Plot",
    x = "Fitted values (cm/bear/pult)",
    y = "Residuals (cm/bear/pult)"
  )

## Interactions of Block and Factors ----

### Group & Position ----
ggplot(
  data = spoonapultData,
  mapping = aes(
    x = Group,
    y = sam_distance,
    color = position,
    shape = position,
    linetype = position,
    group = position
  )
) +
  stat_summary(fun = "mean", geom = "point") + # <3>
  stat_summary(fun = "mean", geom = "line") + # <3>
  theme_bw() +
  labs(
    title = "Interaction of Group & Position",
    x = "Group",
    y = "SAM of distance flung (cm/bear/pult)",
    color = "Launch position",
    shape = "Launch position",
    linetype = "Launch position"
  ) +
  theme(
    legend.position = "right"
  ) +
  scale_color_manual(values = psuPalette)

### Group & Angle ----
ggplot(
  data = spoonapultData,
  mapping = aes(
    x = Group,
    y = sam_distance,
    color = angle,
    shape = angle,
    linetype = angle,
    group = angle
  )
) +
  stat_summary(fun = "mean", geom = "point") + # <3>
  stat_summary(fun = "mean", geom = "line") + # <3>
  theme_bw() +
  labs(
    title = "Interaction of Group & Angle",
    x = "Group",
    y = "SAM of distance flung (cm/bear/pult)",
    color = "Launch angle",
    shape = "Launch angle",
    linetype = "Launch angle"
  ) +
  theme(
    legend.position = "right"
  ) +
  scale_color_manual(values = psuPalette)

### Group & Treatment ----
ggplot(
  data = spoonapultData,
  mapping = aes(
    x = position:angle,
    y = sam_distance,
    color = Group,
    shape = Group,
    linetype = Group,
    group = Group
  )
) +
  stat_summary(fun = "mean", geom = "point") + # <3>
  stat_summary(fun = "mean", geom = "line") + # <3>
  theme_bw() +
  labs(
    title = "Interaction of Group & Treatment",
    x = "Treatments",
    y = "SAM of distance flung (cm/bear/pult)",
    color = "Group",
    shape = "Group",
    linetype = "Group"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_color_manual(values = psuPalette)

# Results ----
## Omnibus ----
parameters::model_parameters(
  model = spoonModel,
  es_type = c("eta", "omega", "epsilon"),
  type = 3,
  drop = "(Intercept)",
  verbose = FALSE
) %>%
  knitr::kable(
    digits = 2,
    format.args = list(big.mark = ","),
    row.names = FALSE,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Spring 2025 Spoonapult Study",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

## Post Hoc Pairwise ----
### Main Effects ----
spoonPostHoc <- emmeans::emmeans(
  object = spoonModel,
  specs = pairwise ~ angle,
  adjust = "sidak",
  level = 0.9
)

knitr::kable(
  x = spoonPostHoc$contrasts,
  digits = 3,
  caption = "Post Hoc Comparisons for Main Effect Launch Angle",
  col.names = c("Pair", "Difference", "SE", "DF", "t", "p-value"),
  align = "lccccc",
  booktabs = TRUE
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )

### Conditional Main ----
anglePositionPostHoc <- emmeans::emmeans(
  object = spoonModel,
  specs = pairwise ~ angle | position,
  adjust = "BH",
  level = 0.90
)

knitr::kable(
  x = anglePositionPostHoc$contrasts,
  digits = 3,
  caption = "Post Hoc Comparisons for Launch Angle Conditioned by Position",
  col.names = c("Pair", "Pos.", "Difference", "SE", "DF", "t", "p-value"),
  align = "llccccc",
  booktabs = TRUE
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )

### Treatments ----
treatmentPostHoc <- emmeans::emmeans(
  object = spoonModel,
  specs = pairwise ~ position:angle,
  ajust = "tukey",
  level = 0.9
)

## Generate effect sizes
treatmentEffects <- as.data.frame(
  eff_size(
    object = treatmentPostHoc,
    sigma = sigma(spoonModel),
    edf = df.residual(spoonModel)
  )
) %>%
  dplyr::mutate(
    contrast = gsub(pattern = "[()]", replacement = "", x = contrast),
    ps = probSup(effect.size),
    .after = effect.size
  ) %>%
  dplyr::select(contrast, effect.size, ps)

## Build table
as.data.frame(treatmentPostHoc$contrasts) %>%
  left_join(
    y = treatmentEffects,
    by = join_by(contrast == contrast)
  ) %>%
  kable(
    digits = 3,
    caption = "Post Hoc Comparisons of Spoonapult Treatments",
    col.names = c("Pair", "Difference", "SE", "DF", "t", "p-value",
                  "Cohen's d", "Prob. of Superiority"),
    align = "lccccccc",
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12,
    latex_options = c("scale_down")
  )


