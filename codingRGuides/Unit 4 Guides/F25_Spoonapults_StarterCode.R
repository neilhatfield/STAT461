# Starter Code for Fall 2025 Spoonapult Study ----
# Load Packages ----
# install.packages(c("tidyverse", "googlesheets4"))
library(tidyverse)
library(googlesheets4)

# Set Global Options ----
options(contrasts = c("contr.sum", "contr.poly"))
options(knitr.kable.NA = "")

# Load Data and Tidy ----
## Load the initial data and tidy to the measurement units
gs4_deauth() # Bypass needing to log into a Google account
gummyBears <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1RL30sRqXOWLybE2WRYMjRailkZtqCdxv5XxHuh9EgQg/edit?usp=sharing"
) %>%
  rename( # Fix Column Names
    Time = 1,
    Team = 2,
    Position = 3,
    Angle = 4,
    GB1 = 5,
    GB2 = 6,
    GB3 = 7
  ) %>%
  pivot_longer(
    cols = starts_with("GB"),
    names_to = "gummyBear",
    values_to = "distance"
  ) %>%
  mutate(
    distance = abs(distance) # Convert negative distances
  )

# Wrangle Data and Tidy to Spoonapults ----
## We will use the SAM of the three measurement units
spoonapults <- gummyBears %>%
  group_by(Time, Team, Position, Angle) %>%
  summarize(
    samDistance = mean(distance),
    .groups = "drop"
  ) %>%
  arrange(Time) %>%
  mutate(
    pseudoorder = row_number(), # Add some order information
    .after = Time,
    across(
      .cols = where(is.character), # Set key attributes as factors
      .fns = ~as.factor(.x)
    )
  )

# Relevel Launch Angle

spoonapults$Angle <- factor(
  x = spoonapults$Angle,
  levels = c("Flat", "Low", "High")
)

# Analysis Code ----
psuPalette <- c("#1E407C", "#BC204B", "#3EA39E", "#E98300",
                "#999999", "#AC8DCE", "#F2665E", "#99CC00")

ggplot(
  data = spoonapults,
  mapping = aes(
    x = Angle,
    y = samDistance,
    color = Position,
    shape = Position,
    linetype = Position,
    group = Position
  )
) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1) +
geom_jitter(
  width = 0.1,
  height = 0.1,
  alpha = 0.7,
  size = 1
) +
  scale_color_manual(values = psuPalette) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  ) +
  labs(
    x = "Launch angle",
    y = "SAM of distance flung (cm/bear)",
    color = "Launch position",
    shape = "Launch position",
    linetype = "Launch position",
    title = "Interaction of Factors",
    caption = "Fall 2025 Spoonapult Study"
  )

# Fit Model ----
spoonapultModel <- aov(
  formula = samDistance ~ Team + Angle*Position,
  data = spoonapults
)

# Assumption Checking ----

## QQ Plot ----
car::qqPlot(
  x = residuals(spoonapultModel),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm/bear)",
  main = "QQ Plot for Fall 2025 Spoonapult Study"
)

## TA Plot ----
ggplot(
  data = data.frame(
    residuals = residuals(spoonapultModel),
    fitted = fitted.values(spoonapultModel)
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
    x = "Fitted values (cm/bear)",
    y = "Residuals (cm/bear)",
    title = "Tukey-Anscombe Plot",
    caption = "Fall 2025 Spoonapult Study"
  )

## Independence of Observations ----

## Interactions of Block and Factors ----
### Team and Position
ggplot(
  data = spoonapults,
  mapping = aes(
    x = Team,
    y = samDistance,
    color = Position,
    shape = Position,
    linetype = Position,
    group = Position
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  theme_bw() +
  labs(
    x = "Team",
    y = "SAM of distance flung (cm/bear)",
    color = "Launch position",
    shape = "Launch position",
    linetype = "Launch position",
    title = "Interaction of Team and Position",
    caption = "Fall 2025 Spoonapult Study"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_color_manual(values = psuPalette)

### Team and Angle
ggplot(
  data = spoonapults,
  mapping = aes(
    x = Team,
    y = samDistance,
    color = Angle,
    shape = Angle,
    linetype = Angle,
    group = Angle
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  theme_bw() +
  labs(
    x = "Team",
    y = "SAM of distance flung (cm/bear)",
    color = "Launch angle",
    shape = "Launch angle",
    linetype = "Launch angle",
    title = "Interaction of Team and Angle",
    caption = "Fall 2025 Spoonapult Study"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_color_manual(values = psuPalette)

### Team and Position & Angle
ggplot(
  data = spoonapults,
  mapping = aes(
    x = Position:Angle,
    y = samDistance,
    color = Team,
    shape = Team,
    linetype = Team,
    group = Team
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  theme_bw() +
  labs(
    x = "Launch Position & Angle",
    y = "SAM of distance flung (cm/bear)",
    color = "Team",
    shape = "Team",
    linetype = "Team",
    title = "Interaction of Team and Position & Angle",
    caption = "Fall 2025 Spoonapult Study"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_color_manual(values = psuPalette)

# Omnibus Results
# Omnibus Test/Modern ANOVA Table ----
## Farming Barley Study
parameters::model_parameters(
  model = spoonapultModel,
  es_type = c("eta", "omega", "epsilon"),
  type = 3,
  drop = "(Intercept)",
  verbose = FALSE
) %>%
  knitr::kable(
    digits = 3,
    row.names = FALSE,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Fall 2025 Spoonapult Study",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")


# Fit Model 2----
spoonapultModel2 <- aov(
  formula = samDistance ~ Team + Angle + Position,
  data = spoonapults
)

parameters::model_parameters(
  model = spoonapultModel2,
  es_type = c("eta", "omega", "epsilon"),
  type = 3,
  drop = "(Intercept)",
  verbose = FALSE
) %>%
  knitr::kable(
    digits = 3,
    row.names = FALSE,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Fall 2025 Spoonapult Study",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )


### Post Hoc ----
## Battery Life Example, Plate Material
spoonPostHoc <- emmeans::emmeans(
  object = spoonapultModel2,
  specs = pairwise ~ Position,
  adjust = "tukey",
  level = 0.9
)

spoonPostHoc$emmeans %>%
  kable(
    digits = 3,
    col.names = c("Position", "Point Est.", "SE", "df", "Lower Bound", "Upper Bound"),
    align = "lccccc"
  ) %>%
  kable_classic()

# Demo code for main effects pairwise with effect sizes ----
## Battery Life Study, Plate Material
### Create effect size table
spoonEffects <- as.data.frame(
  eff_size(
    object = spoonPostHoc,
    sigma = sigma(spoonapultModel2),
    edf = df.residual(spoonapultModel2)
  )
) %>%
  dplyr::mutate(
    contrast = gsub(pattern = "[()]", replacement = "", x = contrast),
    ps = probSup(effect.size),
    .after = effect.size
  ) %>%
  dplyr::select(contrast, effect.size, ps)

### Build table
as.data.frame(spoonPostHoc$contrasts) %>%
  left_join(
    y = spoonEffects,
    by = join_by(contrast == contrast)
  ) %>%
  knitr::kable(
    digits = 3,
    # caption = "Post Hoc Comparisons for Main Effect Plate Material",
    col.names = c("Pair", "Difference", "SE", "DF", "t", "p-value", "Cohen's d",
                  "Prob. of Superiority"),
    align = "lccccccc",
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )

exploratoryModel <- aov(
  formula = samDistance ~ Team + Angle:Position + Team*(Angle:Position),
  data = spoonapults
)

subData <- spoonapults %>%
  mutate(
    Factor = as.factor(paste(Angle, Position, sep = "-"))
  ) %>%
  dplyr::select(Team, Factor, samDistance) %>%
  pivot_wider(
    names_from = Team,
    values_from = samDistance
  ) %>%
  column_to_rownames(var = "Factor") %>%
  as.matrix()

library(additivityTests)
tukey.test(subData, alpha = 0.1)
