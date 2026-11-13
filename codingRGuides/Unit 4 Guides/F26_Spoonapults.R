# Analyzing Spoonapult Data ----

## Load Packages ----
library(tidyverse)
library(psych)
library(car)
library(nullabor)
library(parameters)
library(knitr)
library(kableExtra)
library(emmeans)

## Set Global Options ----
options(contrasts = c("contr.sum", "contr.poly"))
options(knitr.kable.NA = "")

psuPalette <- c("#1E407C", "#BC204B", "#3EA39E", "#E98300",
                "#999999", "#AC8DCE", "#F2665E", "#99CC00")

## Load Data ----
spoonapults <- read.table(
  file = "dataFiles/spoonapults_Fall2026.csv",
  header = TRUE,
  sep = ","
)

str(spoonapults)

spoonapults$Kit <- as.factor(spoonapults$Kit)
spoonapults$Position <- factor(
  x = spoonapults$Position,
  levels = c("Front", "Back")
)
spoonapults$Angle <- factor(
  x = spoonapults$Angle,
  levels = c("Flat", "Low", "High")
)

## EDA ----
### Box Plots ----
ggplot(
  data = spoonapults,
  mapping = aes(
    x = interaction(Position, Angle, sep = " x "),
    y = sam
  )
) +
  geom_boxplot() +
  labs(
    x = "Launch Position by Angle",
    y = "SAM of Distance Flung (cm/bear)"
  ) +
  theme_bw()

### Descriptive Stats ----
spoonapultStats <- describeBy(
  sam ~ Position + Angle,
  data = spoonapults,
  quant = c(0.25, 0.75),
  IQR = TRUE,
  mat = TRUE,
  digits = 4
)

spoonapultStats |>
  remove_rownames() |>
  dplyr::select(group1, group2, n, min, Q0.25, median, Q0.75, max,
                mad, mean, sd, skew, kurtosis) |>
  kable(
    digits = 2,
    col.names = c("Position", "Angle", "n", "Min", "Q1", "Median", "Q3",
                  "Max", "MAD", "SAM", "SASD", "S. Skew",
                  "S. Ex. Kurtosis"),
    align = "llccccccccccc",
    booktabs = TRUE
  ) |>
  kable_classic()

### Check Interactions ----
ggplot(
  data = spoonapults,
  mapping = aes(
    x = Angle,
    y = sam,
    shape = Position,
    color = Position,
    linetype = Position,
    group = Position
  )
) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_manual(values = psuPalette) +
  labs(
    x = "Launch Angle",
    y = "SAM of Distance Flung (cm/bear)",
    shape = "Launch Position",
    color = "Launch Position",
    linetype = "Launch Position"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )

## Fit the Model ----
spoonModel <- aov(
  formula = sam ~ Position*Angle,
  data = spoonapults
)

## Assess Assumptions ----
### Gaussian Residuals ----
qqPlot(
  x = residuals(spoonModel),
  distribution = "norm",
  envelope = 0.9,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm/bear)"
)

#### Line Up Test ----
lineup_residuals(model = spoonModel, type = 2)

### Homoscedasticity ----
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
  labs(
    x = "Fitted values (cm/bear)",
    y = "Residuals (cm/bear)"
  ) +
  theme_bw()

#### Line Up Test ----
lineup_residuals(model = spoonModel, type = 1) + theme_bw()

## Omnibus Results ----
model_parameters(
  model = spoonModel,
  es_type = c("eta", "omega", "epsilon"),
  type = 3,
  drop = "(Intercept)",
  verbose = FALSE
) |>
  kable(
    digits = 2,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    format.args = list(big.mark = ","),
    align = "lcccccccc",
    booktab = TRUE
  ) |>
  kable_classic()

### Point Estimates ----
pointEst <- dummy.coef(spoonModel)
pointEst <- unlist(pointEst)
names(pointEst) <- c(
  "Grand Mean",
  levels(spoonapults$Position),
  levels(spoonapults$Angle),
  outer(
    levels(spoonapults$Position),
    levels(spoonapults$Angle),
    FUN = paste,
    sep = " x "
  )
)

data.frame("Estimate" = pointEst) |>
  rownames_to_column("source") |>
  kable(
    digits = 2,
    col.names = c("Source", "Estimate"),
    booktabs = TRUE,
    align = "lc"
  ) |>
  kable_classic(
    full_width = FALSE
  ) |>
  group_rows(group_label = "Launch Position", start_row = 2, end_row = 3) |>
  group_rows(group_label = "Launch Angle", start_row = 4, end_row = 6) |>
  group_rows(group_label = "Interactions", start_row = 7, end_row = 12)

#### With Intervals ----
##### Fit Each Effect Separately ----
positionIntervals <- emmeans(
  object = spoonModel,
  specs = ~ Position,
  level = 0.93,
  adjust = "sidak"
)

angleIntervals <- emmeans(
  object = spoonModel,
  specs = ~ Angle,
  level = 0.93,
  adjust = "sidak"
)

interactionIntervals <- emmeans(
  object = spoonModel,
  specs = ~ Position:Angle,
  level = 0.93,
  adjust = "sidak"
)

##### Combine into One Family ----
rbind(positionIntervals, angleIntervals, interactionIntervals, adjust = "sidak") |>
  as.data.frame() |>
  mutate(
    Position = na_if(x = Position, "."),
    Angle = na_if(x = Angle, ".")
  ) |>
  kable(
    digits = 2,
    col.names = c("Position", "Angle", "Marginal Mean", "SE", "df",
                  "Lower Bound", "Upper Bound"),
    align = "llccccc",
    booktabs = TRUE
  ) |>
  kable_classic()
