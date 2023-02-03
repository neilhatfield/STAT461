# Example Code for Fitting RCBD in R and Assessing Assumptions ----

# 1) Load your data ----
# URL for Estimation Errors data
## https://raw.github.com/neilhatfield/STAT461/master/dataFiles/estimationErrors_Fall2022.csv

barleyData <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/barley.dat",
  header = TRUE,
  sep = ","
)


# 2) Set both your factor and your block variables to be "factor" type ----

names(barleyData)[which(names(barleyData) == "Treatment")] <- "Varietal"
barleyData$Varietal <- as.factor(barleyData$Varietal)

barleyData$Field <- as.factor(barleyData$Field)
names(barleyData)[which(names(barleyData) == "Planting.Harvesting.Order")] <- "Order"

# 3) Fit the model--Barley Example ----

barleyModel <- aov(
  formula = Yield ~ Field + Varietal,
  data = barleyData
)

# 4) Assess Assumptions--Barley Example ----

## Check 1 ----
car::qqPlot(
  x = residuals(barleyModel),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (BPA)"
)

## Check 2 ----
ggplot(
  data = data.frame(
    residuals = residuals(barleyModel),
    fitted = fitted.values(barleyModel)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  geom_hline( ## Adds reference line at zero
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_smooth( ## Adds the smoothed line
    formula = y ~ x,
    method = stats::loess,
    method.args = list(degree = 1),
    se = FALSE,
    size = 0.5
  ) +
  theme_bw() +
  xlab("Fitted values (BPA)") +
  ylab("Residuals (BPA)")

## Check 3 ----
ggplot(
  data = data.frame(
    residuals = barleyModel$residuals,
    index = 1:length(barleyModel$residuals)
  ),
  mapping = aes(x = index, y = residuals)
) +
  geom_point(size = 1.5) +
  geom_line() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  xlab("Measurement order") +
  ylab("Residuals")

## Check 4 ----
ggplot2::ggplot(
  data = barleyData,
  mapping = aes(
    x = Varietal,
    y = Yield,
    color = Field,
    shape = Field,
    linetype = Field,
    group = Field
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  ggplot2::theme_bw() +
  xlab("Variety") +
  ylab("Yield (BPA)") +
  labs(color = "Field") +
  theme(
    legend.position = "right"
  )