# Fall 2023 Gummy Bears Data ----
library(tidyverse)
library(openxlsx)

bears <- openxlsx::readWorkbook(
  xlsxFile = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/gummyBears_Fall2023.xlsx",
  sheet = 1,
  colNames = TRUE,
  rowNames = FALSE
)

# Measurement Units Analysis ----
bears$angle <- as.factor(bears$angle)
bears$position <- as.factor(bears$position)
bears$team <- as.factor(bears$team)

bears <- bears %>%
  mutate(
    shifted_distance = case_when(
      position == "Front" ~ distance - 2.54*3,
      position == "Back" ~ distance - 2.54*33
    )
  )

ggplot(
  data = bears,
  mapping = aes(x = angle, y = distance, fill = position)
) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = boastUtils::psuPalette[2:3]) +
  labs(
    title = "Side-by-Side Box Plots for Gummy Bear Study",
    x = "Launch angle",
    y = "Distance flown (cm)",
    fill = "Launch position"
  )
ggsave(filename = "~/Desktop/temp1.png", width = 6, height = 4, units = "in")

ggplot(
  data = bears,
  mapping = aes(
    x = angle,
    y = distance,
    shape = position,
    color = position,
    linetype = position,
    group = position
  )
) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.25, size = 1) +
  ggplot2::theme_bw() +
  labs(
    x = "Launch angle",
    y = "Distance flown (cm)",
    color = "Launch position",
    shape = "Launch position",
    linetype = "Launch position",
    title = "Interaction Plot for Launch Angle and Position"
  ) +
  scale_color_manual(values = boastUtils::psuPalette[2:3]) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
ggsave(filename = "~/Desktop/temp1.png", width = 6, height = 4, units = "in")

modelGB <- aov(
  formula = distance ~ angle*position,
  data = bears
)

car::qqPlot(
  x = residuals(modelGB),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm)"
)

ggplot(
  data = data.frame(
    residuals = residuals(modelGB),
    fitted = fitted.values(modelGB)
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
  xlab("Fitted values (cm)") +
  ylab("Residuals (cm)") +
  ggtitle("Tukey-Anscombe Plot for Gummy Bear Study")
ggsave(filename = "~/Desktop/ta_ModelGB.png", width = 3, height = 2, units = "in")

ggplot(
  data = bears,
  mapping = aes(
    x = order,
    y = distance
  )
) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  xlab("Measurement order") +
  ylab("Distance (cm)") +
  facet_wrap(
    facets = vars(team),
    scales = "fixed"
  ) +
  ggtitle("Index Plots by Spoonapult")
ggsave(filename = "~/Desktop/index_ModelGB.png", width = 6, height = 4, units = "in")


means <- bears %>%
  group_by(team) %>%
  summarize(
    sam = mean(distance, na.rm = TRUE),
    sasd = sd(distance, na.rm = TRUE)
  )

catData2 <- left_join(
  x = bears,
  y = means,
  by = join_by("team" == "team")
)

# Create the Index Plot by Group/Team ----
ggplot(
  data = catData2,
  mapping = aes(
    x = order,
    y = distance
  )
) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_smooth(
    formula = y ~ x,
    method = "lm",
    se = TRUE,
    level = 0.9,
    linetype = 1
  ) +
  geom_line(
    data = catData2,
    mapping = aes(y = sam, x = order),
    linetype = 2,
    color = "red"
  ) +
  geom_line(
    data = catData2,
    mapping = aes(y = (sam - 3*sasd), x = order),
    linetype = "dashed",
    color = boastUtils::psuPalette[2]
  ) +
  geom_line(
    data = catData2,
    mapping = aes(y = (sam + 3*sasd), x = order),
    linetype = "dashed",
    color = boastUtils::psuPalette[2]
  ) +
  theme_bw() +
  xlab("Measurement order") +
  ylab("Distance (cm)") +
  facet_wrap(
    facets = vars(team),
    scales = "fixed"
  ) +
  ggtitle("Index-Control Plots by Spooonapult")
ggsave(filename = "~/Desktop/temp1.png", width = 6, height = 4, units = "in")


plot(model1)
parameters::model_parameters(model1, effectsize_type = c("eta", "omega", "epsilon")) %>%
  kable(digits = 4) %>%
  kable_classic()

model1b <- aov(
  formula = shifted_distance ~ angle*position,
  data = bears
)
plot(model1b)
anova(model1b)

# Experimental Unit Analysis ----
bears2 <- bears %>%
  group_by(angle, position, team) %>%
  summarize(
    sam_dist = mean(distance, na.rm = TRUE),
    sam_shifted = mean(shifted_distance, na.rm = TRUE)
  )

modelSP <- aov(
  formula = sam_dist ~ angle*position,
  data = bears2
)

car::qqPlot(
  x = residuals(modelSP),
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm)"
)

ggplot(
  data = data.frame(
    residuals = residuals(modelSP),
    fitted = fitted.values(modelSP)
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
  xlab("Fitted values (cm)") +
  ylab("Residuals (cm)") +
  ggtitle("Tukey-Anscombe Plot for Gummy Bear Study")
ggsave(filename = "~/Desktop/ta_ModelGB.png", width = 3, height = 2, units = "in")

ggplot(
  data = bears,
  mapping = aes(
    x = order,
    y = distance
  )
) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  xlab("Measurement order") +
  ylab("Distance (cm)") +
  facet_wrap(
    facets = vars(team),
    scales = "fixed"
  ) +
  ggtitle("Index Plots by Spoonapult")




plot(model2)
parameters::model_parameters(model2, effectsize_type = c("eta", "omega", "epsilon")) %>%
  kable(digits = 4) %>%
  kable_classic()

model2b <- aov(
  formula = sam_shifted ~ angle*position,
  data = bears2
)
plot(model2b)
anova(model2b)
