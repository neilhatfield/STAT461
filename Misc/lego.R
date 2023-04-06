library(dplyr)
library(openxlsx)
library(ggplot2)
options(contrasts = c("contr.sum", "contr.poly"))

legoPrices <- read.xlsx(
  xlsxFile = "Misc/sets.xlsx",
  sheet = "price"
)

legoSets <- read.xlsx(
  xlsxFile = "Misc/sets.xlsx",
  sheet = "sets"
)

subthemes <- c("Botanical Collection", "Modular Buildings", "Modular Buildings Collection",
            "Winter Village")
themes <- c("Star Wars", "Harry Potter", "Architecture")

legoData0 <- legoPrices %>%
  filter(Subtheme %in% subthemes | Theme %in% themes) %>%
  left_join(
    y = legoSets,
    by = join_by(Number == set_num, Year == year)
  ) %>%
  mutate(
    Retail = round(readr::parse_number(Retail), 2),
    Subtheme = case_when(
      Subtheme == "Modular Buildings Collection" ~ "Modular Buildings",
      .default = Subtheme
    ),
    collection = case_when(
      Theme %in% themes ~ Theme,
      .default = Subtheme
    )
  ) %>%
  dplyr::select(
    Number, collection, Year, SetName, Retail, num_parts, img_url
  ) %>%
  rename(
    setNumber = Number, year = Year,
    setName = SetName, price = Retail, numParts = num_parts, imgURL = img_url
  ) %>%
  filter(
    numParts >= 100,
    price > 0
  )


legoData <- legoData0 %>%
  filter(collection != "Winter Village") %>%
  group_by(collection) %>%
  slice_sample(n = 10)

legoData$collection <- as.factor(legoData$collection)

legoModel <- aov(
  formula = price ~ numParts + collection,
  data = legoData
)

legoModel2 <- aov(
  formula = price ~ numParts + collection + numParts:collection,
  data = legoData
)

car::qqPlot(residuals(legoModel))


legoData %>%
ggplot(
  mapping = aes(x = numParts, y = price)
) +
  geom_point(mapping = aes(color = collection, shape = collection)) +
  geom_smooth(
    mapping = aes(color = collection),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
  ) +
  theme_bw()

anova(legoModel2)

write.csv(legoData, "Misc/lego3.csv", row.names = FALSE)

library(ggimage)
legoData$x <- rep(1:10, times = 5)
legoData$y <- 100*rep(1:5, each = 10)

legoData %>%
ggplot(
  mapping = aes(x = x, y = y)
) +
  geom_image(
    mapping = aes(image = imgURL),
    size = 0.1
  ) +
  theme_bw()
