library(dplyr)
library(openxlsx)

legoPrices <- read.xlsx(
  xlsxFile = "~/Desktop/sets.xlsx",
  sheet = "price"
)

legoSets <- read.xlsx(
  xlsxFile = "~/Desktop/sets.xlsx",
  sheet = "sets"
)

subthemes <- c("Botanical Collection", "Modular Buildings", "Modular Buildings Collection",
            "Winter Village")
themes <- c("Star Wars", "Harry Potter")

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
  group_by(collection) %>%
  slice_sample(n = 10)

legoData$collection <- as.factor(legoData$collection)\

legoModel <- aov(
  formula = price ~ numParts + collection,
  data = legoData
)

legoModel2 <- aov(
  formula = price ~ numParts + collection + numParts:collection,
  data = legoData
)

plot(legoModel)

library(ggplot2)
ggplot(
  data = legoData,
  mapping = aes(x = numParts, y = price)
) +
  geom_point(mapping = aes(color = collection, shape = collection)) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "black"
  ) +
  theme_bw()
