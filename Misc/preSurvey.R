responses <- list(
  r = c(
    rep(0, 10), 1, 2, 3, 3, 3,
    rep(4, 3), rep(5, 4), rep(6, 3),
    rep(7, 7), rep(8, 5), 9),
  sas = c(
    rep(0, 28), 1, 1, 5, 5, 6, 7, 9, 9, 10),
  jmp = c(
    rep(0, 34), 1, 8, 9),
  minitab = c(
    rep(0, 14), rep(1, 4), rep(2, 3), 3, 4, 4,
    rep(5, 4), 6, 6, 7, 7, 8, 8, 9, 10, 10),
  spss = c(
    rep(0, 31), 1, 1, 4, 4, 6, 7),
  dice = c(
    rep("3 ways out of 36", 11),
    rep("chance on next roll", 24),
    "3 products in every 36 rolls",
    "3/36ths of the time"
  ),
  randomness = c(
    rep("can't predict", 2),
    "haphazard",
    rep("no pattern", 9),
    rep("min bias, enable prediction", 18),
    rep("by chance", 5),
    rep("by luck", 2)
  ),
  cis = c(
    rep("about method", 1),
    rep("bewteen 12.3 and 15.7", 29),
    rep("formula given", 1),
    rep("no answer/IDK", 3),
    rep("likely between 12.3 and 15.7", 1),
    rep("most differences between 12.3 and 15.7", 1)
  ),
  height = c(
    rep("48 heights out of 72", 12),
    rep("chance of next person", 13),
    rep("relative freq. of set", 5),
    rep("48/72nds of time", 7)
  ),
  grade = c(
    rep("A", 33),
    rep("B", 3),
    "C"
  )
)

ggplot(
  data = as.data.frame(responses$grade),
  mapping = aes(x = responses$grade)
) +
  geom_bar(
    color = "black",
    fill = "blue"
  ) +
  theme_bw() +
  xlab("Student's grade") +
  ylab("Frequency") +
  theme(
    text = element_text(size = 14)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0, add = c(0,2))
  )


