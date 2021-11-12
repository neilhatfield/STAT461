callOnStudent(sec = 2, num = 1)

MWF <- c(
  "Alex", "Maria", "Allison", "Mitch",
  "Connor", "Marquis", "Miranda", "Zackery (Dingyu)",
  "Jack (Mengqiu)", "Anthony", "Quichen", "Nick",
  "Luke", "Garian", "Brendan", "Hong",
  "Justyna", "Ben", "Alyssa", "Yuanyuan",
  "Sam (Xunheng)", "Jason (Junsheng)", "Xiaowen", "Yue",
  "Enhao", "Bill (Ziheng)", "Qinhao (Jason)", "Dengchen"
)

TRF <- c(
  "Yuning", "Jessica", "Reid", "Quan", "Aish",
  "Arpit", "Allison", "Hanming",
  "Lydia (Xinyi)", "Jinfan", "Miaosen"
)

callOnStudent <- function(sec, num, classLists = list(MWF, TRF)) {
  if (sec == 2) {
    return(
      sample(
        x = classLists[[1]],
        size = num,
        replace = ifelse(
          test = num > length(classLists[[1]]),
          yes = TRUE,
          no = FALSE
        )
      )
    )
  } else if (sec == 3) {
    return(
      sample(
        x = classLists[[2]],
        size = num,
        replace = ifelse(
          test = num > length(classLists[[2]]),
          yes = TRUE,
          no = FALSE
        )
      )
    )
  }
}
