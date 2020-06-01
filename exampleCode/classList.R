sec1 <- c(
  "Javier A.",
  "Anran C.",
  "Aaron C.",
  "Zelin D.",
  "Carlie F.",
  "Lindsey F.",
  "Abhilasha G.",
  "Brendan G.",
  "Chenese G.",
  "Leah H.",
  "Soni H.",
  "Maura J.",
  "Sung J.",
  "Josh K.",
  "Mingze L.",
  "Manzhao L.",
  "Dane M.",
  "Darrell P.",
  "Anisha. P.",
  "Cassandra P.",
  "Tommy P.",
  "Weitao R.",
  "Allison S.",
  "Patrick S.",
  "Erica S.",
  "Hao S.",
  "Morgan T.",
  "Nick V.",
  "Anastasia V.",
  "Ethan W.",
  "Hilla X.",
  "Yan X.",
  "Ruida Z.",
  "Tianji Z.",
  "George Z.",
  "Ziling Z.",
  "Xuanying Z.",
  "Eli Z.",
  "Tianyi Z."
)
sec3 <- c(
  "Zidong C.",
  "Allison C.",
  "Ariel D.",
  "Regyna H.",
  "Ray H.",
  "Jared J.",
  "Mallet J.",
  "Bora J.",
  "Ryan K.",
  "Dylan K.",
  "Spark K.",
  "Aidan L.",
  "John L.",
  "Nick L.",
  "Gonghao L.",
  "Ryan L.",
  "Hao L.",
  "Rohit M.",
  "Ishan M.",
  "Faiz M.",
  "Izzi O.",
  "Raelee P.",
  "Vinuri P.",
  "Kayla T.",
  "Dan W.",
  "Tong W.",
  "Hong X.",
  "William X.",
  "Keira Y."
)
combinedRoster <- c(sec1, sec3)

set.seed(461)
hw1 <- sample(combinedRoster, size = length(combinedRoster),
              replace = FALSE)
oreo1 <- hw1[1:23]
oreo2 <- hw1[24:47]
oreo3 <- hw1[48:70]
oreoAssignments <- data.frame("Oreo Data Set 1" = c(oreo1,NA),
                    "Oreo Data Set 2" = oreo2,
                    "Oreo Data Set 3" = c(oreo3,NA))
View(oreoAssignments)

assignment <- openxlsx::createWorkbook()
openxlsx::addWorksheet(assignment, sheetName = "Assignments")
openxlsx::writeData(assignment, sheet = "Assignments", oreoAssignments)
openxlsx::saveWorkbook(assignment,"hw1DataAssignment.xlsx", overwrite = TRUE)

remove(list = c("assignment","hw1","combinedRoster",
                "sec1","sec3","oreo1","oreo2","oreo3"))