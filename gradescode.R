#Benjamin Weisel bmw7rx
library(tidyr)

gradesdata <- read.csv("C:/Users/bweis/Downloads/grades - grades.csv.csv")
#Removes all columns with unessecary data
gradesdata <- gradesdata[c(1,2)]
#Renames column
names(gradesdata)[2] = "gpa"
View(gradesdata)
write.csv(gradesdata,'C:/Users/bweis/Documents/SYS2202 final/CleanedGradesData.csv')
