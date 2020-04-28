#Benjamin Weisel bmw7rx
library(tidyr)

deadlinesdata <- read.csv("C:/Users/bweis/Documents/SYS2202 final/deadlines.csv")
#Removes extra columns with NA values
deadlinesdata <- deadlinesdata[-c(73:147)]
#Creates new column with total sum of deadlines
deadlinesdata$total <- rowSums( deadlinesdata[,2:72])
#Removes all columns besides the uid and total deadline count
deadlinesdata <- deadlinesdata[-c(2:72)]
View(deadlinesdata)
write.csv(deadlinesdata, "C:/Users/bweis/Documents/SYS2202 final/CleanedDeadlinesData.csv")
