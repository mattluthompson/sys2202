# Script created by Matt Thompson (mlt2we)

library(gcookbook)
library(tidyverse)
library(farver)
library(dplyr, quietly = T)
library(anytime)

d = read.table(file = 'dining/txt/u01.txt', header = FALSE, col.names = c('datetime','diningHall','meal'), sep = ',')
?read.table
view(d)
write_csv(d, 'diningcsv/u01.csv')
print(file.exists('dining/txt/u00.txt'))

for(i in 0:59){
  if(i<10){
    currentfile = paste('dining/txt/u','0',as.character(i),'.txt', sep = '')
  } else {
    currentfile = paste('dining/txt/u',as.character(i),'.txt', sep = '')
  }
  if(file.exists(currentfile)){
    currentcsv = read.table(file = currentfile, header = FALSE, col.names = c('datetime','diningHall','meal'), sep = ',')
    currentcsv$datetime =  as.POSIXct(currentcsv$datetime, format="%Y-%m-%d%H:%M:%OS")
    currentcsv$diningHall = as.character(currentcsv$diningHall)
    currentcsv$meal = as.character(currentcsv$meal)
    newfile = paste('dining/csvraw/',substr(currentfile,12,14),'.csv',sep='')
    write_csv(currentcsv, newfile)
  } else {
    print(paste('file does not exist: ', currentfile, sep = ''))
  }
}

d = read_csv('dining/csvraw/u01.csv')
view(d)
for (i in 1:length(d$datetime)){
  for (j in 1:ncol(d)){
    if(d[[i,j]] == "53 Commons"){
      d[[i,j]] <- "bad dining hall"
    }
  }
}







