# Script created by Matt Thompson (mlt2we)

#Load all necessary packages
library(gcookbook)
library(tidyverse)
library(farver)
library(dplyr, quietly = T)
library(anytime)
library(lubridate)

cleanData <- function(currentfile) {
  d = read_csv(currentfile)
  #convert epoch into 'human time' and find time differences
  d =  mutate(d, datetime = as.character(as.POSIXct(anytime(start), format="%Y-%m-%d%H:%M:%OS")))
  d = mutate(d, lock_length = as.numeric(as.POSIXct(anytime(end), format="%Y-%m-%d%H:%M:%OS") - as.POSIXct(anytime(start), format="%Y-%m-%d%H:%M:%OS")))
  d$start <- NULL
  d$end <- NULL
  
  #create new data frame that will contain binned data by day, #https://stackoverflow.com/questions/32712301/create-empty-data-frame-with-column-names-by-assigning-a-string-vector/32712555
  dbinned <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('datetime','num_locked', 'total_lock_time'))
  
  # set binning parameters
  lock_total = 0
  num_locked = 0
  day = day(as.POSIXct(d$datetime[1]))
  binned_index = 0
  
  # bin data by hour based on total lock time and number of times locked that day
  for (i in 1:length(d$datetime)){
    #reset the day to compare whenever it's a new day
    current_day = day
    day = day(as.POSIXct(d$datetime[i]))
    
    #bin if new day
    if(day != current_day | i == length(d$datetime)) {
      dbinned <- add_row(dbinned)
      binned_index <- binned_index + 1
      dbinned$datetime[binned_index] <- as.POSIXct(substring(d$datetime[i-1],1,10), format="%Y-%m-%d")
      dbinned$num_locked[binned_index] <- num_locked
      dbinned$total_lock_time[binned_index] <- round(lock_total, digits = 2)
      
      #reset binning parameters once binned
      num_locked = 0
      lock_total = 0
    } else {
      #if same day, add to binning parameters
      lock_total = lock_total + d$lock_length[i]
      num_locked = num_locked + 1
    }
  }
  dbinned <- mutate(dbinned, datetime = as.POSIXct(anytime(datetime), format="%Y-%m-%d"))
}

#iterate through each user data and clean them using function above
for(i in 0:59){
  if(i<10){
    currentfile = paste('0',as.character(i),'.csv', sep = '')
  } else {
    currentfile = paste(as.character(i),'.csv', sep = '')
  }
  currentfilepath = paste('sensing/phonelock_raw/phonelock_u', currentfile, sep = '')
  if(file.exists(currentfilepath)){
    print(currentfilepath)
    newfile = paste('sensing/phonelock_cleaned/phonelock_u',currentfile,sep='')
    newfilename = cleanData(currentfilepath)
    write_csv(newfilename, newfile)
  } else {
    print(paste('file does not exist:', currentfile))
  }
}
