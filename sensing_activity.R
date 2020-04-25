# Script created by Matt Thompson (mlt2we)

#Load all necessary packages
library(gcookbook)
library(tidyverse)
library(farver)
library(dplyr, quietly = T)
library(anytime)
library(lubridate)

#returns a cleaned dataframe binned by day and proportion of day spent stationary, walking, or running
cleanData <- function(currentfile) {
  d = read_csv(currentfile)
  #convert epoch into 'human time' and creates attributes month, day, hour, minute
  d =  mutate(d, datetime = as.POSIXct(anytime(timestamp), format="%Y-%m-%d%H:%M:%OS"))
  d =  mutate(d, month = month(d$datetime))
  d =  mutate(d, day = day(d$datetime))
  
  # bin data
  
  #copy dataframe and delete all rows 
  dbinned <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('datetime','proportion_stationary', 'proportion_walking', 'proportion_running'))
  
  #these parameters will help with iteration that will bin the data
  activity_list = c(NULL)
  activity_index = 1
  day = d$day[1]
  binned_index = 0
  
  #iterate through all rows of original dataframe
  for (i in 1:length(d$`activity inference`)){
    
    currentday <- day
    
    #reset bin
    day = d$day[i]
    
    #if in new bin
    if(currentday != day | i == length(d$`activity inference`)) {
      if(length(activity_list)!=0) {
        
        #add all new data within bin to new dataframe
        dbinned <- add_row(dbinned)
        binned_index <- binned_index + 1
        dbinned$datetime[binned_index] <- as.POSIXct(substring(as.character(d$datetime[i-1]),1,10), format="%Y-%m-%d")
        dbinned$proportion_stationary[binned_index] <- round(sum(activity_list == 0)/length(activity_list), digits = 3)
        dbinned$proportion_walking[binned_index] <- round(sum(activity_list == 1)/length(activity_list), digits = 3)
        dbinned$proportion_running[binned_index] <- round(sum(activity_list == 2)/length(activity_list), digits = 3)
      }
      
      #reset bin parameters
      activity_list = c(NULL)
      activity_index = 1
    } else {
      #add to running list of activities in that bin if not 3, which is unknown
      if(d$`activity inference`[i] != 3) {
        activity_list[activity_index] <- d$`activity inference`[i]
        activity_index = activity_index + 1
      }
    }
  }
  dbinned <- mutate(dbinned, datetime = as.POSIXct(anytime(datetime), format="%Y-%m-%d"))
  
  return(dbinned)
}

#iterate through each user data and clean them using function above
for(i in 0:59){
  if(i<10){
    currentfile = paste('0',as.character(i),'.csv', sep = '')
  } else {
    currentfile = paste(as.character(i),'.csv', sep = '')
  }
  currentfilepath = paste('sensing/activity_raw/activity_u', currentfile, sep = '')
  if(file.exists(currentfilepath)){
    print(currentfilepath)
    newfile = paste('sensing/activity_cleaned/activity_u',currentfile,sep='')
    newfilename = cleanData(currentfilepath)
    write_csv(newfilename, newfile)
  } else {
    print(paste('file does not exist:', currentfile))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~

#original binning data that binned mode activity by 30 minutes

# #Gets mode from inputted vector (from https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# #this is a standardized function that when a an activity data csv name is inputted, 
# #it returns a csv that is binned by 30 minute intervals
# cleanData <- function(currentfile) {
#   d = read_csv(currentfile)
#   #convert epoch into 'human time' and creates attributes month, day, hour, minute
#   d =  mutate(d, datetime = as.POSIXct(anytime(timestamp), format="%Y-%m-%d%H:%M:%OS"))
#   d =  mutate(d, month = month(d$datetime))
#   d =  mutate(d, day = day(d$datetime))
#   d =  mutate(d, hour = hour(d$datetime))
#   d =  mutate(d, minute = minute(d$datetime))
#   
#   # bin data
#   
#   #copy dataframe and delete all rows 
#   dbinned <- data.frame(d)
#   dbinned <- dbinned[-1:-length(dbinned$datetime),]
#   dbinned$timestamp <- NULL
#   
#   #these parameters will help with iteration that will bin the data
#   activity_list = c(NULL)
#   activity_index = 1
#   month = d$month[1]
#   day = d$day[1]
#   hour = d$hour[1]
#   minute = d$minute[1]
#   
#   #separate bin into below and above 30 minutes
#   if(minute < 30){
#     bin = 1
#   } else {
#     bin = 2
#   }
#   binned_index = 0
#   
#   #iterate through all rows of original dataframe
#   for (i in 1:length(d$`activity inference`)){
#     currentbin <- bin
#     
#     #reset bin
#     minute = d$minute[i]
#     if(minute < 30){
#       bin = 1
#     } else {
#       bin = 2
#     }
#     
#     #if in new bin
#     if(currentbin != bin | hour != d$hour[i] | day != d$day[i] | month != d$month[i]) {
#       if(!is.null(getmode(activity_list))) {
#         #add all new data within bin to new dataframe
#         dbinned <- add_row(dbinned)
#         binned_index <- binned_index + 1
#         dbinned$month[binned_index] <- month
#         dbinned$day[binned_index] <- day
#         dbinned$hour[binned_index] <- hour
#         if(currentbin == 1){
#           dbinned$minute[binned_index] <- 0
#         } else {
#           dbinned$minute[binned_index] <- 30
#         }
#         dbinned$activity.inference[binned_index] <- getmode(activity_list)
#         
#         if (dbinned$day[binned_index]<10){
#           day = paste('0', as.character(dbinned$day[binned_index]), sep = '')
#         } else {
#           day = as.character(dbinned$day[binned_index])
#         }
#         if (dbinned$hour[binned_index]<10){
#           hour = paste('0', as.character(dbinned$hour[binned_index]), sep = '')
#         } else {
#           hour = as.character(dbinned$hour[binned_index])
#         }
#         if (dbinned$minute[binned_index] == 0){
#           minute = '00'
#         } else {
#           minute = '30'
#         }
#         month = paste('0',as.character(dbinned$month[binned_index]), sep = '')
#         dbinned$datetime[binned_index] <- as.POSIXct(paste('2013-',month,'-',day,' ',hour,':',minute,':','01', sep = ''))
#       }
#       
#       #reset bin parameters
#       activity_list = c(NULL)
#       activity_index = 1
#       month = d$month[i]
#       day = d$day[i]
#       hour = d$hour[i]
#     } else {
#       #add to running list of activities in that bin if not 3, which is unknown
#       if(d$`activity inference`[i] != 3) {
#         activity_list[activity_index] <- d$`activity inference`[i]
#         activity_index = activity_index + 1
#       }
#     }
#   }
#   
#   #clean new dataframe
#   dbinned$month <- NULL
#   dbinned$day <- NULL
#   dbinned$hour <- NULL
#   dbinned$minute <- NULL
#   dbinned <- dbinned[c(2,1)]
#   
#   return(dbinned)
# }

