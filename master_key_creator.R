# Script created by Matt Thompson (mlt2we) 

#Load all necessary packages
library(gcookbook)
library(tidyverse)
library(farver)
library(dplyr, quietly = T)
library(anytime)
library(lubridate)

#create masterkey
masterkey <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c('user'))

#create a row for each user
for (i in 0:59){
  masterkey <- add_row(masterkey)
  if (i < 10) {
    masterkey$user[i+1] <- paste('u0',as.character(i),sep='')
  } else {
    masterkey$user[i+1] <- paste('u',as.character(i),sep='')
  }
}

#columns for activity data
masterkey$wknd_stationary_mean <- c(NA) 
masterkey$wknd_walking_mean <- c(NA) 
masterkey$wknd_running_mean <- c(NA) 
masterkey$wkdy_stationary_mean <- c(NA) 
masterkey$wkdy_walking_mean <- c(NA) 
masterkey$wkdy_running_mean <- c(NA) 

#function that can be called to create all the data points for that user for activity data
enterActivity <- function(masterkey, filename, ind) {
  activity <- read_csv(filename)
  
  masterkey$wknd_stationary_mean[ind+1] <- mean(filter(activity, wday(datetime) == 1 | wday(datetime) == 7)$proportion_stationary)
  masterkey$wknd_walking_mean[ind+1] <- mean(filter(activity, wday(datetime) == 1 | wday(datetime) == 7)$proportion_walking)
  masterkey$wknd_running_mean[ind+1] <- mean(filter(activity, wday(datetime) == 1 | wday(datetime) == 7)$proportion_running)
  
  masterkey$wkdy_stationary_mean[ind+1] <- mean(filter(activity, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | 
                                                       wday(datetime) == 5 | wday(datetime) == 6)$proportion_stationary)
  masterkey$wkdy_walking_mean[ind+1] <- mean(filter(activity, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | 
                                                    wday(datetime) == 5 | wday(datetime) == 6)$proportion_walking)
  masterkey$wkdy_running_mean[ind+1] <- mean(filter(activity, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | 
                                                    wday(datetime) == 5 | wday(datetime) == 6)$proportion_running)
  return(masterkey)
}

#iterate to add activity ratings for all users
for(i in 0:59){
  if(i<10){
    currentfile = paste('0',as.character(i),'.csv', sep = '')
  } else {
    currentfile = paste(as.character(i),'.csv', sep = '')
  }
  currentfilepath = paste('sensing/activity_cleaned/activity_u', currentfile, sep = '')
  if(file.exists(currentfilepath)){
    masterkey <- enterActivity(masterkey, currentfilepath, i)
  } else {
    print(paste('file does not exist:', currentfile))
  }
}

#new columns for conversation data

masterkey$wknd_num_conversations <- c(NA)
masterkey$wknd_length_conversations <- c(NA)
masterkey$wknd_num_conversations_avg <- c(NA)
masterkey$wknd_length_conversations_avg <- c(NA)

masterkey$wkdy_num_conversations <- c(NA)
masterkey$wkdy_length_conversations <- c(NA)
masterkey$wkdy_num_conversations_avg <- c(NA)
masterkey$wkdy_length_conversations_avg <- c(NA)

#function for ranking conversation data
enterConversations <- function(masterkey, filename, ind) {
  conversations <- read_csv(filename)
  
  masterkey$wknd_num_conversations[ind+1] <- sum(filter(conversations, wday(datetime) == 1 | wday(datetime) == 7)$num_conversations, na.rm = TRUE)
  masterkey$wknd_length_conversations[ind+1] <- sum(filter(conversations, wday(datetime) == 1 | wday(datetime) == 7)$num_conversations*filter(conversations, wday(datetime) == 1 | wday(datetime) == 7)$avg_conversation_length, na.rm = TRUE)
  masterkey$wknd_num_conversations_avg[ind+1] <- mean(filter(conversations, wday(datetime) == 1 | wday(datetime) == 7)$num_conversations, na.rm = TRUE)
  masterkey$wknd_length_conversations_avg[ind+1] <- masterkey$wknd_length_conversations[ind+1]/masterkey$wknd_num_conversations[ind+1]
  
  masterkey$wkdy_num_conversations[ind+1] <- sum(filter(conversations, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | wday(datetime) == 5 | wday(datetime) == 6)$num_conversations, na.rm = TRUE)
  masterkey$wkdy_length_conversations[ind+1] <- sum(filter(conversations, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | wday(datetime) == 5 | wday(datetime) == 6)$num_conversations)*mean(filter(conversations, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | wday(datetime) == 5 | wday(datetime) == 6)$avg_conversation_length, na.rm = TRUE)
  masterkey$wkdy_num_conversations_avg[ind+1] <- mean(filter(conversations, wday(datetime) == 2 | wday(datetime) == 3 | wday(datetime) == 4 | wday(datetime) == 5 | wday(datetime) == 6)$num_conversations, na.rm = TRUE)
  masterkey$wkdy_length_conversations_avg[ind+1] <- masterkey$wkdy_length_conversations[ind+1]/masterkey$wkdy_num_conversations[ind+1]
  
  return(masterkey)
}

#iterate to create conversation data for each user
for(i in 0:59){
  if(i<10){
    currentfile = paste('0',as.character(i),'.csv', sep = '')
  } else {
    currentfile = paste(as.character(i),'.csv', sep = '')
  }
  currentfilepath = paste('sensing/conversation_cleaned/conversation_u', currentfile, sep = '')
  if(file.exists(currentfilepath)){
    masterkey <- enterConversations(masterkey, currentfilepath, i)
  } else {
    print(paste('file does not exist:', currentfile))
  }
}

#create columns for phone lock data
masterkey$lock_time_avg <- c(NA)


#function that creates rating for an inputted user's conversations data
enterPhonelock <- function(masterkey, filename, ind) {
  phonelock <- read_csv(filename)
  masterkey$lock_time_avg[ind] <- mean(phonelock$total_lock_time, na.rm = TRUE)
  return(masterkey)
}

#iterate to rank converation data for all users
for(i in 0:59){
  if(i<10){
    user = paste('u0',as.character(i), sep = '')
    currentfile = paste('0',as.character(i),'.csv', sep = '')
  } else {
    user = paste('u',as.character(i), sep = '')
    currentfile = paste(as.character(i),'.csv', sep = '')
  }
  currentfilepath = paste('sensing/phonelock_cleaned/phonelock_u', currentfile, sep = '')
  if(file.exists(currentfilepath)){
    if(user %in% masterkey$user) {
      for (i in 1:42) {
        if(user == masterkey$user[i]) {
          masterkey <- enterPhonelock(masterkey, currentfilepath, i)
        }
      }
    }
  } else {
    print(paste('file does not exist:', currentfile))
  }
}

#working with deadline data; dividing data points by 10 in order to have number of assign how many 
# deadlines students have per week (homework deadlines, projects, quiz, mid-terms and finals). 
#Data was collected over 10 weeks.

deadlines = read_csv('education/CleanedDeadlinesData.csv')
masterkey$deadlines_per_week <- c(NA)

for (i in 1:length(deadlines$uid)){
  user = deadlines$uid[i]
  deadlines_week = deadlines$total[i]/10
    
  j = i
  while (masterkey$user[j] != user) {
    j = j + 1
  }
  
  masterkey$deadlines_per_week[j] <- deadlines_week
}

# adding raw gpa data to the master key (no need to extract any features from this since GPA on a 4.0 scale is standard)

gpa = read_csv('education/CleanedGradesData.csv')
masterkey$gpa <- c(NA)

for (i in 1:length(gpa$uid)){
  user = gpa$uid[i]
  gradepoint = gpa$gpa[i]
  
  j = i
  while (masterkey$user[j] != user) {
    j = j + 1
  }
  
  masterkey$gpa[j] <- gradepoint
}

# adding Piazza contributions per week to the master key 

contributions = read_csv('education/CleanedPiazzaData.csv')
masterkey$piazza_contributions_per_week <- c(NA)

for (i in 1:length(contributions$uid)){
  user = contributions$uid[i]
  contributions_week = contributions$contributions[i]/10
  
  j = i
  while (masterkey$user[j] != user) {
    j = j + 1
  }
  
  masterkey$piazza_contributions_per_week[j] <- contributions_week
}

#adding social data (how many people they talk to per day)

social  = read_csv('EMA/social.csv')
masterkey$people_conversed_with_day = c(NA)

for (i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  if (uid %in% social$user & !is.nan(mean(filter(social, user == uid)$number, na.rm = TRUE))){
    masterkey$people_conversed_with_day[i+1] <- mean(filter(social, user == uid)$number, na.rm = TRUE, )
  }
}


#adding sleep data (hours of sleep per night, quality of sleep, sleeping in class)

sleep = read_csv('EMA/sleep.csv')
masterkey$sleep_night <- c(NA)
masterkey$quality_sleep <- c(NA)
masterkey$awakeness_in_class <- c(NA)

for (i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  if (uid %in% sleep$user){
    if(!is.nan(mean(filter(sleep, user == uid)$hour, na.rm = TRUE))){
      masterkey$sleep_night[i+1] <- mean(filter(sleep, user == uid)$hour, na.rm = TRUE)
    }
    if(!is.nan(mean(filter(sleep, user == uid)$rate, na.rm = TRUE))){
      masterkey$quality_sleep[i+1] <- mean(filter(sleep, user == uid)$rate, na.rm = TRUE)
    }
    if(!is.nan(mean(filter(sleep, user == uid)$social, na.rm = TRUE))){
      masterkey$awakeness_in_class[i+1] <- mean(filter(sleep, user == uid)$social, na.rm = TRUE)
    }
  }
}

# adding activity data (working and relaxing alone vs working and relaxing with others)
activity = read_csv('EMA/activity.csv')
masterkey$relax_alone <- c(NA)
masterkey$work_alone <- c(NA)
masterkey$relax_with_others <- c(NA)
masterkey$work_with_others <- c(NA)

for (i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  
  if (uid %in% activity$user) {
    if (!is.nan(mean(filter(activity, user == uid)$relaxing, na.rm = TRUE))){
      masterkey$relax_alone[i+1] <- mean(filter(activity, user == uid)$relaxing, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(activity, user == uid)$working, na.rm = TRUE))){
      masterkey$work_alone[i+1] <- mean(filter(activity, user == uid)$working, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(activity, user == uid)$other_relaxing, na.rm = TRUE))){
      masterkey$relax_with_others[i+1] <- mean(filter(activity, user == uid)$other_relaxing, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(activity, user == uid)$other_working, na.rm = TRUE))){
      masterkey$work_with_others[i+1] <- mean(filter(activity, user == uid)$other_working, na.rm = TRUE)
    }
  }
}

# adding the rest of Katie's EMA data

behavior = read_csv('EMA/behavior.csv')

for (i in 1:length(behavior$user)) {
  for (j in 2:14) {
    if (!is.na(behavior[[i,j]])){
      if (behavior[[i,j]] == 'null') {
        behavior[[i,j]] <- NA
      }
    }
  }
}

behavior$calm <- as.numeric(behavior$calm)
behavior$conventional <- as.numeric(behavior$conventional)
behavior$dependable <- as.numeric(behavior$dependable)
behavior$disorganized <- as.numeric(behavior$disorganized)
behavior$enthusiastic <- as.numeric(behavior$enthusiastic)
behavior$experiences <- as.numeric(behavior$experiences)
behavior$reserved <- as.numeric(behavior$reserved)
behavior$sympathetic <- as.numeric(behavior$sympathetic)

# behavior EMA data
masterkey$enthusiastic_extrovert_15min <- c(NA)
masterkey$critial_quarrelsome_15min <- c(NA)
masterkey$dependable_selfDisciplined_15min <- c(NA)
masterkey$anxious_easilyUpset_15min <- c(NA)
masterkey$openToNewExperiences_complex_15min <- c(NA)
masterkey$reserved_quiet_15min <- c(NA)
masterkey$sympathetic_warm_15min <- c(NA)
masterkey$disorganized_careless_15min <- c(NA)
masterkey$calm_emotionallyStable_15min <- c(NA)
masterkey$convential_uncreative_15min <- c(NA)

for(i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  print(uid)
  if (uid %in% behavior$user) {
    if (!is.nan(mean(filter(behavior, user == uid)$enthusiastic, na.rm = TRUE))) {
      masterkey$enthusiastic_extrovert_15min[i+1] <- mean(filter(behavior, user == uid)$enthusiastic, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$critical, na.rm = TRUE))) {
      masterkey$critial_quarrelsome_15min[i+1] <- mean(filter(behavior, user == uid)$critical, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$dependable, na.rm = TRUE))) {
      masterkey$dependable_selfDisciplined_15min[i+1] <- mean(filter(behavior, user == uid)$dependable, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$anxious, na.rm = TRUE))) {
      masterkey$anxious_easilyUpset_15min[i+1] <- mean(filter(behavior, user == uid)$anxious, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$experiences, na.rm = TRUE))) {
      masterkey$openToNewExperiences_complex_15min[i+1] <- mean(filter(behavior, user == uid)$experiences, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$reserved, na.rm = TRUE))) {
      masterkey$reserved_quiet_15min[i+1] <- mean(filter(behavior, user == uid)$reserved, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$sympathetic, na.rm = TRUE))) {
      masterkey$sympathetic_warm_15min[i+1] <- mean(filter(behavior, user == uid)$sympathetic, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$disorganized, na.rm = TRUE))) {
      masterkey$disorganized_careless_15min[i+1] <- mean(filter(behavior, user == uid)$disorganized, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$calm, na.rm = TRUE))) {
      masterkey$calm_emotionallyStable_15min[i+1] <- mean(filter(behavior, user == uid)$calm, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(behavior, user == uid)$conventional, na.rm = TRUE))) {
      masterkey$convential_uncreative_15min[i+1] <- mean(filter(behavior, user == uid)$conventional, na.rm = TRUE)
    }
  }
}

mean(filter(behavior, user == 'u07')$experiences, na.rm = TRUE)

# adding EMA exercise data

exercise <- read_csv('EMA/excercise.csv')

masterkey$exercise <- c(NA)
masterkey$walk <- c(NA)

for(i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  
  if (uid %in% exercise$user) {
    if (!is.nan(mean(filter(exercise, user == uid)$exercise, na.rm = TRUE))) {
      masterkey$exercise[i+1] <- mean(filter(exercise, user == uid)$exercise, na.rm = TRUE)
    }
    if (!is.nan(mean(filter(exercise, user == uid)$walk, na.rm = TRUE))) {
      masterkey$walk[i+1] <- mean(filter(exercise, user == uid)$walk, na.rm = TRUE)
    }
  }
}

# adding stress levels
stress <- read_csv('EMA/stress.csv')
masterkey$stress <- c(NA)

for(i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  
  if (uid %in% stress$user) {
    if (!is.nan(mean(filter(stress, user == uid)$level, na.rm = TRUE))) {
      masterkey$stress[i+1] <- mean(filter(stress, user == uid)$level, na.rm = TRUE)
    }
  }
}

#add self-worth
masterkey_selfworth <- data.frame(masterkey)
masterkey_selfworth$stress <- NULL
masterkey_selfworth$selfWorth <- c(NA)

mental = read_csv('self_worth_score.csv')

for(i in 0:59) {
  if (i < 10) {
    uid = paste('u0',i, sep='')
  } else {
    uid = paste('u',i, sep='')
  }
  
  if (uid %in% mental$user) {
    masterkey_selfworth$selfWorth[i+1] <- filter(mental, user == uid)$score
  }
}

# cleaning out NA values from masterkey

masterkey <- filter(masterkey, !is.na(stress))
masterkey$gpa <- NULL
masterkey <- filter(masterkey, user != 'u05', user != 'u09', user != 'u34', user != 'u41', user != 'u42', user != 'u47')

# fill in NA deadlines data w/ average of column
avg = mean(masterkey$deadlines_per_week, na.rm = TRUE)
for(i in 1:length(masterkey$user)) {
  if (is.na(masterkey$deadlines_per_week[i])) {
    masterkey$deadlines_per_week[i] <- avg
  }
}

#phone locked replacing NaN values w/ mean for u39
masterkey$wknd_num_locked[28] <- NA
masterkey$wknd_lock_time_avg[28] <- NA
masterkey$wknd_lock_time[28] <- NA
masterkey$wknd_num_locked_avg[28] <- NA

mean_wknd_num_locked = mean(masterkey$wknd_num_locked, na.rm = TRUE)
mean_wknd_lock_time_avg = mean(masterkey$wknd_lock_time_avg, na.rm = TRUE)
mean_wknd_lock_time = mean(masterkey$wknd_lock_time, na.rm = TRUE)
mean_wknd_num_locked_avg = mean(masterkey$wknd_num_locked_avg, na.rm = TRUE)

masterkey$wknd_num_locked[28] <- mean_wknd_num_locked
masterkey$wknd_lock_time_avg[28] <- mean_wknd_lock_time_avg
masterkey$wknd_lock_time[28] <- mean_wknd_lock_time
masterkey$wknd_num_locked_avg[28] <- mean_wknd_num_locked_avg

# delete extraneous columns
masterkey$wkdy_stationary_mean <- NULL
masterkey$wknd_stationary_mean<- NULL
masterkey$wknd_length_conversations<- NULL
masterkey$wknd_num_conversations<- NULL
masterkey$wkdy_length_conversations_avg<- NULL
masterkey$wkdy_length_conversations<- NULL
masterkey$wkdy_num_conversations<- NULL
masterkey$wkdy_length_conversations_avg<- NULL

write_csv(masterkey, 'masterkey.csv')

# cleaning out NA values from masterkey_selfworth
masterkey_selfworth <- filter(masterkey_selfworth, !is.na(selfWorth))
masterkey_selfworth$gpa <- NULL
masterkey_selfworth <- filter(masterkey_selfworth, user != 'u05', user != 'u09', user != 'u34', user != 'u41', user != 'u42', user != 'u47')

# fill in NA deadlines data w/ average of column
avg = mean(masterkey_selfworth$deadlines_per_week, na.rm = TRUE)
for(i in 1:length(masterkey_selfworth$user)) {
  if (is.na(masterkey_selfworth$deadlines_per_week[i])) {
    masterkey_selfworth$deadlines_per_week[i] <- avg
  }
}

# delete extraneous columns
masterkey_selfworth$wkdy_stationary_mean <- NULL
masterkey_selfworth$wknd_stationary_mean<- NULL
masterkey_selfworth$wknd_length_conversations<- NULL
masterkey_selfworth$wknd_num_conversations<- NULL
masterkey_selfworth$wkdy_length_conversations_avg<- NULL
masterkey_selfworth$wkdy_length_conversations<- NULL
masterkey_selfworth$wkdy_num_conversations<- NULL
masterkey_selfworth$wkdy_length_conversations_avg<- NULL
masterkey_selfworth <- masterkey_selfworth[,-9:-16]

masterkey_selfworth$lock_time_avg <- c(NA)
#iterate to rank converation data for all users for self worth data
for(i in 0:59){
  if(i<10){
    user = paste('u0',as.character(i), sep = '')
    currentfile = paste('0',as.character(i),'.csv', sep = '')
  } else {
    user = paste('u',as.character(i), sep = '')
    currentfile = paste(as.character(i),'.csv', sep = '')
  }
  currentfilepath = paste('sensing/phonelock_cleaned/phonelock_u', currentfile, sep = '')
  if(file.exists(currentfilepath)){
    if(user %in% masterkey_selfworth$user) {
      for (i in 1:35) {
        if(user == masterkey_selfworth$user[i]) {
          masterkey_selfworth <- enterPhonelock(masterkey_selfworth, currentfilepath, i)
        }
      }
    }
  } else {
    print(paste('file does not exist:', currentfile))
  }
}

#create and plot self-worth and stress levels
mentalStress <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('user', 'stress','selfWorth'))
for (i in 0:59) {
  if(i<10){
    uid = paste('u0',as.character(i), sep = '')
  } else {
    uid = paste('u',as.character(i), sep = '')
  }
  mentalStress <- add_row(mentalStress)
  mentalStress$user[i+1] <- uid
  if(length(filter(masterkey, user == uid)$stress != 0)){
    if(mentalStress$user[i+1] == uid) {
      mentalStress$stress[i+1] <- filter(masterkey, user == uid)$stress
    }
  }
  if(length(filter(masterkey_selfworth, user == uid)$selfWorth != 0)){
    if(mentalStress$user[i+1] == uid) {
      mentalStress$selfWorth[i+1] <- filter(masterkey_selfworth, user == uid)$selfWorth
    }
  }
}

mentalStress <- na.omit(mentalStress)

plot(mentalStress$stress, mentalStress$selfWorth)


mean(masterkey$stress)
sd(masterkey$stress)























