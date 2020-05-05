#Katherine Korngiebel (kbk3czn)
#import required libraries 
library(RMySQL)
library(DBI)
library(tidyverse)
library(dplyr)
library(dbplyr)

##Import 
#add the name of each future dataframe to a list 
types = c("activity", "behavior", "excercise", "mood", "mood1", "mood2", "sleep", "social", "stress", "class")
for (type in types) {
  f_name = paste("C:/Users/Katie Korngiebel/Desktop/sys2022/", type, ".csv", sep="")
  assign(type, data.frame(read.csv(f_name, header=TRUE)))
} 

##Clean 
#Activity 
s_act = activity %>%
  #select columns 
  select(user=ï..user, relaxing, other_relaxing, working, other_working, resp_time) %>%
  mutate(resp_time = as.POSIXct(resp_time, origin="1970-01-01", tz="UTC")) %>%
  #seperate by user
  group_by(user) %>%
  #obtain representitive value for each column 
  summarize(relax = mean(relaxing, na.rm=TRUE),
            o_relax = mean(other_relaxing, na.rm=TRUE),
            work = mean(working, na.rm=TRUE),
            o_work = mean(other_working, na.rm=TRUE))
#remove null values 
s_act = s_act[complete.cases(s_act), ]

#Behavior
s_beh = behavior %>%
  select(user=ï..user, 2, 5, 3:4, 6:9, 11, 14) %>%
  #remove null values 
  subset(anxious != "null" & calm != "null" & conventional != "null" & critical != "null" & dependable != "null" &
        disorganized != "null" & enthusiastic != "null" & experiences != "null" & reserved != "null" & sympathetic != "null")
#convert character columns to numeric 
s_beh$calm = as.numeric(as.character(s_beh$calm))
s_beh$conventional = as.numeric(as.character(s_beh$conventional))
s_beh$dependable = as.numeric(as.character(s_beh$dependable))
s_beh$disorganized = as.numeric(as.character(s_beh$disorganized))
s_beh$enthusiastic = as.numeric(as.character(s_beh$enthusiastic))
s_beh$experiences = as.numeric(as.character(s_beh$experiences))
s_beh$reserved = as.numeric(as.character(s_beh$reserved))
s_beh$sympathetic = as.numeric(as.character(s_beh$sympathetic))
#group by user and obtain summary value 
s_beh = s_beh %>%
  group_by(user) %>%
  summarize_all(mean)

#Excercise
s_exc = excercise %>%
  select(user=ï..user, exercise, walk) %>%
  group_by(user) %>%
  summarise_all(mean, na.rm=TRUE)

#Mood
s_mood = mood %>%
  select(user=ï..user, happy, sad) %>%
  subset(happy != "null" & sad != "null") %>%
  group_by(user) %>%
  summarise_all(mean, na.rm=TRUE)

#function to determine the Mode of a dataset
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Mood1
s_mood1 = mood1 %>%
  select(user=ï..user, tmr_mood = tomorrow) %>%
  group_by(user) %>%
  summarise(tmr_mood = Mode(tmr_mood))
#remove null values 
s_mood1 = s_mood1[complete.cases(s_mood1), ]

#Mood2
s_mood2 = mood2 %>%
  select(user=ï..user, how_feel = how) %>%
  group_by(user) %>%
  summarise(how_feel = Mode(how_feel))
s_mood2 = s_mood2[complete.cases(s_mood2), ]

#Sleep
s_sleep = sleep %>%
  select(user=ï..user, hr_sleep = hour, qual_sleep=rate, awake=social) %>%
  group_by(user) %>%
  summarize_all(mean, na.rm=TRUE)

#Social
s_soc = social %>%
  select(user=ï..user, num_interact = number) %>%
  group_by(user) %>%
  summarize(num_interact = mean(num_interact, na.rm=TRUE))

#Stress
s_stress = stress %>%
  select(user=ï..user, stress = level) %>%
  group_by(user) %>%
  summarize(stress = mean(stress, na.rm=TRUE))

#Class
s_class = class %>%
  select(user=ï..user, due, experience, hours) %>%
  group_by(user) %>%
  #normalize due value to between 0 and 1 
  summarize(due = mean(due, na.rm=TRUE) - 1,
            experience = mean(experience, na.rm=TRUE),
            hours = mean(hours, na.rm=TRUE))
s_class = s_class[complete.cases(s_class), ]


#combine factors into one final dataframe
dataset = Reduce(function(x, y) merge (x=x, y=y, by = "user"), list(s_act, s_beh, s_exc, s_mood, s_mood1, s_mood2, 
                                                                     s_sleep, s_soc, s_stress, s_class))
#select columns for machine learning model 
dataset = select(dataset, work, o_work, anxious, disorganized, experiences, exercise, happy, sad, 
                 tmr_mood, how_feel, hr_sleep, awake, qual_sleep, due, hours, experience, stress)

##Analyze
# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$stress, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = stress ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

library(MASS)
step.model <- stepAIC(regressor, direction = "both", 
                      trace = FALSE)
summary(step.model)

forward.model <- stepAIC(regressor, direction = "forward", 
                         trace = FALSE)
summary(forward.model)

backward.model <- stepAIC(regressor, direction = "backward", 
                          trace = FALSE)
summary(backward.model)

summary(step.model)$adj.r.squared
summary(forward.model)$adj.r.squared
summary(backward.model)$adj.r.squared

#obtain assumption plots 
plot(step.model,1)
plot(density(resid(step.model)))
qqnorm(resid(step.model)) 
qqline(resid(step.model))
plot(step.model,3)






