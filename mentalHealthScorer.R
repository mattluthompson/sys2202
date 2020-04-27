# Script created by Matt Thompson (mlt2we) 

mental <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('user','sum','num','score'))

for (i in 0:59){
  mental <- add_row(mental)
  if (i < 10) {
    mental$user[i+1] <- paste('u0',as.character(i),sep='')
  } else {
    mental$user[i+1] <- paste('u',as.character(i),sep='')
  }
}



bigFive <- read_csv('Survey/BigFive2.csv')

bigFive <- filter(bigFive, type == 'post')


for (i in 1:length(bigFive$X1)) {
  for (j in 4:47) {
    print(paste(as.character(i), as.character(j), sep=','))
    if(!is.na(bigFive[[i,j]])) {
      if (bigFive[[i,j]]==0){
        bigFive[[i,j]] = -2
      } else if (bigFive[[i,j]]==1) {
        bigFive[[i,j]] = -1
      } else if (bigFive[[i,j]]==2) {
        bigFive[[i,j]] = 0
      } else if (bigFive[[i,j]]==3) {
        bigFive[[i,j]] = 1
      } else {
        bigFive[[i,j]] = 2
      }
    }
  }
}

bigFive$`I am Talkative` <- NULL
bigFive$`I Find Faults in Others` <- NULL
bigFive$`I do a thorough Job` <- bigFive$`I do a thorough Job`*-1
bigFive$`I am Original` <- bigFive$`I am Original`*-1
bigFive$`I am Reserved` <- NULL
bigFive$`I am Helpful and Unselfish` <- bigFive$`I am Helpful and Unselfish`*-1
bigFive$`I am Relaxed` <- NULL
bigFive$`I am Curious` <- NULL
bigFive$`I am Full of Energy` <- bigFive$`I am Full of Energy`*-1
bigFive$`I am Depressed` <- bigFive$`I am Depressed`*3
bigFive$`I am Reliable` <- bigFive$`I am Reliable`*-1
bigFive$`I am Tense` <- NULL
bigFive$`I am a Deep Thinker` <- bigFive$`I am a Deep Thinker`*-1
bigFive$`I Generate Enthusiasm` <- bigFive$`I Generate Enthusiasm`*-1
bigFive$`I am Forgiving` <- NULL
bigFive$`I Worry A lot` <- NULL
bigFive$`I Have an Active Imagination` <- NULL
bigFive$`I am Quiet` <- NULL
bigFive$`I am Trusting` <- bigFive$`I am Trusting`*-1
bigFive$`I am Emotionally Stable` <- bigFive$`I am Emotionally Stable`*-1
bigFive$`I am Inventive` <- NULL
bigFive$`I am Assertive` <- bigFive$`I am Assertive`*-1
bigFive$`I Persevere Until Tasks are Finished` <- bigFive$`I Persevere Until Tasks are Finished`*-1
bigFive$`I am Moody` <- NULL
bigFive$`I Value Aesthetics` <- NULL
bigFive$`I am Reserved_1` <- NULL
bigFive$`I am Considerate` <- bigFive$`I am Considerate`*-1
bigFive$`I do Things Efficiently` <- NULL
bigFive$`I Remain Calm in Tense Situations` <- NULL
bigFive$`I Prefer Routine Work` <- NULL
bigFive$`I am Sociable` <- bigFive$`I am Sociable`*-1
bigFive$`I Make Plans and Follow Through` <- bigFive$`I Make Plans and Follow Through`*-1
bigFive$`I get Nervous Easily` <- NULL
bigFive$`I like to Reflect and Play with Ideas` <- NULL
bigFive$`I Have Few Artistic Interests` <- NULL
bigFive$`I Like to Cooperate with Others` <- NULL
bigFive$`I get Distracted Easily` <- NULL
bigFive$`I am Sophisticated in the Arts` <- NULL
bigFive$`I am Careless` <- NULL
bigFive$X1 <- NULL
bigFive$type <- NULL

bigFive$score <- c(NA)

for (i in 1:length(bigFive$uid)) {
  userRunningTotal = 0
  for (j in 2:21) {
    userRunningTotal = userRunningTotal + bigFive[[i,j]]
  }
  bigFive[[i, 22]] <- userRunningTotal
}

for (i in 0:59) {
  if (i < 10) {
    id = paste('u0',i, sep='')
  } else {
    id = paste('u',i, sep='')
  }
  if(id %in% bigFive$uid) {
    if(is.na(mental$sum[i+1])) {
      mental$sum[i+1] <- 0
      mental$num[i+1] <- 0
      mental$score[i+1] <- 0
    }
    mental$sum[i+1] <- mental$sum[i+1] + filter(bigFive, uid == id)$score 
    mental$num[i+1] <- mental$num[i+1] + 20
    mental$score[i+1] <- mental$sum[i+1]/mental$num[i+1]
  }
}

flourishing <- read_csv('Survey/FlourishingScale.csv')
flourishing <- filter(flourishing, type == 'post')
flourishing$type <- NULL

for (i in 1:length(flourishing$uid)) {
  for (j in 2:9) {
    if (flourishing[[i,j]] == 1) {
      flourishing[[i,j]] <- 2
    } else if (flourishing[[i,j]] == 2) {
      flourishing[[i,j]] <- 4/3
    } else if (flourishing[[i,j]] == 3) {
      flourishing[[i,j]] <- 2/3
    } else if (flourishing[[i,j]] == 4) {
      flourishing[[i,j]] <- 0
    } else if (flourishing[[i,j]] == 5) {
      flourishing[[i,j]] <- -2/3
    } else if (flourishing[[i,j]] == 6) {
      flourishing[[i,j]] <- -4/3
    } else {
      flourishing[[i,j]] <- -2
    }
  }
}

flourishing$score <- c(NA)
for(i in 1:length(flourishing$uid)) {
  score = 0
  for (j in 2:9) {
    score = score + flourishing[[i,j]]
  }
  flourishing$score[i] <- score
}

for (i in 0:59) {
  if (i < 10) {
    id = paste('u0',i, sep='')
  } else {
    id = paste('u',i, sep='')
  }
  if(id %in% flourishing$uid) {
    if(is.na(mental$sum[i+1])) {
      mental$sum[i+1] <- 0
      mental$num[i+1] <- 0
      mental$score[i+1] <- 0
    }
    mental$sum[i+1] <- mental$sum[i+1] + filter(flourishing, uid == id)$score 
    mental$num[i+1] <- mental$num[i+1] + 8
    mental$score[i+1] <- mental$sum[i+1]/mental$num[i+1]
  }
}

loneliness <- read_csv('Survey/LonelinessScale2.csv')
loneliness <- filter(loneliness, type == 'post')
loneliness$X1 <- NULL
loneliness$type <- NULL

for (i in 1:length(loneliness$uid)) {
  for (j in 2:21) {
    if(loneliness[[i,j]] == 0) {
      loneliness[[i,j]] <- -2
    } else if (loneliness[[i,j]] == 1) {
      loneliness[[i,j]] <- -1
    } else if (loneliness[[i,j]] == 2) {
      loneliness[[i,j]] <- 1
    } else {
      loneliness[[i,j]] <- 2
    }
  }
}

loneliness$`I am in Tune with Others` <- loneliness$`I am in Tune with Others`*-1
loneliness$`I am not Alone` <- loneliness$`I am not Alone`*-1
loneliness$`I Feel like I'm in a Group of Friends` <- loneliness$`I Feel like I'm in a Group of Friends`*-1
loneliness$`I have a lot in Common` <- loneliness$`I have a lot in Common`*-1
loneliness$`I am Outgoing` <- NULL
loneliness$`There are People I am Close to` <- loneliness$`There are People I am Close to`*-1
loneliness$`I Find Companionship when I want it` <- loneliness$`I Find Companionship when I want it`*-1
loneliness$`There are People who Understand Me` <- loneliness$`There are People who Understand Me`*-1
loneliness$`There are People I can Talk to` <- loneliness$`There are People I can Talk to`*-1
loneliness$`There are People I can Turn to` <- loneliness$`There are People I can Turn to`*-1

loneliness$score <- c(NA)

for (i in 1:length(loneliness$uid)) {
  score = 0
  for (j in 2:20) {
    score = score + loneliness[[i,j]]
  }
  loneliness$score[i] <- score
}

for (i in 0:59) {
  if (i < 10) {
    id = paste('u0',i, sep='')
  } else {
    id = paste('u',i, sep='')
  }
  if(id %in% loneliness$uid) {
    if(is.na(mental$sum[i+1])) {
      mental$sum[i+1] <- 0
      mental$num[i+1] <- 0
      mental$score[i+1] <- 0
    }
    mental$sum[i+1] <- mental$sum[i+1] + filter(loneliness, uid == id)$score 
    mental$num[i+1] <- mental$num[i+1] + 19
    mental$score[i+1] <- mental$sum[i+1]/mental$num[i+1]
  }
}

phq = read_csv('Survey/PHQ-92.csv')
phq <- filter(phq, type == 'post')
phq$X1 <- NULL
phq$type <- NULL

for (i in 1:length(phq$uid)) {
  for (j in 2:10) {
    if (phq[[i,j]] == 0) {
      phq[[i,j]] <- -2
    } else if (phq[[i,j]] == 1) {
      phq[[i,j]] <- -1
    } else if (phq[[i,j]] == 2) {
      phq[[i,j]] <- 1
    } else {
      phq[[i,j]] <- 2
    }
  }
}

phq$`Irregular or Unhealthy Sleep Habit` <- NULL
phq$`Having Little Energy` <- NULL
phq$`Trouble Concentrating on Things` <- NULL
phq$`Being Sluggish or Restless` <- NULL
phq$score <- c(NA)

for (i in 1:length(phq$uid)) {
  score = 0
  for (j in 2:6) {
    score = score + phq[[i,j]]
  }
  phq$score[i] <- score
}

for (i in 0:59) {
  if (i < 10) {
    id = paste('u0',i, sep='')
  } else {
    id = paste('u',i, sep='')
  }
  if(id %in% phq$uid) {
    if(is.na(mental$sum[i+1])) {
      mental$sum[i+1] <- 0
      mental$num[i+1] <- 0
      mental$score[i+1] <- 0
    }
    mental$sum[i+1] <- mental$sum[i+1] + filter(phq, uid == id)$score 
    mental$num[i+1] <- mental$num[i+1] + 5
    mental$score[i+1] <- mental$sum[i+1]/mental$num[i+1]
  }
}













