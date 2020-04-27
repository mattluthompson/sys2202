
UVA_data <- read.csv("/Users/maxstjohn/UVA_Data.csv")



UVA_data <- UVA_data[-c(1)]
UVA_data <- UVA_data[-c(1:52, 139:154, 167:172, 174:214, 221:250, 300:302, 312:318, 321)]
UVA_data <- UVA_data[-c(5:8, 11:41, 45:48, 55:75, 101:112, 115:124, 127:133, 137:146, 148:153)]
"
UVA_data_copy <- UVA_data
Key_name <- as.vector(UVA_Key$Name)
Key_label <- as.vector(UVA_Key$Label)
UVA_colnames <- colnames(UVA_data)

for (i in 1:365) {
  in_data = is.element(Key_name[i], UVA_colnames)
  if (in_data == TRUE) {
    names(UVA_data_copy)[names(UVA_data_copy) == Key_name[i]] <- Key_label[i]
  }
}
"
#write.csv(UVA_data_copy, "UVA_data_column_titles.csv")



#Combining columns to see overall perceived safety of students
UVA_data$NQ7A <- as.character(UVA_data$NQ7A)
UVA_data$NQ7B <- as.character(UVA_data$NQ7B)
UVA_data$NQ7C <- as.character(UVA_data$NQ7C)
UVA_data$NQ7D <- as.character(UVA_data$NQ7D)

UVA_data$NQ7A[UVA_data$NQ7A == "Very safe"] <- 4
UVA_data$NQ7B[UVA_data$NQ7B == "Very safe"] <- 4
UVA_data$NQ7C[UVA_data$NQ7C == "Very safe"] <- 4
UVA_data$NQ7D[UVA_data$NQ7D == "Very safe"] <- 4

UVA_data$NQ7A[UVA_data$NQ7A == "Somewhat safe"] <- 3
UVA_data$NQ7B[UVA_data$NQ7B == "Somewhat safe"] <- 3
UVA_data$NQ7C[UVA_data$NQ7C == "Somewhat safe"] <- 3
UVA_data$NQ7D[UVA_data$NQ7D == "Somewhat safe"] <- 3

UVA_data$NQ7A[UVA_data$NQ7A == "Somewhat unsafe"] <- 2
UVA_data$NQ7B[UVA_data$NQ7B == "Somewhat unsafe"] <- 2
UVA_data$NQ7C[UVA_data$NQ7C == "Somewhat unsafe"] <- 2
UVA_data$NQ7D[UVA_data$NQ7D == "Somewhat unsafe"] <- 2

UVA_data$NQ7A[UVA_data$NQ7A == "Not safe at all"] <- 1
UVA_data$NQ7B[UVA_data$NQ7B == "Not safe at all"] <- 1
UVA_data$NQ7C[UVA_data$NQ7C == "Not safe at all"] <- 1
UVA_data$NQ7D[UVA_data$NQ7D == "Not safe at all"] <- 1

UVA_data$NQ7A[is.na(UVA_data$NQ7A)] <- 1.5
UVA_data$NQ7B[is.na(UVA_data$NQ7B)] <- 1.5
UVA_data$NQ7C[is.na(UVA_data$NQ7C)] <- 1.5
UVA_data$NQ7D[is.na(UVA_data$NQ7D)] <- 1.5

UVA_data$Safe_Campus_Day <- as.numeric(UVA_data$NQ7A)
UVA_data$Safe_Campus_Night <- as.numeric(UVA_data$NQ7B)
UVA_data$Safe_Community_Day <- as.numeric(UVA_data$NQ7C)
UVA_data$Safe_Community_Night <- as.numeric(UVA_data$NQ7D)

#Finding average days a person drinks during month
UVA_data$NQ8A5 <- as.character(UVA_data$NQ8A5)

UVA_data$NQ8A5[UVA_data$NQ8A5 == "1-2 days"] <- 1.5
UVA_data$NQ8A5[UVA_data$NQ8A5 == "3-5 days"] <- 4
UVA_data$NQ8A5[UVA_data$NQ8A5 == "6-9 days"] <- 7.5
UVA_data$NQ8A5[UVA_data$NQ8A5 == "10-19 days"] <- 14.5
UVA_data$NQ8A5[UVA_data$NQ8A5 == "20-29 days"] <- 24.5
UVA_data$NQ8A5[UVA_data$NQ8A5 == "Used daily"] <- 30
UVA_data$NQ8A5[UVA_data$NQ8A5 == "Never used"] <- 0
UVA_data$NQ8A5[UVA_data$NQ8A5 == "Have used, but not in last 30 days"] <- 0
UVA_data$NQ8A5[is.na(UVA_data$NQ8A5)] <- 0

UVA_data$DrinkingAvg <- as.numeric(UVA_data$NQ8A5)

#Finding average days a person uses marijuana during month
UVA_data$NQ8A6 <- as.character(UVA_data$NQ8A6)

UVA_data$NQ8A6[UVA_data$NQ8A6 == "1-2 days"] <- 1.5
UVA_data$NQ8A6[UVA_data$NQ8A6 == "3-5 days"] <- 4
UVA_data$NQ8A6[UVA_data$NQ8A6 == "6-9 days"] <- 7.5
UVA_data$NQ8A6[UVA_data$NQ8A6 == "10-19 days"] <- 14.5
UVA_data$NQ8A6[UVA_data$NQ8A6 == "20-29 days"] <- 24.5
UVA_data$NQ8A6[UVA_data$NQ8A6 == "Used daily"] <- 30
UVA_data$NQ8A6[UVA_data$NQ8A6 == "Never used"] <- 0
UVA_data$NQ8A6[UVA_data$NQ8A6 == "Have used, but not in last 30 days"] <- 0
UVA_data$NQ8A6[is.na(UVA_data$NQ8A6)] <- 0

UVA_data$MarijuanaAvg <- UVA_data$NQ8A6

#Sex Data

#Removing NA's
UVA_data$NQ19[is.na(UVA_data$NQ19)] <- 0
UVA_data$RNQ20A[is.na(UVA_data$RNQ20A)] <- "No"
UVA_data$RNQ20B[is.na(UVA_data$RNQ20B)] <- "No"


#Number of partners over last year
UVA_data$NumberPartners <- as.numeric(UVA_data$NQ19)

#Determining if person had partner last 12 months
UVA_data$RNQ20A <- as.character(UVA_data$RNQ20A)
UVA_data$RNQ20B <- as.character(UVA_data$RNQ20B)
UVA_data$RNQ20A[UVA_data$RNQ20A == "Yes"] <- 1
UVA_data$RNQ20A[UVA_data$RNQ20A == "No"] <- 0
UVA_data$RNQ20B[UVA_data$RNQ20B == "Yes"] <- 1
UVA_data$RNQ20B[UVA_data$RNQ20B == "No"] <- 0

UVA_data$RNQ20A <- as.numeric(UVA_data$RNQ20A)
UVA_data$RNQ20B <- as.numeric(UVA_data$RNQ20B)
UVA_data$SexLast12Months <- UVA_data$RNQ20A + UVA_data$RNQ20B
UVA_data$SexLast12Months[UVA_data$SexLast12Months == 2] <- 1

#Determine if person had partner last 30 days
UVA_data$NQ21A <- as.character(UVA_data$NQ21A)
UVA_data$NQ21B <- as.character(UVA_data$NQ21B)
UVA_data$NQ21C <- as.character(UVA_data$NQ21C)


UVA_data$NQ21A[UVA_data$NQ21A == "No, have done this sexual activity in the past but not in the last 30 days"] <- 0
UVA_data$NQ21A[UVA_data$NQ21A == "No, have never done this sexual activity"] <- 0
UVA_data$NQ21A[UVA_data$NQ21A == "Yes"] <- 1
UVA_data$NQ21A[is.na(UVA_data$NQ21A)] <- 0

UVA_data$NQ21B[UVA_data$NQ21B == "No, have done this sexual activity in the past but not in the last 30 days"] <- 0
UVA_data$NQ21B[UVA_data$NQ21B == "No, have never done this sexual activity"] <- 0
UVA_data$NQ21B[UVA_data$NQ21B == "Yes"] <- 1
UVA_data$NQ21B[is.na(UVA_data$NQ21B)] <- 0

UVA_data$NQ21C[UVA_data$NQ21C == "No, have done this sexual activity in the past but not in the last 30 days"] <- 0
UVA_data$NQ21C[UVA_data$NQ21C == "No, have never done this sexual activity"] <- 0
UVA_data$NQ21C[UVA_data$NQ21C == "Yes"] <- 1
UVA_data$NQ21C[is.na(UVA_data$NQ21C)] <- 0

UVA_data$NQ21A <- as.numeric(UVA_data$NQ21A)
UVA_data$NQ21B <- as.numeric(UVA_data$NQ21B)
UVA_data$NQ21C <- as.numeric(UVA_data$NQ21C)

UVA_data$SexLast30Days <- UVA_data$NQ21A + UVA_data$NQ21B + UVA_data$NQ21C
UVA_data$SexLast30Days[UVA_data$SexLast30Days == 3] <- 1
UVA_data$SexLast30Days[UVA_data$SexLast30Days == 2] <- 1

#Exercise Data
UVA_data$NQ29A <- as.character(UVA_data$NQ29A)
UVA_data$NQ29B <- as.character(UVA_data$NQ29B)
UVA_data$NQ29C <- as.character(UVA_data$NQ29C)

UVA_data$NQ29A[UVA_data$NQ29A == "0 days"] <- 0
UVA_data$NQ29A[UVA_data$NQ29A == "1 day"] <- 1
UVA_data$NQ29A[UVA_data$NQ29A == "2 days"] <- 2
UVA_data$NQ29A[UVA_data$NQ29A == "3 days"] <- 3
UVA_data$NQ29A[UVA_data$NQ29A == "4 days"] <- 4
UVA_data$NQ29A[UVA_data$NQ29A == "5 days"] <- 5
UVA_data$NQ29A[UVA_data$NQ29A == "6 days"] <- 6
UVA_data$NQ29A[UVA_data$NQ29A == "7 days"] <- 7
UVA_data$NQ29A[is.na(UVA_data$NQ29A)] <- 0

UVA_data$NQ29B[UVA_data$NQ29B == "0 days"] <- 0
UVA_data$NQ29B[UVA_data$NQ29B == "1 day"] <- 1
UVA_data$NQ29B[UVA_data$NQ29B == "2 days"] <- 2
UVA_data$NQ29B[UVA_data$NQ29B == "3 days"] <- 3
UVA_data$NQ29B[UVA_data$NQ29B == "4 days"] <- 4
UVA_data$NQ29B[UVA_data$NQ29B == "5 days"] <- 5
UVA_data$NQ29B[UVA_data$NQ29B == "6 days"] <- 6
UVA_data$NQ29B[UVA_data$NQ29B == "7 days"] <- 7
UVA_data$NQ29B[is.na(UVA_data$NQ29B)] <- 0

UVA_data$NQ29C[UVA_data$NQ29C == "0 days"] <- 0
UVA_data$NQ29C[UVA_data$NQ29C == "1 day"] <- 1
UVA_data$NQ29C[UVA_data$NQ29C == "2 days"] <- 2
UVA_data$NQ29C[UVA_data$NQ29C == "3 days"] <- 3
UVA_data$NQ29C[UVA_data$NQ29C == "4 days"] <- 4
UVA_data$NQ29C[UVA_data$NQ29C == "5 days"] <- 5
UVA_data$NQ29C[UVA_data$NQ29C == "6 days"] <- 6
UVA_data$NQ29C[UVA_data$NQ29C == "7 days"] <- 7
UVA_data$NQ29C[is.na(UVA_data$NQ29C)] <- 0

UVA_data$Moderate_Cardio_Week <- as.numeric(UVA_data$NQ29A)
UVA_data$Intense_Cardio_Week <- as.numeric(UVA_data$NQ29B)
UVA_data$Strength_Training_Week <- as.numeric(UVA_data$NQ29C)


#Difficult to Handle Data
UVA_data$NQ33A <- as.character(UVA_data$NQ33A)
UVA_data$NQ33B <- as.character(UVA_data$NQ33B)
UVA_data$NQ33C <- as.character(UVA_data$NQ33C)
UVA_data$NQ33D <- as.character(UVA_data$NQ33D)
UVA_data$NQ33E <- as.character(UVA_data$NQ33E)
UVA_data$NQ33F <- as.character(UVA_data$NQ33F)
UVA_data$NQ33G <- as.character(UVA_data$NQ33G)
UVA_data$NQ33H <- as.character(UVA_data$NQ33H)
UVA_data$NQ33I <- as.character(UVA_data$NQ33I)
UVA_data$NQ33J <- as.character(UVA_data$NQ33J)
UVA_data$NQ33K <- as.character(UVA_data$NQ33K)
UVA_data$NQ33L <- as.character(UVA_data$NQ33L)

UVA_data$NQ33A[UVA_data$NQ33A == "Yes"] <- 1
UVA_data$NQ33A[UVA_data$NQ33A == "No"] <- 0
UVA_data$NQ33A[is.na(UVA_data$NQ33A)] <- 0

UVA_data$NQ33B[UVA_data$NQ33B == "Yes"] <- 1
UVA_data$NQ33B[UVA_data$NQ33B == "No"] <- 0
UVA_data$NQ33B[is.na(UVA_data$NQ33B)] <- 0

UVA_data$NQ33C[UVA_data$NQ33C == "Yes"] <- 1
UVA_data$NQ33C[UVA_data$NQ33C == "No"] <- 0
UVA_data$NQ33C[is.na(UVA_data$NQ33C)] <- 0

UVA_data$NQ33D[UVA_data$NQ33D == "Yes"] <- 1
UVA_data$NQ33D[UVA_data$NQ33D == "No"] <- 0
UVA_data$NQ33D[is.na(UVA_data$NQ33D)] <- 0

UVA_data$NQ33E[UVA_data$NQ33E == "Yes"] <- 1
UVA_data$NQ33E[UVA_data$NQ33E == "No"] <- 0
UVA_data$NQ33E[is.na(UVA_data$NQ33E)] <- 0

UVA_data$NQ33F[UVA_data$NQ33F == "Yes"] <- 1
UVA_data$NQ33F[UVA_data$NQ33F == "No"] <- 0
UVA_data$NQ33F[is.na(UVA_data$NQ33F)] <- 0

UVA_data$NQ33G[UVA_data$NQ33G == "Yes"] <- 1
UVA_data$NQ33G[UVA_data$NQ33G == "No"] <- 0
UVA_data$NQ33G[is.na(UVA_data$NQ33G)] <- 0

UVA_data$NQ33H[UVA_data$NQ33H == "Yes"] <- 1
UVA_data$NQ33H[UVA_data$NQ33H == "No"] <- 0
UVA_data$NQ33H[is.na(UVA_data$NQ33H)] <- 0

UVA_data$NQ33I[UVA_data$NQ33I == "Yes"] <- 1
UVA_data$NQ33I[UVA_data$NQ33I == "No"] <- 0
UVA_data$NQ33I[is.na(UVA_data$NQ33I)] <- 0

UVA_data$NQ33J[UVA_data$NQ33J == "Yes"] <- 1
UVA_data$NQ33J[UVA_data$NQ33J == "No"] <- 0
UVA_data$NQ33J[is.na(UVA_data$NQ33J)] <- 0

UVA_data$NQ33K[UVA_data$NQ33K == "Yes"] <- 1
UVA_data$NQ33K[UVA_data$NQ33K == "No"] <- 0
UVA_data$NQ33K[is.na(UVA_data$NQ33K)] <- 0

UVA_data$NQ33L[UVA_data$NQ33L == "Yes"] <- 1
UVA_data$NQ33L[UVA_data$NQ33L == "No"] <- 0
UVA_data$NQ33L[is.na(UVA_data$NQ33L)] <- 0

UVA_data$Diff_to_Handle_Academics <- as.numeric(UVA_data$NQ33A)
UVA_data$Diff_to_Handle_Career <- as.numeric(UVA_data$NQ33B)
UVA_data$Diff_to_Handle_Death <- as.numeric(UVA_data$NQ33C)
UVA_data$Diff_to_Handle_Family <- as.numeric(UVA_data$NQ33D)
UVA_data$Diff_to_Handle_Relationship <- as.numeric(UVA_data$NQ33E)
UVA_data$Diff_to_Handle_Friends <- as.numeric(UVA_data$NQ33F)
UVA_data$Diff_to_Handle_Finances <- as.numeric(UVA_data$NQ33G)
UVA_data$Diff_to_Handle_Health_Of_Family <- as.numeric(UVA_data$NQ33H)
UVA_data$Diff_to_Handle_Personal_Appearance <- as.numeric(UVA_data$NQ33I)
UVA_data$Diff_to_Handle_Personal_Health <- as.numeric(UVA_data$NQ33J)
UVA_data$Diff_to_Handle_Sleep <- as.numeric(UVA_data$NQ33K)
UVA_data$Diff_to_Handle_Other <- as.numeric(UVA_data$NQ33L)




#Sleep data
UVA_data$NQ42 <- as.character(UVA_data$NQ42)

UVA_data$NQ42[UVA_data$NQ42 == "0 days"] <- 0
UVA_data$NQ42[UVA_data$NQ42 == "1 day"] <- 1
UVA_data$NQ42[UVA_data$NQ42 == "2 days"] <- 2
UVA_data$NQ42[UVA_data$NQ42 == "3 days"] <- 3
UVA_data$NQ42[UVA_data$NQ42 == "4 days"] <- 4
UVA_data$NQ42[UVA_data$NQ42 == "5 days"] <- 5
UVA_data$NQ42[UVA_data$NQ42 == "6 days"] <- 6
UVA_data$NQ42[UVA_data$NQ42 == "7 days"] <- 7
UVA_data$NQ42[is.na(UVA_data$NQ42)] <- 0

UVA_data$DaysWellRested <- as.numeric(UVA_data$NQ42)

#Age
UVA_data$NQ46 <- as.numeric(UVA_data$NQ46)
UVA_data$Age <- UVA_data$NQ46

UVA_data$Sex <- as.character(UVA_data$RNQ47A)
UVA_data$Sex[UVA_data$Sex == "Male"] <- 1
UVA_data$Sex[UVA_data$Sex == "Female"] <- 0
UVA_data$Sex[is.na(UVA_data$Sex)] <- 0

#Weight
UVA_data$Weight <- as.numeric(UVA_data$NQ50)

#Year in school
UVA_data$FirstYear <- as.character(UVA_data$NQ51)
UVA_data$SecondYear <- as.character(UVA_data$NQ51)
UVA_data$ThirdYear <- as.character(UVA_data$NQ51)
UVA_data$FourthYear <- as.character(UVA_data$NQ51)

UVA_data$FirstYear[UVA_data$FirstYear == "1st year undergraduate"] <- 1
UVA_data$FirstYear[UVA_data$FirstYear == "2nd year undergraduate"] <- 0
UVA_data$FirstYear[UVA_data$FirstYear == "3rd year undergraduate"] <- 0
UVA_data$FirstYear[UVA_data$FirstYear == "4th year undergraduate"] <- 0
UVA_data$FirstYear[UVA_data$FirstYear == "5th year or more undergraduate"] <- 0
UVA_data$FirstYear[UVA_data$FirstYear == "Graduate or professional"] <- 0
UVA_data$FirstYear[is.na(UVA_data$FirstYear)] <- 0

UVA_data$SecondYear[UVA_data$SecondYear == "1st year undergraduate"] <- 0
UVA_data$SecondYear[UVA_data$SecondYear == "2nd year undergraduate"] <- 1
UVA_data$SecondYear[UVA_data$SecondYear == "3rd year undergraduate"] <- 0
UVA_data$SecondYear[UVA_data$SecondYear == "4th year undergraduate"] <- 0
UVA_data$SecondYear[UVA_data$SecondYear == "5th year or more undergraduate"] <- 0
UVA_data$SecondYear[UVA_data$SecondYear == "Graduate or professional"] <- 0
UVA_data$SecondYear[is.na(UVA_data$SecondYear)] <- 0

UVA_data$ThirdYear[UVA_data$ThirdYear == "1st year undergraduate"] <- 0
UVA_data$ThirdYear[UVA_data$ThirdYear == "2nd year undergraduate"] <- 0
UVA_data$ThirdYear[UVA_data$ThirdYear == "3rd year undergraduate"] <- 1
UVA_data$ThirdYear[UVA_data$ThirdYear == "4th year undergraduate"] <- 0
UVA_data$ThirdYear[UVA_data$ThirdYear == "5th year or more undergraduate"] <- 0
UVA_data$ThirdYear[UVA_data$ThirdYear == "Graduate or professional"] <- 0
UVA_data$ThirdYear[is.na(UVA_data$ThirdYear)] <- 0

UVA_data$FourthYear[UVA_data$FourthYear == "1st year undergraduate"] <- 0
UVA_data$FourthYear[UVA_data$FourthYear == "2nd year undergraduate"] <- 0
UVA_data$FourthYear[UVA_data$FourthYear == "3rd year undergraduate"] <- 0
UVA_data$FourthYear[UVA_data$FourthYear == "4th year undergraduate"] <- 1
UVA_data$FourthYear[UVA_data$FourthYear == "5th year or more undergraduate"] <- 0
UVA_data$FourthYear[UVA_data$FourthYear == "Graduate or professional"] <- 0
UVA_data$FourthYear[is.na(UVA_data$FourthYear)] <- 0





#Relationship
UVA_data$Relationship <- as.character(UVA_data$NQ56)

UVA_data$Relationship[UVA_data$Relationship == "In a relationship and living together"] <- 1
UVA_data$Relationship[UVA_data$Relationship == "In a relationship but not living together"] <- 1
UVA_data$Relationship[UVA_data$Relationship == "Not in a relationship"] <- 0
UVA_data$Relationship[is.na(UVA_data$Relationship)] <- 0

#GPA
UVA_data$GPA <- as.character(UVA_data$NQ63)
UVA_data$GPA[UVA_data$GPA == "N/A"] <- NA
UVA_data$GPA[is.na(UVA_data$GPA)] <- "B"

UVA_data$GPA_A <- UVA_data$GPA
UVA_data$GPA_B<- UVA_data$GPA

UVA_data$GPA_A[UVA_data$GPA_A == "A"] <- 1
UVA_data$GPA_A[UVA_data$GPA_A == "B"] <- 0
UVA_data$GPA_A[UVA_data$GPA_A == "C"] <- 0
UVA_data$GPA_B[UVA_data$GPA_B == "B"] <- 1
UVA_data$GPA_B[UVA_data$GPA_B == "A"] <- 0
UVA_data$GPA_B[UVA_data$GPA_B == "C"] <- 0

#Disabilities
UVA_data$NQ65A <- as.character(UVA_data$NQ65A)
UVA_data$NQ65B <- as.character(UVA_data$NQ65B)
UVA_data$NQ65C <- as.character(UVA_data$NQ65C)
UVA_data$NQ65D <- as.character(UVA_data$NQ65D)
UVA_data$NQ65E <- as.character(UVA_data$NQ65E)
UVA_data$NQ65F <- as.character(UVA_data$NQ65F)
UVA_data$NQ65G <- as.character(UVA_data$NQ65G)
UVA_data$NQ65H <- as.character(UVA_data$NQ65H)
UVA_data$NQ65I <- as.character(UVA_data$NQ65I)

UVA_data$NQ65A[UVA_data$NQ65A == "Yes"] <- 1
UVA_data$NQ65A[UVA_data$NQ65A == "No"] <- 0
UVA_data$NQ65A[is.na(UVA_data$NQ65A)] <- 0

UVA_data$NQ65B[UVA_data$NQ65B == "Yes"] <- 1
UVA_data$NQ65B[UVA_data$NQ65B == "No"] <- 0
UVA_data$NQ65B[is.na(UVA_data$NQ65B)] <- 0

UVA_data$NQ65C[UVA_data$NQ65C == "Yes"] <- 1
UVA_data$NQ65C[UVA_data$NQ65C == "No"] <- 0
UVA_data$NQ65C[is.na(UVA_data$NQ65C)] <- 0

UVA_data$NQ65D[UVA_data$NQ65D == "Yes"] <- 1
UVA_data$NQ65D[UVA_data$NQ65D == "No"] <- 0
UVA_data$NQ65D[is.na(UVA_data$NQ65D)] <- 0

UVA_data$NQ65E[UVA_data$NQ65E == "Yes"] <- 1
UVA_data$NQ65E[UVA_data$NQ65E == "No"] <- 0
UVA_data$NQ65E[is.na(UVA_data$NQ65E)] <- 0

UVA_data$NQ65F[UVA_data$NQ65F == "Yes"] <- 1
UVA_data$NQ65F[UVA_data$NQ65F == "No"] <- 0
UVA_data$NQ65F[is.na(UVA_data$NQ65F)] <- 0

UVA_data$NQ65G[UVA_data$NQ65G == "Yes"] <- 1
UVA_data$NQ65G[UVA_data$NQ65G == "No"] <- 0
UVA_data$NQ65G[is.na(UVA_data$NQ65G)] <- 0

UVA_data$NQ65H[UVA_data$NQ65H == "Yes"] <- 1
UVA_data$NQ65H[UVA_data$NQ65H == "No"] <- 0
UVA_data$NQ65H[is.na(UVA_data$NQ65H)] <- 0

UVA_data$NQ65I[UVA_data$NQ65I == "Yes"] <- 1
UVA_data$NQ65I[UVA_data$NQ65I == "No"] <- 0
UVA_data$NQ65I[is.na(UVA_data$NQ65I)] <- 0

UVA_data$ADHD <- as.numeric(UVA_data$NQ65A)
UVA_data$Chronic_Illness <- as.numeric(UVA_data$NQ65B)
UVA_data$Hearing_Loss <- as.numeric(UVA_data$NQ65C)
UVA_data$Learning_Disability <- as.numeric(UVA_data$NQ65D)
UVA_data$Mobility_Disability <- as.numeric(UVA_data$NQ65E)
UVA_data$Sight_Disability <- as.numeric(UVA_data$NQ65F)
UVA_data$Psychiatric_Disability <- as.numeric(UVA_data$NQ65G)
UVA_data$Speech_Disability <- as.numeric(UVA_data$NQ65H)
UVA_data$Other_Disability <- as.numeric(UVA_data$NQ65I)

UVA_data$BMI <- as.numeric(UVA_data$BMI)


#Perceived Stress Data
UVA_data$NQ37 <- as.character(UVA_data$NQ37)

UVA_data$NQ37[UVA_data$NQ37 == "Tremendous stress"] <- 4
UVA_data$NQ37[UVA_data$NQ37 == "More than average stress"] <- 3
UVA_data$NQ37[UVA_data$NQ37 == "Average stress"] <- 2
UVA_data$NQ37[UVA_data$NQ37 == "Less than average stress"] <- 1
UVA_data$NQ37[UVA_data$NQ37 == "No stress"] <- 0
UVA_data$NQ37[is.na(UVA_data$NQ37)] <- 2

UVA_data$Hopeless <- as.character(UVA_data$NQ30A)
UVA_data$Hopeless[UVA_data$Hopeless == "Yes, in the last 2 weeks"] <- 1
UVA_data$Hopeless[UVA_data$Hopeless == "Yes, in the last 30 days"] <- 1
UVA_data$Hopeless[UVA_data$Hopeless == "No, never"] <- 0
UVA_data$Hopeless[UVA_data$Hopeless == "No, not in the last 12 months"] <- 0
UVA_data$Hopeless[UVA_data$Hopeless == "Yes, in the last 12 months"] <- 0
UVA_data$Hopeless[is.na(UVA_data$Hopeless)] <- 0

UVA_data$Overwhelmed <- as.character(UVA_data$NQ30B)
UVA_data$Overwhelmed[UVA_data$Overwhelmed == "Yes, in the last 2 weeks"] <- 1
UVA_data$Overwhelmed[UVA_data$Overwhelmed == "Yes, in the last 30 days"] <- 1
UVA_data$Overwhelmed[UVA_data$Overwhelmed == "No, never"] <- 0
UVA_data$Overwhelmed[UVA_data$Overwhelmed == "No, not in the last 12 months"] <- 0
UVA_data$Overwhelmed[UVA_data$Overwhelmed == "Yes, in the last 12 months"] <- 0
UVA_data$Overwhelmed[is.na(UVA_data$Overwhelmed)] <- 0

UVA_data$Exhausted <- as.character(UVA_data$NQ30C)
UVA_data$Exhausted[UVA_data$Exhausted == "Yes, in the last 2 weeks"] <- 1
UVA_data$Exhausted[UVA_data$Exhausted == "Yes, in the last 30 days"] <- 1
UVA_data$Exhausted[UVA_data$Exhausted == "No, never"] <- 0
UVA_data$Exhausted[UVA_data$Exhausted == "No, not in the last 12 months"] <- 0
UVA_data$Exhausted[UVA_data$Exhausted == "Yes, in the last 12 months"] <- 0
UVA_data$Exhausted[is.na(UVA_data$Exhausted)] <- 0

UVA_data$Lonely <- as.character(UVA_data$NQ30D)
UVA_data$Lonely[UVA_data$Lonely == "Yes, in the last 2 weeks"] <- 1
UVA_data$Lonely[UVA_data$Lonely == "Yes, in the last 30 days"] <- 1
UVA_data$Lonely[UVA_data$Lonely == "No, never"] <- 0
UVA_data$Lonely[UVA_data$Lonely == "No, not in the last 12 months"] <- 0
UVA_data$Lonely[UVA_data$Lonely == "Yes, in the last 12 months"] <- 0
UVA_data$Lonely[is.na(UVA_data$Lonely)] <- 0

UVA_data$VerySad <- as.character(UVA_data$NQ30E)
UVA_data$VerySad[UVA_data$VerySad == "Yes, in the last 2 weeks"] <- 1
UVA_data$VerySad[UVA_data$VerySad == "Yes, in the last 30 days"] <- 1
UVA_data$VerySad[UVA_data$VerySad == "No, never"] <- 0
UVA_data$VerySad[UVA_data$VerySad == "No, not in the last 12 months"] <- 0
UVA_data$VerySad[UVA_data$VerySad == "Yes, in the last 12 months"] <- 0
UVA_data$VerySad[is.na(UVA_data$VerySad)] <- 0

UVA_data$Depressed <- as.character(UVA_data$NQ30F)
UVA_data$Depressed[UVA_data$Depressed == "Yes, in the last 2 weeks"] <- 1
UVA_data$Depressed[UVA_data$Depressed == "Yes, in the last 30 days"] <- 1
UVA_data$Depressed[UVA_data$Depressed == "No, never"] <- 0
UVA_data$Depressed[UVA_data$Depressed == "No, not in the last 12 months"] <- 0
UVA_data$Depressed[UVA_data$Depressed == "Yes, in the last 12 months"] <- 0
UVA_data$Depressed[is.na(UVA_data$Depressed)] <- 0

UVA_data$Anxiety <- as.character(UVA_data$NQ30G)
UVA_data$Anxiety[UVA_data$Anxiety == "Yes, in the last 2 weeks"] <- 1
UVA_data$Anxiety[UVA_data$Anxiety == "Yes, in the last 30 days"] <- 1
UVA_data$Anxiety[UVA_data$Anxiety == "No, never"] <- 0
UVA_data$Anxiety[UVA_data$Anxiety == "No, not in the last 12 months"] <- 0
UVA_data$Anxiety[UVA_data$Anxiety == "Yes, in the last 12 months"] <- 0
UVA_data$Anxiety[is.na(UVA_data$Anxiety)] <- 0

UVA_data$Anger <- as.character(UVA_data$NQ30H)
UVA_data$Anger[UVA_data$Anger == "Yes, in the last 2 weeks"] <- 1
UVA_data$Anger[UVA_data$Anger == "Yes, in the last 30 days"] <- 1
UVA_data$Anger[UVA_data$Anger == "No, never"] <- 0
UVA_data$Anger[UVA_data$Anger == "No, not in the last 12 months"] <- 0
UVA_data$Anger[UVA_data$Anger == "Yes, in the last 12 months"] <- 0
UVA_data$Anger[is.na(UVA_data$Anger)] <- 0


UVA_data$Stress <- as.numeric(UVA_data$NQ37)

UVA_data <- UVA_data[-c(1:58)]
UVA_data <- UVA_data[-c(2)]
UVA_data <- UVA_data[-c(35)]

UVA_data <- na.omit(UVA_data)








