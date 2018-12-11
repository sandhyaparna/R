options(scipen = 999)

# Train <- read.csv("C:/Users/spashikanti/Downloads/train_users_2.csv")

Train <-read.csv("C:/Users/Sandhya/OneDrive/Data Science Practical Learning/Projects/Airbnb New User Bookings/Data/train_users_2.csv")
Test <- read.csv("C:/Users/Sandhya/OneDrive/Data Science Practical Learning/Projects/Airbnb New User Bookings/Data/test_users.csv")
Test$country_destination <- NA
sessions <- read.csv("C:/Users/Sandhya/OneDrive/Data Science Practical Learning/Projects/Airbnb New User Bookings/Data/sessions.csv")
age_gender_bkts <- read.csv("C:/Users/Sandhya/OneDrive/Data Science Practical Learning/Projects/Airbnb New User Bookings/Data/age_gender_bkts.csv")
countries <- read.csv("C:/Users/Sandhya/OneDrive/Data Science Practical Learning/Projects/Airbnb New User Bookings/Data/countries.csv")


#Data pre-processing is done after combining test and train
Train$data <- "Train"
Test$data <- "Test"
All <- rbind(Train,Test)

summary(All)
summary(Train)
summary(Test)

# 50% of "age" is missing
# If we look at train and test seperately - date_first_booking is completely in test and a lot in Train
#Number of missing values in date_first_booking
missing_first <- sum(Train$date_first_booking=="")
missing_first_Test <- sum(Test$date_first_booking=="")

# Hence "date_first_booking" variable is missing

#Age Univariate analysis and clean up
Age_Train <- as.data.frame(table(Train$age))
Age_Test <- as.data.frame(table(Test$age))
Age <- as.data.frame(table(All$age))

All$age1 <- ifelse(All$age>1900, 2015-All$age,All$age)
All$age1 <- ifelse(All$age1<14 | All$age1>110, NA,All$age1)

Age1 <- as.data.frame(table(All$age1))
#
All$age_bucket = cut(All$age1, breaks = c(min(All$age1), 4, 9, 14, 19, 24,
                                 29, 34, 39, 44, 49, 54,
                                 59, 64, 69, 74, 79, 84,
                                 89, 94, 99, max(All$age1)))
All$age_bucket <- plyr::mapvalues(All$age_bucket,
                         from=c("(1,4]", "(4,9]", "(9,14]", "(14,19]",
                                "(19,24]", "(24,29]", "(29,34]", "(34,39]",
                                "(39,44]", "(44,49]", "(49,54]", "(54,59]",
                                "(59,64]", "(64,69]", "(69,74]", "(74,79]",
                                "(79,84]", "(84,89]", "(89,94]", "(94,99]", "(99,150]"),
                         to=c("0-4", "5-9", "10-14", "15-19",
                              "20-24", "25-29", "30-34", "35-39",
                              "40-44", "45-49", "50-54", "55-59",
                              "60-64", "65-69", "70-74", "75-79",
                              "80-84", "85-89", "90-94", "95-99", "100+"))
# See how age is related to Country Destination
Age_COuntry <- All[c(16:17)]
Age_COuntry <- na.omit(Age_COuntry)
Age_COuntry_plot <- boxplot(age1~country_destination, data=Age_COuntry)

# for date variables create day, year, month, weekday, Quarter, Season
All$first_active_year <- substr(as.character(All$timestamp_first_active),1,4)
All$first_active_month <- substr(as.character(All$timestamp_first_active),5,6)
All$first_active_day <- substr(as.character(All$timestamp_first_active),7,8)
All$first_active_date <- ISOdate(All$first_active_year, All$first_active_month, All$first_active_day)
All$first_active_date <- as.Date(All$first_active_date,"%m%d%Y")
All$first_active_weekday <- weekdays(All$first_active_date)
All$first_active_Qtr <- quarters(All$first_active_date) 

All$date_account_created <- as.Date(All$date_account_created)
All$account_created_weekday <- weekdays(All$date_account_created)
All$account_created_Qtr <- quarters(All$date_account_created) 
All$account_created_day <- as.numeric(format(All$date_account_created, "%d"))
All$account_created_year <- as.numeric(format(All$date_account_created, "%Y"))
All$account_created_month <- as.numeric(format(All$date_account_created, "%m"))

All$date_first_booking <- ISOdate(substr(as.character(All$date_first_booking),1,4), substr(as.character(All$date_first_booking),6,7), substr(as.character(All$date_first_booking),9,10))
All$date_first_booking <- as.Date(All$date_first_booking,"%m%d%Y")
All$first_booking_weekday <- weekdays(All$date_first_booking)
All$first_booking_Qtr <- quarters(All$date_first_booking) 
All$first_booking_day <- as.numeric(format(All$date_first_booking, "%d"))
All$first_bookinging_year <- as.numeric(format(All$date_first_booking, "%Y"))
All$first_bookinging_month <- as.numeric(format(All$date_first_booking, "%m"))

All$Active_Account_Days <- as.numeric(All$date_account_created-All$first_active_date)
All$Active_Booking_Days <- as.numeric(All$date_first_booking-All$first_active_date)
All$Account_Booking_Days <- as.numeric(All$date_first_booking-All$date_account_created)

x <- All
#Join Countries
All <- sqldf::sqldf("select A.*, B.distance_km,B.destination_km2,
                    B.destination_language,B.language_levenshtein_distance
                    from x as A LEFT JOIN countries as B ON
                    A.country_destination=B.country_destination")


#Sessions

#No of Actions
#A <- sessions
sessions <- A
Freq_action <- as.data.frame(table(sessions$action))
Freq_actiontype <- as.data.frame(table(sessions$action_type))
Freq_actiondetail <- as.data.frame(table(sessions$action_detail))
Freq_devicetype <- as.data.frame(table(sessions$device_type))
Freq_sessions <- as.data.frame(table(sessions$user_id))

sessions$action <- ifelse(sessions$action=="","BLANK",as.character(sessions$action))
sessions$action_type <- ifelse(sessions$action_type=="","BLANK",as.character(sessions$action_type))
sessions$action_detail <- ifelse(sessions$action_detail=="","BLANK",as.character(sessions$action_detail))
sessions$device_type <- ifelse(sessions$device_type=="","BLANK",as.character(sessions$device_type))


# Counts and Distinct Counts
Action <- aggregate(action ~ user_id, sessions, length)
Dis_Action <- aggregate(action ~ user_id, sessions, function(x) length(unique(x)))

Action_type <- aggregate(action_type ~ user_id, sessions, length)
Dis_Action_type <- aggregate(action_type ~ user_id, sessions, function(x) length(unique(x)))

Action_detail <- aggregate(action_detail ~ user_id, sessions, length)
Dis_Action_detail <- aggregate(action_detail ~ user_id, sessions, function(x) length(unique(x)))

Device_type <- aggregate(device_type ~ user_id, sessions, length)
Dis_Device_type <- aggregate(device_type ~ user_id, sessions, function(x) length(unique(x)))

Sec <- aggregate(secs_elapsed ~ user_id, sessions, sum)
Sec_Mean <- aggregate(secs_elapsed ~ user_id, sessions, mean)
Sec_Min <- aggregate(secs_elapsed ~ user_id, sessions, min)

#Max seconds by a user
Sec_Max <- aggregate(secs_elapsed ~ user_id, sessions, max)
Sec_Max$Max <- 1

# Actions, Action_type, Device_type, action_detail that they spend the most time on
Max_Sec_Cat <- sqldf::sqldf("Select A.*,B.Max from sessions as A LEFT JOIN Sec_Max as B ON
                  A.user_id=B.user_id and A.secs_elapsed=B.secs_elapsed")
Max_Sec_Cat <- dplyr::filter(Max_Sec_Cat,Max==1)
# Identify duplicates
Freq_id_secs <- dplyr::filter(as.data.frame(table(A$user_id)),Freq>1)

#Entropy
X <- entropy::entropy(as.factor(sessions$action))


# Action that was used max no of times(and % of that action compared to other actions)
Action_Max <- sqldf::sqldf("select user_id, action, count(action) as Freq
                                from sessions group by user_id, action") 

Action_Max1 <- aggregate(Freq ~ user_id, Action_Max, max)
Action_Max1$Max_cat <- 1
Action_Max <- sqldf::sqldf("select A.*,B.Max_cat from Action_Max as A LEFT JOIN Action_Max1 as B ON
                                A.user_id=B.user_id and A.Freq=B.Freq") 
Action_Max <- dplyr::filter(Action_Max,Max_cat==1)
Action_Max <- sqldf::sqldf("select A.*, B.action as Freq_All from Action_Max as A Left JOIn Action as B 
                           On A.user_id=B.user_id")
Action_Max$Freq_perc <- Action_Max$Freq/Action_Max$Freq_All

# Device that was used max no of times
Device_type_Max <- sqldf::sqldf("select user_id, device_type, count(device_type) as Freq 
                                from sessions group by user_id, device_type") 

Device_type_Max1 <- aggregate(Freq ~ user_id, Device_type_Max, max)
Device_type_Max1$Max_cat <- 1
Device_type_Max <- sqldf::sqldf("select A.*,B.Max_cat from Device_type_Max as A LEFT JOIN Device_type_Max1 as B ON
                                A.user_id=B.user_id and A.Freq=B.Freq") 
Device_type_Max <- dplyr::filter(Device_type_Max,Max_cat==1)

# ACtion Type that was used max no of times
Action_type_Max <- sqldf::sqldf("select user_id, action_type, count(action_type) as Freq 
                                from sessions group by user_id, action_type") 

Action_type_Max1 <- aggregate(Freq ~ user_id, Action_type_Max, max)
Action_type_Max1$Max_cat <- 1
Action_type_Max <- sqldf::sqldf("select A.*,B.Max_cat from Action_type_Max as A LEFT JOIN Action_type_Max1 as B ON
                                A.user_id=B.user_id and A.Freq=B.Freq") 
Action_type_Max <- dplyr::filter(Action_type_Max,Max_cat==1)

# Action Detail that was used max no of times
Action_detail_Max <- sqldf::sqldf("select user_id, action_detail, count(action_detail) as Freq 
                                from sessions group by user_id, action_detail") 

Action_detail_Max1 <- aggregate(Freq ~ user_id, Action_detail_Max, max)
Action_detail_Max1$Max_cat <- 1
Action_detail_Max <- sqldf::sqldf("select A.*,B.Max_cat from Action_detail_Max as A LEFT JOIN Action_detail_Max1 as B ON
                  A.user_id=B.user_id and A.Freq=B.Freq") 
Action_detail_Max <- dplyr::filter(Action_detail_Max,Max_cat==1)


#Age Gender Buckets - In a country - for a specific age group - What is the probability that the person might be a male
Male <- dplyr::filter(age_gender_bkts,gender=="male")
Female <- dplyr::filter(age_gender_bkts,gender=="female")

Age_Gender <- sqldf::sqldf("select A.*, B.population_in_thousands as Female_pop from Male as A LEFT JOIN Female as B 
                           On A.age_bucket=B.age_bucket and A.country_destination =B.country_destination")
Age_Gender$Total_Pop <- Age_Gender$population_in_thousands+Age_Gender$Female_pop
Age_Gender$M2F <- Age_Gender$population_in_thousands/Age_Gender$Total_Pop



save.image("C:/Users/Sandhya/OneDrive/Data Science Practical Learning/Projects/Airbnb New User Bookings/Data/r data.RData")


Train$NewID <- as.numeric(factor(Train$id,labels=levels(Train$id)))
