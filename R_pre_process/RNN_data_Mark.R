#setwd("~/B/VU/Data mining/1")
#set working directory at directory of this script

library(data.table)
data <- data.table::fread(input = "../dataset_mood_smartphone.csv",drop = 1)
length(unique(data$id)) #just 27 users
variable_names <- unique(data$variable)
length(variable_names) # 19 variable

#some subjects have NA's for arousal and valence (total of 202 timestamps)
#sum(is.na(data))
#remove rows where value is NA
#data <- data[!is.na(value)]

data$time <- as.Date(data$time)


#Extra variables
#head(weekdays(data$time))
#data_sep$mood[,.N,by=.(id,time,variable)]


#split data on variables and aggregate by day where the mean(1-4) or sum(5-19) and rebind 
data_sep <- split(data, data$variable)
{data_sep$mood <- data_sep$mood[,.(value = mean(value)),by=.(id,time,variable)]
data_sep$circumplex.arousal <- data_sep$circumplex.arousal[,.(value = mean(value)),by=.(id,time,variable)]
data_sep$circumplex.valence <- data_sep$circumplex.valence[,.(value = mean(value)),by=.(id,time,variable)]
data_sep$activity <- data_sep$activity[,.(value = mean(value)),by=.(id,time,variable)]
data_sep$screen <- data_sep$screen[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$call <- data_sep$call[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$sms <- data_sep$sms[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.builtin <- data_sep$appCat.builtin[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.communication <- data_sep$appCat.communication[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.entertainment <- data_sep$appCat.entertainment[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.finance <- data_sep$appCat.finance[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.game <-data_sep$appCat.game[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.office <- data_sep$appCat.office[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.other <- data_sep$appCat.other[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.social <- data_sep$appCat.social[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.travel <- data_sep$appCat.travel[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.unknown <- data_sep$appCat.unknown[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.utilities <- data_sep$appCat.utilities[,.(value = sum(value)),by=.(id,time,variable)]
data_sep$appCat.weather <- data_sep$appCat.weather[,.(value = sum(value)),by=.(id,time,variable)]}
data_new <- do.call("rbind", data_sep)


data_new <- data_new[order(id, time, variable),]


#only take days where there is mood measured
#mood_only_dates <- data_new[variable == "mood"][,.(id,time)]
#data_new <- data_new[mood_only_dates, on = c(id="id", time="time")]


#changing data from long to wide format for exploratory reasons
library(reshape2)
data_wide <- dcast(data_new, id + time ~ variable, value.var = "value")

#plot to look to the distribution of mood over time
#in Februari there is a total of 2 timestamps for mood registered. Hence, we throw februari out of the dataset
plot(data_wide$time,data_wide$mood, xlab = "Date", ylab = "Mood", main = "Mood versus time of all subjects")

data_wide_new <- data_wide[format(data_wide$time, '%m') != "02",]
plot(data_wide_new$time,data_wide_new$mood, xlab = "Date", ylab = "Mood", main = "Mood versus time of all subjects")

#check if variables are correlated to mood, only circumplex valence seems to be slightly
cor.test(data_wide_new$mood, data_wide_new$circumplex.valence)

#replace duration NA's with 0
data_wide_new[,c(4:16, 20, 21)][is.na(data_wide_new[,c(4:16, 20, 21)])] <- 0

#make RNN data
source("RNN_transformation_function_Mark.R")
result <- RNN_data(data_wide_new, 5)

RNN_data <- data.frame(result)

#change column names
colnames(RNN_data) <- c("id","activity1", "builtin1", "communication1", "entertainment1", "finance1", "game1", "office1", "other1", 
                        "social1", "travel1", "unknown1", "utilities1", "weather1", "call1", "arousal1", "valence1", "mood1", "screen1", "sms1", "week1", "month-day1", "month1",
                        "activity2", "builtin2", "communication2", "entertainment2", "finance2", "game2", "office2", "other2", 
                        "social2", "travel2", "unknown2", "utilities2", "weather2", "call2", "arousal2", "valence2", "mood2", "screen2", "sms2", "week2", "month-day2", "month2",
                        "activity3", "builtin3", "communication3", "entertainment3", "finance3", "game3", "office3", "other3", 
                        "social3", "travel3", "unknown3", "utilities3", "weather3", "call3", "arousal3", "valence3", "mood3", "screen3", "sms3", "week3", "month-day3", "month3",
                        "activity4", "builtin4", "communication4", "entertainment4", "finance4", "game4", "office4", "other4", 
                        "social4", "travel4", "unknown4", "utilities4", "weather4", "call4", "arousal4", "valence4", "mood4", "screen4", "sms4", "week4", "month-day4", "month4",
                        "activity5", "builtin5", "communication5", "entertainment5", "finance5", "game5", "office5", "other5", 
                        "social5", "travel5", "unknown5", "utilities5", "weather5", "call5", "arousal5", "valence5", "mood5", "screen5", "sms5", "week5", "month-day5", "month5",
                        "target")

#delete all rows where target variable is NA
RNN_data <- RNN_data[!is.na(RNN_data$target),]

#replace NA's of activity, arousal, valence and mood with mean from whole dataset per id
x <- unique(RNN_data$id)
for(id in x){
  activity.mean <- mean(data_wide[data_wide$id == id,]$activity, na.rm = TRUE)
  RNN_data[RNN_data$id == id, c("activity1", "activity2", "activity3", "activity4", "activity5")][is.na(RNN_data[RNN_data$id == id, c("activity1", "activity2", "activity3", "activity4", "activity5")])] = activity.mean
  
  arousal.mean <- mean(data_wide[data_wide$id == id,]$circumplex.arousal, na.rm = TRUE)
  RNN_data[RNN_data$id == id, c("arousal1", "arousal2", "arousal3", "arousal4", "arousal5")][is.na(RNN_data[RNN_data$id == id, c("arousal1", "arousal2", "arousal3", "arousal4", "arousal5")])] = arousal.mean
  
  valence.mean <- mean(data_wide[data_wide$id == id,]$circumplex.valence, na.rm = TRUE)
  RNN_data[RNN_data$id == id, c("valence1", "valence2", "valence3", "valence4", "valence5")][is.na(RNN_data[RNN_data$id == id, c("valence1", "valence2", "valence3", "valence4", "valence5")])] = valence.mean
  
  mood.mean <- mean(data_wide[data_wide$id == id,]$mood, na.rm = TRUE)
  RNN_data[RNN_data$id == id, c("mood1", "mood2", "mood3", "mood4", "mood5")][is.na(RNN_data[RNN_data$id == id, c("mood1", "mood2", "mood3", "mood4", "mood5")])] = mood.mean
}

#now remove every row with NA, because this is a row with at least one day where no data is captured
RNN_data <- na.omit(RNN_data)

#write RNN_data to disk
#write.csv(RNN_data, file = "../RNN_data_version2.csv")
