

#setwd(".../mood_smartphone")

library(data.table)
data <- data.table::fread(input = "dataset_mood_smartphone.csv",drop = 1)
length(unique(data$id)) #just 27 users
variable_names <- unique(data$variable)
length(variable_names) # 19 variable

#remove rows where value is NA
data <- data[!is.na(value)]

data$time <- as.Date(data$time)


#split data on variables and aggregate by day where the mean(1-4) or sum(5-19) and rebind 
data_sep <- split(data, data$variable)

{data_sep$mood.count <- data_sep$mood[,.(variable = "mood.count", value = .N),by=.(id,time)]
  
  
  data_sep$mood <- data_sep$mood[,.(value = mean(value)),by=.(id,time,variable)]
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
mood_only_dates <- data_new[variable == "mood"][,.(id,time)]
data_new <- data_new[mood_only_dates, on = c(id="id", time="time")]


#changing data from long to wide format for exploratory reasons
library(reshape2)
data_wide <- dcast(data_new, id + time ~ variable, value.var = "value")

#plot to look to the distribution of mood over time
plot(data_wide$time,data_wide$mood)

#check if variables are correlated to mood, only circumplex valence seems to be slightly
cor.test(data_wide$mood, data_wide$circumplex.valence)





#replace duration NA's with 0
data_wide[,-3][is.na(data_wide[,-3])] <- 0

#make RNN data
#setwd(".../mood_smartphone/R_pre_process")
source("/RNN_transformation_function.R")
result <- RNN_data(data_wide, 5)
RNN_data <- data.table(result[,c(-23,-24,-45,-46,-67,-68,-89,-90)])

#change column names
colnames(RNN_data) <- c("id","time",
                        "activity1", "builtin1", "communication1", "entertainment1", "finance1", "game1", "office1", "other1", 
                        "social1", "travel1", "unknown1", "utilities1", "weather1", "call1", "arousal1", "valence1", "mood1", "count1", "screen1", "sms1",
                        "activity2", "builtin2", "communication2", "entertainment2", "finance2", "game2", "office2", "other2", 
                        "social2", "travel2", "unknown2", "utilities2", "weather2", "call2", "arousal2", "valence2", "mood2", "count2", "screen2", "sms2",
                        "activity3", "builtin3", "communication3", "entertainment3", "finance3", "game3", "office3", "other3", 
                        "social3", "travel3", "unknown3", "utilities3", "weather3", "call3", "arousal3", "valence3", "mood3", "count3", "screen3", "sms3",
                        "activity4", "builtin4", "communication4", "entertainment4", "finance4", "game4", "office4", "other4", 
                        "social4", "travel4", "unknown4", "utilities4", "weather4", "call4", "arousal4", "valence4", "mood4", "count4", "screen4", "sms4",
                        "activity5", "builtin5", "communication5", "entertainment5", "finance5", "game5", "office5", "other5", 
                        "social5", "travel5", "unknown5", "utilities5", "weather5", "call5", "arousal5", "valence5", "mood5", "count5", "screen5", "sms5",
                        "target")

 activity.mean <- mean(data_wide$activity, na.rm = TRUE)
 RNN_data[,c("activity1", "activity2", "activity3", "activity4", "activity5")][is.na(RNN_data[,c("activity1", "activity2", "activity3", "activity4", "activity5")])] <-  activity.mean

# # USELESS CODE when omitting all rows with NA's afterwards 
#
# #delete all rows where target variable is NA
# RNN_data <- RNN_data[!is.na(RNN_data$target),]
# 
# #replace NA's of arousal, valence and mood with mean from whole dataset
# arousal.mean <- mean(data_wide$circumplex.arousal, na.rm = TRUE)
# RNN_data[,c("arousal1", "arousal2", "arousal3", "arousal4", "arousal5")][is.na(RNN_data[,c("arousal1", "arousal2", "arousal3", "arousal4", "arousal5")])] <-  arousal.mean
# 
# valence.mean <- mean(data_wide$circumplex.valence, na.rm = TRUE)
# RNN_data[,c("valence1", "valence2", "valence3", "valence4", "valence5")][is.na(RNN_data[,c("valence1", "valence2", "valence3", "valence4", "valence5")])] = valence.mean
# 
# mood.mean <- mean(data_wide$mood, na.rm = TRUE)
# RNN_data[,c("mood1", "mood2", "mood3", "mood4", "mood5")][is.na(RNN_data[,c("mood1", "mood2", "mood3", "mood4", "mood5")])] = mood.mean

#now remove every row with NA, because this is a row with at least one day where no data is captured
RNN_data <- na.omit(RNN_data)
RNN_data[,2:103] <- lapply(RNN_data[,2:103], function(x) as.numeric(x))

RNN_data$time <- as.Date(RNN_data$time+5,origin = "01-01-1970", format = "%d-%m-%Y")
RNN_data$weekday <- as.numeric(format(RNN_data$time,"%w")) 
RNN_data$month <- month(RNN_data$time)
RNN_data$monthday <- as.numeric(format(RNN_data$time,"%d")) 

RNN_data$slope <- apply(RNN_data[,c("mood1","mood2","mood3","mood4","mood5")], 1, function(x) lm(unname(unlist(x)) ~ c(1,2,3,4,5))$coefficients[[2]])
RNN_data$intersect <- apply(RNN_data[,c("mood1","mood2","mood3","mood4","mood5")], 1, function(x) lm(unname(unlist(x)) ~ c(1,2,3,4,5))$coefficients[[1]])


#write RNN_data to disk
#write.csv(RNN_data, file = ".../mood_smartphone/RNN_data_version1.csv")

TempForm_data <- RNN_data[,c(1,2,103,104,105,106,107,108)]



{TempForm_data$social <- rowMeans(RNN_data[,c("social1","social2","social3","social4","social5")])
TempForm_data$travel <- rowMeans(RNN_data[,c("travel1","travel2","travel3","travel4","travel5")])
TempForm_data$unknown <- rowMeans(RNN_data[,c("unknown1","unknown2","unknown3","unknown4","unknown5")])
TempForm_data$utilities <- rowMeans(RNN_data[,c("utilities1","utilities2","utilities3","utilities4","utilities5")])
TempForm_data$weather <- rowMeans(RNN_data[,c("weather1","weather2","weather3","weather4","weather5")])
TempForm_data$call <- rowMeans(RNN_data[,c("call1","call2","call3","call4","call5")])
TempForm_data$arousal <- rowMeans(RNN_data[,c("arousal1","arousal2","arousal3","arousal4","arousal5")])
TempForm_data$valence <- rowMeans(RNN_data[,c("valence1","valence2","valence3","valence4","valence5")])
TempForm_data$mood <- rowMeans(RNN_data[,c("mood1","mood2","mood3","mood4","mood5")])
TempForm_data$count <- rowMeans(RNN_data[,c("count1","count2","count3","count4","count5")])
TempForm_data$screen <- rowMeans(RNN_data[,c("screen1","screen2","screen3","screen4","screen5")])
TempForm_data$sms <- rowMeans(RNN_data[,c("sms1","sms2","sms3","sms4","sms5")])
TempForm_data$activity <- rowMeans(RNN_data[,c("activity1","activity2","activity3","activity4","activity5")])
TempForm_data$builtin <- rowMeans(RNN_data[,c("builtin1","builtin2","builtin3","builtin4","builtin5")])
TempForm_data$communication <- rowMeans(RNN_data[,c("communication1","communication2","communication3","communication4","communication5")])
TempForm_data$entertainment <- rowMeans(RNN_data[,c("entertainment1","entertainment2","entertainment3","entertainment4","entertainment5")])
TempForm_data$finance <- rowMeans(RNN_data[,c("finance1","finance2","finance3","finance4","finance5")])
TempForm_data$game <- rowMeans(RNN_data[,c("game1","game2","game3","game4","game5")])
TempForm_data$office <- rowMeans(RNN_data[,c("office1","office2","office3","office4","office5")])
TempForm_data$other <- rowMeans(RNN_data[,c("other1","other2","other3","other4","other5")])}




#write TempForm_data to disk
#write.csv(TempForm_data, file = ".../mood_smartphone/TempForm_data_version1.csv")
  
  