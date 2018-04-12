setwd("~/B/VU/Data mining/1")

library(data.table)
data <- data.table::fread(input = "dataset_mood_smartphone.csv",drop = 1)
length(unique(data$id)) #just 27 users
variable_names <- unique(data$variable)
length(variable_names) # 19 variable

#remove rows where value is NA
data <- data[!is.na(value)]

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
mood_only_dates <- data_new[variable == "mood"][,.(id,time)]
data_new <- data_new[mood_only_dates, on = c(id="id", time="time")]


#changing data from long to wide format for exploratory reasons
library(reshape2)
data_wide <- dcast(data_new, id + time ~ variable, value.var = "value")

#plot to look to the distribution of mood over time
plot(data_wide$time,data_wide$mood)

#check if variables are correlated to mood, only circumplex valence seems to be slightly
cor.test(data_wide$mood, data_wide$circumplex.valence)
