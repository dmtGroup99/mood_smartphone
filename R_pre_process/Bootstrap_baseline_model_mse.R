#use bootstrap to find empirical cdf of baseline mse. This can be used to estimate sd and/or variance of mse estimator

library(data.table)
data <- data.table::fread(input = "../RNN_data_version2.csv",drop = 1)

error <- data$mood5 - data$target

mse <- function(x){
  return(sum(x^2)/(length(x)))
}

bootstrap <- function(data, statistic, B){
  result <- numeric(B)
  for(i in 1:B){
    x <- sample(data, length(data), replace=T)
    result[i] = statistic(x)
  }
  return(result)
}

mean(mse(error))
ecdf_error <- bootstrap(error, mse, 1000)
hist(ecdf_error)
var(ecdf_error)
boxplot(ecdf_error)
