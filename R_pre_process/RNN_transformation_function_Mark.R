#input is data_wide_new from file.R and window range
#output is raw RNN input data
RNN_data <- function(dat, days){
  ids <- unique(dat$id)
  result <- NULL
  id_vec <- vector()
  for(k in ids){
    print(k)
    x <- dat[dat$id == k,]
    temp <- NULL
    start <- min(x$time)
    end <- max(x$time) - days
    while(start < end){
      vec <- vector()
      id_vec <- c(id_vec,k)
      for(j in 0:(days - 1)){
        if(any(x$time == start + j)){
          vec <- unname(unlist(c(vec, x[x$time == start + j, 3:length(x)])))
        }else{
          vec <- c(vec, rep(NA, length(x) - 2))
        }
        vec <- c(vec, as.numeric(format(start + j, format="%w")),
                 as.numeric(format(start + j, format="%d")), as.numeric(format(start + j, format="%m")))
      }
      if(any(x$time == start + days)){
        vec <- unname(unlist(c(vec, x[x$time == start + days, 'mood'])))
      }else{
        vec <- c(vec, NA)
      }
      result <- rbind(result,vec)
      start <- start + 1
    }
    result <- rbind(result,temp)
  }
  result <- cbind(id_vec, data.frame(result))
  return(result)
}