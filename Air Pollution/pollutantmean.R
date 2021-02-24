pollutantmean <- function(directory, pollutant, id = 1:332 ){
  means <- c()
  for (i in id){
    path <- paste(directory, '/', sprintf("%03d", i), '.csv', sep='')
    data <- read.csv(path)
    pollutant_data <- data[pollutant]
    cleaned_data <- pollutant_data[!is.na(pollutant_data)]
    means <- c(means, mean(cleaned_data))
  }
  mean(means)
}

complete <- function(directory, id= 1:332){
  outputs <- data.frame(id = c(), nobs = c())
  for (i in id){
    path = paste(directory, '/', sprintf("%03d", i), '.csv', sep='')
    data <- read.csv(path)
    row_num = nrow(data[complete.cases(data),])
    outputs <- rbind(outputs, data.frame(id = i, nobs = row_num))
  }
  outputs
}

corr <- function(directory, threshold = 0) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- vector(mode = "numeric", length = 0)
  for (i in 1:length(files_full)) {
    moni_i <- read.csv(files_full[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      dat <- c(dat, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  dat
}
