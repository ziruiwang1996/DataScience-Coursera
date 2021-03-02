outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome) #look at the first few rows
ncol(outcome) #the number of rows with the nrow function
names(outcome) #the names of each column
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11]) #histogram of the 30-day death rates from heart attack

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!any(state==data$State)){
    stop('invalid state')
  }
  else if (!((outcome=='heart attack')|(outcome=='heart failure')|(outcome=='pneumonia'))){
    stop('invalid outcome')
  }
  ## Return hospital name in that state with lowest 30-day death 
  #s <- split(data, data$State)
  s <- subset(data, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  min_row <- which(as.numeric(s[ ,colnum]) == min(as.numeric(s[ ,colnum]), na.rm = TRUE))
  hospitals <- sort(s[min_row,2])
  return(hospitals[1])
}