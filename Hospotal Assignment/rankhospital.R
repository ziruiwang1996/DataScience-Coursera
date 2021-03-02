rankhospital <- function(state, outcome, num = "best") { 
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!any(state==data$State)){
    stop('invalid state')
  }
  else if (!((outcome=='heart attack')|(outcome=='heart failure')|(outcome=='pneumonia'))){
    stop('invalid outcome')
  }
  ## Return hospital name in that state with the given rank ## 30-day death rate
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
  #order first by outcome(colnum), if tie then by hospital name
  s2 <- s[order(as.numeric(s[ ,colnum]),s[,2]), ] 
  #remove missing values
  s3 <- s2[(!is.na(s2[ ,colnum])),]
  if(num == "best"){
    num <- 1
  }            
  else if (num == "worst"){
    num <- nrow(s3)
  }      
  return(s3[num,2])
}