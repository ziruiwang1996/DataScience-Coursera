rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!((outcome=='heart attack')|(outcome=='heart failure')|(outcome=='pneumonia'))){
    stop('invalid outcome')
  }
  ## For each state, find the hospital of the given rank
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  data[ ,colnum] <- as.numeric(data[ ,colnum])
  data <- data[!is.na(data[,colnum]),]
  #split data frame by state
  splitdata <- split(data, data$State)
  result = lapply(splitdata, function(x, num){
    x = x[order(x[,colnum], x$Hospital.Name),]
    if (class(num) =='character'){
      if (num == 'best'){
        return(x$Hospital.Name[1])
      }
      else if (num == 'worst'){
        return(x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return(x$Hospital.Name[num])
    }
  }, num)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return ( data.frame(hospital=unlist(result), state=names(result)) )
}