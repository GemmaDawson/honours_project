ranmin <- function(y)
  if(length(which(y==min(y, na.rm = T)))==1){
    which.min(as.matrix(y))
  } else{
    sample(which(y==min(y, na.rm = T)),1)  
  }
