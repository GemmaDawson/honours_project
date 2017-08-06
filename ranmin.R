ranmin <- function(x)
  if(length(which(x==min(x, na.rm = T)))==1){
    which.min(x)
  } else{
    sample(which(x==min(x, na.rm = T)),1)  
  }
