ranmax <- function(x)
  if(length(which(x==max(x, na.rm = T)))==1){
    which.max(x)
  } else{
    sample(which(x==max(x, na.rm = T)),1)  
  }