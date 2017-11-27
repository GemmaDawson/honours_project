stingy_func <- function(Pstar, P, c, Sstar.value.change, n, p, dm)
for (k in 1:(n - p)){
  #STEP 1
  #For each node that is currently in the median set,
  # find the total cost if this node were to be excluded from the median set
  # ie find distance for all nodes to nearest node in set excl. this node and sum
  # look at min between dist to current closest median and dist to closest median if node j in 
  # Pstar is removed from Pstar
  
  c <- foreach(i=Pstar, .combine = 'c') %do% sum(apply(dm[-i,], 2, min))
  
  #STEP 2
  #Find the smallest value in c
  # i.e. find the node that has a close median that is not itself
  r <- Pstar[ranmin(y=c)]
  
  #Step 3
  #Update P
  #Remove vertex from Pstar
  P[k] <- r
  Pstar <- Pstar[Pstar!=r]
  
  
  #Step 4
  #Update S
  S <- sum(apply(dm[Pstar,], 2, min))
  Sstar.value.change[k] <- S
  
  stingy_output <- list(P,
                        Pstar,
                        S,
                        Sstar.value.change)
  S
  
}

# returnValue()
