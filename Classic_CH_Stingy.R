library(tidyverse)
library(stringr)
library(tictoc)

pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"

###################################
# MLAD07 page 4 (930)
# Stingy Algorithm
# Start with all nodes as medians and remove one at a time
# The median 
###################################

# Sstar <- data_frame()

for (problem in 1:40){
  #load relevant list
  x <- read_rds(str_c(pmedfolder, problem, ".rds"))
  
  # STEP 0
  # Start with every node included in the median set
  Pstar <- as.vector(1:x$vertices)
  P <- NA
  u <- vector(mode = "numeric", length=x$vertices)
  u[1:x$vertices] <- 0
  c <- vector(mode = "numeric", length=x$vertices)

  tic()  
  for (k in 1:(x$vertices-x$p)){
    #STEP 1
    #For each node that is currently in the median set,
    # find the total cost if this node were to be excluded from the median set
    # ie find distance for all nodes to nearest node in set excl. this node and sum
    # look at min between dist to current closest median and dist to closest median if node j in 
    # Pstar is removed from Pstar
    
    for (j in seq_along(Pstar)){
      PPstar <- Pstar[Pstar!=Pstar[j]]
      c[Pstar[j]] <- sum(apply(x$distancematrix[PPstar,],2,min))
    }

    
    #STEP 2
    #Find the largest value in c
    r <- ranmin(y=c)
    
    #Step 3
    #Update P
    #Remove vertex from Pstar
    P[k] <- Pstar[Pstar==r]
    Pstar <- Pstar[Pstar!=r]
    
      
    #Step 4
    #Update u
    u <- apply(x$distancematrix[Pstar,],2,min)
    #Update S
    S <- c[r]
    #update c
    c[r] <- NA
    
  }
  print(str_c("Test problem ",problem))
  tt <- toc()
  
  Sstar[problem,1] <- x$problem
  Sstar[problem,2] <- x$p
  Sstar[problem,3] <- x$vertices
  Sstar[problem,4] <- x$edges
  Sstar[problem,5] <- x$opt
  Sstar[problem,6] <- S
  Sstar[problem,7] <- tt$toc-tt$tic
  
  x$greedymedians <- Pstar
  x$greedyS <- S
  name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Algorithm1_Greedy/Problem", problem, "Greedy1", ".rds", sep="")
  write_rds(x, path = name)
  
  # Sstar[problem,8] <- Pstar2
}
names(Sstar) <- c("problem", "p", "vertices", "edges", "optimal", "solution", "time(sec)")
# "PMedians"
write_rds(Sstar, path = "Algorithm1_Greedy/Greedy1.rds")
