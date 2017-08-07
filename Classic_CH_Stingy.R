library(tidyverse)
library(stringr)
library(tictoc)

pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"

###################################
# MLAD07 page 4 (930)
# Stingy Algorithm
# Start with all nodes as medians and remove one at a time
###################################

Sstar <- data_frame()

for (problem in 1:40){
  #load relevant list
  x <- read_rds(str_c(pmedfolder, problem, ".rds"))
  
  # STEP 0
  # Start with every node included in the median set
  Pstar <- as.vector(1:x$vertices)
  u <- vector(mode = "numeric", length=x$vertices)
  c <- vector(mode = "numeric", length=x$vertices)
  
  tic()  
  for (k in 1:(x$vertices-x$p)){
    #STEP 1
    #For each node that is currently in the median set, 
    #find the sum of the max/min
    #of the distance from given node to every other node still considered a median
    #of the distance from given node to currently assigned median
    #distance is either the 
    # the max of dist to every other node & current dist
    #
    for (j in seq_along(Pstar[!is.na(Pstar)])){
      c[j] <- sum(pmin(x$distancematrix[,j], u))
    }

    
    #STEP 2
    #Find the largest value in c
    #R's which.max will return largest index in the case of ties
    #So slight tweak to randomly select a vertex in the case of tie for min(c)
    r <- ranmax(x=c)
    
    #Step 3
    #Remove vertex to Pstar
    Pstar[k] <- r
    
    #Step 4
    #Update u
    u <- pmin(x$distancematrix[,r], u)
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
