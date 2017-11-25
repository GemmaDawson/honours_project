library(tidyverse)
library(stringr)
library(tictoc)

#Define my functions
# source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
source("E:/Project/honours_project/ranmin.R")

# pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
pmedfolder = "E:/Project/TestProblems/pmed"

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
  
  Stingy_Solution <-  vector(mode = "numeric", length=100)
  Stingy_Percent <- vector(mode = "numeric", length=100)
  Stingy_Time <- vector(mode = "numeric", length=100)
  
  for(abc in seq_along(1:100)){
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
    print(str_c("Test problem ",problem, " - rep ", abc ))
    tt <- toc()
    
    Stingy_Solution[abc] <- S
    Stingy_Percent[abc] <- (S-x$opt)/x$opt
    Stingy_Time[abc] <- tt$toc-tt$tic
    
  }
  
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             Stingy_Soultions = Stingy_Solution,
             Stingy_Percents <- Stingy_Percent,
             Stingy_Times <- Stingy_Time)
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/Stingy Solutions/Stingy", problem, ".rds", sep="")
  name=str_c("E:/Project/Stingy Solutions/Stingy", problem, ".rds", sep="")
  write_rds(sol, path = name)
  
}
