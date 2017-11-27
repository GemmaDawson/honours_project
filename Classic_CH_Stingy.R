library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(tictoc)
p_load(foreach)
p_load(R.utils)

#Define my functions
# source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
source("E:/Project/honours_project/ranmin.R")
source("C:/Users/Gemma.Dawson/Documents/GitHub/honours_project/function_stingy.R")

# pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
pmedfolder = "E:/Project/TestProblems/pmed"

problem <- 1



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
  
  Stingy_Solution <-  vector(mode = "numeric", length=50)
  Stingy_Percent <- vector(mode = "numeric", length=50)
  Stingy_Time <- vector(mode = "numeric", length=50)
  Stingy_S_Change <- list()
  
  for(abc in seq_along(1:50)){
    # STEP 0
    # Start with every node included in the median set
    Pstar <- c(1:x$vertices)
    P <- 0
    c <- rep(0, length=x$vertices)
    Sstar.value.change <- Inf

    tic()
    for (k in 1:(x$vertices-x$p)){
      #STEP 1
      #For each node that is currently in the median set,
      # find the total cost if this node were to be excluded from the median set
      # ie find distance for all nodes to nearest node in set excl. this node and sum
      # look at min between dist to current closest median and dist to closest median if node j in
      # Pstar is removed from Pstar

      c <- foreach(i=Pstar, .combine = 'c') %do% sum(apply(x$distancematrix[-i,], 2, min))

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
      S <- sum(apply(x$distancematrix[Pstar,], 2, min))
      Sstar.value.change[k] <- S

    }
    print(str_c("STINGY Test Problem ",problem, " - rep ", abc ))
    
    tt <- toc()
    
    if (tt$toc - tt$tic > 120 & abc > 4){
      break
    }

    Stingy_Solution[abc] <- S
    Stingy_Percent[abc] <- (S-x$opt)/x$opt
    Stingy_Time[abc] <- tt$toc-tt$tic
    Stingy_S_Change <- Sstar.value.change
    
  }
  
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             Stingy_Soultions = Stingy_Solution,
             Stingy_Percents = Stingy_Percent,
             Stingy_Times = Stingy_Time,
             Stingy_S_Changes = Stingy_S_Change)
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/Stingy Solutions/Stingy", problem, ".rds", sep="")
  name=str_c("E:/Project/Stingy Solutions/Stingy", problem, ".rds", sep="")
  write_rds(sol, path = name)
  
}



  
  
