library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(tictoc)

#Define my functions
# source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
source("E:/Project/honours_project/ranmin.R")

# pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
pmedfolder <- "E:/Project/TestProblems/pmed"
# problem <- 1

###################################
# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing Greedy with CFN - Algorithm 1 (page 97 (3))
###################################

# Sstar <- data_frame()

for (problem in 1:40){
  #load relevant list
  x <- read_rds(str_c(pmedfolder, problem, ".rds"))
  
  Greedy_Solution <-  vector(mode = "numeric", length=100)
  Greedy_Percent <- vector(mode = "numeric", length=100)
  Greedy_Time <- vector(mode = "numeric", length=100)

  for(abc in seq_along(1:50)){
    # STEP 0
    Pstar <- vector(mode = "numeric", length=x$p)
    u <- vector(mode = "numeric", length=x$vertices)
    u[1:x$vertices] <- Inf
    c <- vector(mode = "numeric", length=x$vertices)
    
    tic()  
    for (k in 1:x$p){
      #STEP 1
      #For each vertex that is not already a median, find the minimum of d & c to every other node
      #
      for (j in seq(from=1, to=x$vertices)){
        c[j] <- sum(pmin(x$distancematrix[,j], u))
      }
      if(k>1){
        #Remove the vertices that belong to Pstar
        c[Pstar]<-NA
      }
      
      #STEP 2
      #Find the smallest value in c
      #R's which.min will return smallest index in the case of ties
      #So slight tweak to randomly select a vertex in the case of tie for min(c)
      r <- ranmin(y=c)
      
      #Add cost to S
      #CHECK IF WORKING BEFORE RUNNING    
      S <- c[r]
      
      #Step 3
      #Add vertex to Pstar
      Pstar[k] <- r
      
      #Step 4
      #Update u
      u <- pmin(x$distancematrix[,r], u)
    }
    print(str_c("Test problem ",problem, " - rep ", abc ))
    tt <- toc()
    
    Greedy_Solution[abc] <- S
    Greedy_Percent[abc] <- (S-x$opt)/x$opt
    Greedy_Time[abc] <- tt$toc-tt$tic
  
  }
  
  sol = list(problem = x$problem, 
           p = x$p, 
           vertices = x$vertices, 
           edges = x$edges,
           opt = x$opt,
           Greedy_Soultions = Greedy_Solution,
           Greedy_Percents <- Greedy_Percent,
           Greedy_Times <- Greedy_Time)


# name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions", "Greedy", problem, ".rds", sep="")
name=str_c("E:/Project/Greedy Solutions/Greedy", problem, ".rds", sep="")
write_rds(sol, path = name)

}
