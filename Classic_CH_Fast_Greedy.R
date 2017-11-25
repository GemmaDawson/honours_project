# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing Fast Greedy Algorithm 2 (page 98 (4))

#######################################

library(tidyverse)
library(stringr)
library(tictoc)

#Define my functions
source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
# source("E:/Project/honours_project/ranmin.R")

pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
# pmedfolder = "E:/Project/TestProblems/pmed"

for (problem in 1:40){
  #load relevant list
  
  x <- read_rds(str_c(pmedfolder, problem, ".rds"))
  
  FGreedy_Solution <-  vector(mode = "numeric", length=100)
  FGreedy_Percent <- vector(mode = "numeric", length=100)
  FGreedy_Time <- vector(mode = "numeric", length=100)
  
  for(abc in seq_along(1:100)){
    # STEP 0
    Pstar <- vector(mode = "numeric", length=x$p)
    
    c <- data_frame()
    c[1:x$vertices,1] <- 0
    
    u <- data_frame()
    u[1:x$vertices,1] <- 0
    u[1:x$vertices,2] <- Inf
    
    I <- data_frame(1:x$vertices)
    
    tic()  
    for (k in 1:x$p){
      #STEP 1
      #For each node that was reassigned in the previous iteration,
      #Assess whether there could be a further decrease
      
      c[,k+1] <- c[,k] 
      
      for (j in seq_along(1:nrow(I))){
        v = as.numeric(I[j,1])
        c[v,k+1] <- sum(pmin(x$distancematrix[,v], u$V2)) + c[v,k] - sum(pmin(x$distancematrix[,v], u$V1))
      }
      
      #Remove all nodes that are assigned as facilities
      c[Pstar,k+1] <- NA
      
      #STEP 2
      #Find the smallest value in c
      #R's which.min will return smallest index in the case of ties
      #So slight tweak to randomly select a vertex in the case of tie for min(c)
      r <- ranmin(c[,k+1])
      
      #Add cost to S
      S <- c[r,k+1]
      
      #Step 3
      #Add vertex to Pstar
      Pstar[k] <- r
      
      #Step 4
      #Update u
      u[,1] <- u[,2]
      u[,2] <- pmin(u$V1, x$distancematrix[,r])
      
      #Update I
      #I contains nodes that have been reassigned
      I <- data_frame(which(x$distancematrix[,r] < u[,1]))
      
    }
    tt <- toc()
    print(str_c("Test problem ",problem, " - rep ", abc ))
    
    
    FGreedy_Solution[abc] <- S
    FGreedy_Percent[abc] <- (S-x$opt)/x$opt
    FGreedy_Time[abc] <- tt$toc-tt$tic
    
  }
  
  
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             FastGreedy_Soultions = FGreedy_Solution,
             FastGreedy_Percents <- FGreedy_Percent,
             FastGreedy_Times <- FGreedy_Time)
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/Fast Greedy Solutions/Fast_Greedy", problem, ".rds", sep="")
  name=str_c("E:/Project/Fast Greedy Solutions/Fast_Greedy", problem, ".rds", sep="")
  write_rds(sol, path = name)
  
}

