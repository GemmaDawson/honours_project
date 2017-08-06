# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing Fast Greedy Algorithm 2 (page 98 (4))

#######################################

library(tidyverse)
library(stringr)
library(tictoc)

setwd("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/")
pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"

Sstar <- data_frame()

for (i in 1:40){
  #load relevant list
  datafile <- str_c(pmedfolder, i, ".rds")
  x <- read_rds(datafile)
  rm(datafile)
  
  # STEP 0
  Pstar <- vector(mode = "numeric", length=x$p)
  
  c <- data_frame()
  c[1:x$vertices,1] <- 0
  c[1:x$vertices,2] <- 0
  
  u <- data_frame()
  u[1:x$vertices,1] <- 0
  u[1:x$vertices,2] <- Inf
  
  I <- data_frame(1:x$vertices)
  
  tic()  
  for (k in 1:x$p){
    #STEP 1
    #For each vertex that is not already a median, find the minimum of d & c to every other node
    #^Change this explaination
    
    for (j in seq_along(1:nrow(I))){
      v = as.numeric(I[j,1])
      c[v,k+1] <- sum(pmin(x$distancematrix[,v], u$V2)) + c[v,k] - sum(pmin(x$distancematrix[,v], u$V1))
    }
    c[,k+2]=c[,k+1]
    
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
    I <- data_frame(which(x$distancematrix[,r] < u[,1]))
    
  }
  print(i)
  tt <- toc()
  Sstar <- data_frame(x$problem, x$p, x$vertices, x$edges, x$opt, S, tt$toc-tt$tic)
  #^^^Check this works & then add to Greedy1
}

names(Sstar) <- c("problem", "p", "vertices", "edges", "optimal", "solution", "sec")
