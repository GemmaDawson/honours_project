# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing Fast Greedy Algorithm 2 (page 98 (4))

#######################################

library(tidyverse)
library(stringr)
library(tictoc)

#Define my functions
# source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
source("E:/Project/honours_project/ranmin.R")

# pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
pmedfolder = "E:/Project/TestProblems/pmed"
problem <- 40

for (problem in 1:40){
  #load relevant list
  
  x <- read_rds(str_c(pmedfolder, problem, ".rds"))
  
  FGreedy_Solution <-  vector(mode = "numeric", length=50)
  FGreedy_Percent <- vector(mode = "numeric", length=50)
  FGreedy_Time <- vector(mode = "numeric", length=50)
  FGreedy_S_Change <- list()
  
  for(abc in seq_along(1:10)){
    # STEP 0
    M <- c(1:x$vertices)
    Pstar <- 0
    P <- M
    
    c <- data_frame(k=rep(0, length = x$vertices))
    
    u <- data_frame('k0' = rep(0, length = x$vertices),
                    'k1' = rep(Inf, length = x$vertices))
    
    
    I <- c(1:x$vertices)
    
    Sstar.value.change <- Inf
    
    tic()  
    for (k in 1:x$p){
      #STEP 1
      #For each node that was reassigned in the previous iteration,
      #Assess whether there could be a further decrease
      
      c[,k+1] <- c[,k] 

        m1 <- pmin(x$distancematrix[I,I], u$k1[I])
        m2 <- pmin(x$distancematrix[I,I], u$k0[I])
        if(length(I) > 1){
          s1 <- apply(m1, MARGIN=2, sum)
          s2 <- apply(m2, MARGIN=2, sum)
        } else {
          s1 <- m1
          s2 <- m2
        }
        
        c[I,k+1] <- s1 + c[I,k] - s2

      

      
      #STEP 2
      #Find the smallest value in c
      #R's which.min will return smallest index in the case of ties
      #So slight tweak to randomly select a vertex in the case of tie for min(c)
      r <- P[ranmin(c[P,k+1])]
      
      
      #Add cost to S
      S <- as.numeric(c[r,k+1])
      Sstar.value.change[k+1] <- S
      
      #Step 3
      # Add node to Pstar
      # Remove node from free set
      Pstar[k] <- r
      P <- M[-Pstar]
      
      #Step 4
      #Update u
      u[,1] <- u[,2]
      u[,2] <- pmin(u$k0, x$distancematrix[,r])
      
      #Update I
      #I contains nodes that have been reassigned
      I <- as.vector(which(x$distancematrix[,r] < u[,1]))
      
    }
    tt <- toc()
    print(str_c("FAST GREEDY Test Problem ",problem, " - rep ", abc ))
    
    
    FGreedy_Solution[abc] <- S
    FGreedy_Percent[abc] <- (S-x$opt)/x$opt
    FGreedy_Time[abc] <- tt$toc-tt$tic
    FGreedy_S_Change[[abc]] <- Sstar.value.change
    
  }
  
  
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             FastGreedy_Solutions = FGreedy_Solution,
             FastGreedy_Percents = FGreedy_Percent,
             FastGreedy_Times = FGreedy_Time,
             Sstar.value.changes = Sstar.value.change)
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/Fast Greedy Solutions/Fast_Greedy", problem, ".rds", sep="")
  name=str_c("E:/Project/Fast Greedy Solutions/Fast_Greedy", problem, ".rds", sep="")
  write_rds(sol, path = name)
  
}

