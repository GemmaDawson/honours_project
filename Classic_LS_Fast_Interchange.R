library(tidyverse)
library(stringr)
library(tictoc)

#Define my functions
source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Algorithm1_Greedy/Problem"
problem=1

###################################
# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing The interchange algorithm of Teitz and Bart - Algorithm 3 (page 99 (5))
###################################

Solution <- data_frame()


for (problem in 1:40){
  #load relevant list
  #New greedy solutions are going to a different folder
  x <- read_rds(str_c(pmedfolder, problem, "Greedy1", ".rds"))
  
  
  #STEP 0 - Initialisation
  q <- 0
  a <- 0
  k <- 1
  
  b <- x$vertices - x$p
  M <- as.vector(1:x$vertices, mode = "numeric")
  SRT <- vector(mode = "numeric", length=x$p)
  #Starting solution
  S <- x$greedyS
  Sstar <- x$greedyS
  Pstar <- x$greedymedians
  P <-  M[-Pstar]
  
  tic()
  while(q!=b | S!=Sstar){
    
    #STEP 1
    for (i in seq(from=1, to=x$vertices)){
      u[i] <- min(x$distancematrix[Pstar,i])
      w[i] <- min(x$distancematrix[Pstar[-ranmin(x$distancematrix[Pstar,i])],i])
    }
    Srt <- 0.5
    
    #STEP 2 & 3
    while(q < b & Srt >= 0){
      q <- q + 1
      r <- P[q]
      for(j in 1:length(Pstar)){
        I <- which(x$distancematrix[,Pstar[j]] > u)
        J <- which(x$distancematrix[,Pstar[j]] == u)
        SRT[j] <- sum(pmin(x$distancematrix[I,r],u[I])-u[I]) + sum(pmin(x$distancematrix[J,r],w[J])-u[J])
      }
      minindex <- ranmin(SRT)
      t <- Pstar[minindex]
      Srt <- SRT[minindex]
    }
    
    
    #STEP 4
    if(Srt < 0){
      k <- k + 1
      Sstar <- Sstar + Srt
      Pstar[Pstar==t] <- r
      P[P==r] <- t
    }
    
    #STEP 5
    if(q == b){
      a <- a + 1
      if(S > Sstar){
        q <- 0
        S <- Sstar
      }
    }
  }#end of algorithm
  tt <- toc()
  
  print(str_c("Test problem ",problem))
  
  x$TB_sol <- S
  x$TB_percent <- (S-x$opt)/x$opt
  x$TB_time <- tt$toc-tt$tic
  x$TB_medians <- Pstar
  
  name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions", "TeitzBart", problem, ".rds", sep="")
  write_rds(x, path = name)
  #GO GO Problem next
}

names(Solution) <- c("problem", "p", "vertices", "edges", "optimal", "solution", "time(sec)")
