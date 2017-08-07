library(tidyverse)
library(stringr)
library(tictoc)

pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Algorithm1_Greedy/Problem"

###################################
# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing The interchange algorithm of Teitz and Bart - Algorithm 3 (page 99 (5))
###################################

Solution <- data_frame()

for (problem in 1:40){
  #load relevant list
  datafile <- str_c(pmedfolder, problem, "Greedy1", ".rds")
  x <- read_rds(datafile)
  rm(datafile)
  
  #STEP 0 - Initialisation
  q <- 0
  a <- 0
  k <- 1
  u <- vector(mode = "numeric", length=x$vertices)
  w <- vector(mode = "numeric", length=x$vertices)
  b <- x$vertices - x$p
  M <- as.vector(1:x$vertices, mode = "numeric")
  SRT <- vector(mode = "numeric", length=x$p)
  #Starting solution
  S <- x$greedyS
  Sstar <- x$greedyS
  Pstar <- x$greedymedians
  P <-  M[-Pstar]
  Srt <- 0.5
  
  
  
  # tic()
  # u1 <- apply(x$distancematrix[Pstar,], 2, min)
  # w1 <- apply(x$distancematrix[Pstar[-which.min(x$distancematrix[Pstar,])],], 2, min)
  # toc()
  tic()
  while(q!=b | S!=Sstar){
    
    Srt <- 0.5
    
    #STEP 1
    for (i in seq(from=1, to=x$vertices)){
      u[i] <- min(x$distancematrix[Pstar,i])
      w[i] <- min(x$distancematrix[Pstar[-ranmin(x$distancematrix[Pstar,i])],i])
    }
    
    
    #STEP 2 & 3
    # check q
    while(q < b & Srt > 0){
      q <- q + 1
      r <- P[q]
      for(j in 1:length(Pstar)){
        I <- which(x$distancematrix[,Pstar[j]] > u)
        J <- which(x$distancematrix[,Pstar[j]] == u)
        SRT[j] <- sum(pmin(x$distancematrix[I,r],u[I])-u[I]) + sum(pmin(x$distancematrix[J,r],w[J])-u[J])
      }
      t <- Pstar[ranmin(SRT)]
      Srt <- SRT[ranmin(SRT)]
      # print(str_c(q, "-", Srt))
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
        S <- Sstar
        q <- 0
      }
    }
  }#end of algorithm
  tt <- toc()
  
  print(str_c("Test problem ",problem))
  
  Solution[problem,1] <- x$problem
  Solution[problem,2] <- x$p
  Solution[problem,3] <- x$vertices
  Solution[problem,4] <- x$edges
  Solution[problem,5] <- x$opt
  Solution[problem,6] <- S
  Solution[problem,7] <- tt$toc-tt$tic
#GO GO Problem next
}

names(Solution) <- c("problem", "p", "vertices", "edges", "optimal", "solution", "time(sec)")
