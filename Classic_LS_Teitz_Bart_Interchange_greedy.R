library(tidyverse)
library(stringr)
library(tictoc)

#Define my functions
# source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
source("E:/Project/honours_project/ranmin.R")

# pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
pmedfolder = "E:/Project/TestProblems/pmed"

# problem=1

###################################
# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing The interchange algorithm of Teitz and Bart - Algorithm 3 (page 99 (5))
###################################

for (problem in 1:40){
  #load relevant list
  x <- read_rds(str_c(pmedfolder, problem, "_SOL.rds"))
  
  TB_Solution <-  vector(mode = "numeric", length=50)
  TB_Percent <- vector(mode = "numeric", length=50)
  TB_Time <- vector(mode = "numeric", length=50)
  TB_Iteration <- vector(mode = "numeric", length=50)
  TB_S_Change <- list()

  for(abc in seq_along(1:50)){
    
    #STEP 0 - Initialisation
    q <- 0
    a <- 0
    k <- 1
    
    b <- x$vertices - x$p
    M <- c(1:x$vertices)
    SRT <- rep(0, length=x$p)
    u <- rep(0, length=x$vertices)
    w <- rep(0, length=x$vertices)
    
    #Starting solution
    S <- x$random_S
    Sstar <- x$random_S
    Pstar <- x$random_solution
    P <-  M[-Pstar]
    
    # Sstar improvement tracker
    Sstar.value.change <- Inf
    
    tic()
    while(q!=b | S!=Sstar){
      
      #STEP 1
      u <- apply(x$distancematrix[Pstar,], 2, min)
      
      r.min <- Pstar[apply(x$distancematrix[Pstar,], 2, ranmin)]
      r.min <- cbind(r.min, c(1:ncol(x$distancematrix[Pstar,])))
      dm <- x$distancematrix
      dm[r.min] <- NA
      w <- apply(dm[Pstar,], 2, min, na.rm = T)
      
      Srt <- 0.5
      
      #STEP 2 & 3
      while(q < b & Srt >= 0){
        q <- q + 1
        r <- P[q]
        
        I <- apply(x$distancematrix[,Pstar] > u, 2, which)
        J <- apply(x$distancematrix[,Pstar] == u, 2, which)
        
        tic()
        for(j in seq_along(Pstar)){
          SRT[j] <- sum(pmin(x$distancematrix[I[[j]],r], u[I[[j]]]) - u[I[[j]]]) + 
            sum(pmin(x$distancematrix[J[[j]],r], w[J[[j]]]) - u[J[[j]]])
        }
        toc()
        
        minindex <- ranmin(SRT)
        t <- Pstar[minindex]
        Srt <- SRT[minindex]
      }
      
      
      #STEP 4
      if(Srt < 0){
        k <- k + 1
        Sstar <- Sstar + Srt
        Sstar.value.change[k] <- Srt
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
    print(str_c("T&B (greedy) Test Problem ", problem, " - rep ", abc ))
    
    
    TB_Solution[abc] <- S
    TB_Percent[abc] <- (S-x$opt)/x$opt
    TB_Time[abc] <- tt$toc-tt$tic}
    TB_Iteration[abc] <- k
    TB_S_Change[[abc]] <- Sstar.value.change
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             TB_Soultions = TB_Solution,
             TB_Percents = TB_Percent,
             TB_Times = TB_Time,
             TB_Iterations = TB_Iteration,
             TB_S_Changes = TB_S_Change
             )
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/TB Interchange Solutions/TB", "TB", problem, ".rds", sep="")
  name=str_c("E:/Project/TB Interchange Solutions/TB_greedy", problem, ".rds", sep="")
  write_rds(sol, path = name)
    
  }
  


