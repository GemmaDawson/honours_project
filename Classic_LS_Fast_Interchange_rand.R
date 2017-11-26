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
# Implementing The fast interchange algorithm - Algorithm 4 (page 99 (5))
###################################

for (problem in 1:40){
  #load relevant list
  x <- read_rds(str_c(pmedfolder, problem, "_SOL.rds"))
  
  FInt_Solution <-  vector(mode = "numeric", length=50)
  FInt_Percent <- vector(mode = "numeric", length=50)
  FInt_Time <- vector(mode = "numeric", length=50)
  FInt_Iteration <- vector(mode = "numeric", length=50)
  FInt_S_Change <- list()
  
  for(abc in seq_along(1:50)){
    
    #STEP 0 - Initialisation
    k <- 1
    z <- 0
    q <- 0
    
    b <- x$vertices - x$p
    M <- as.vector(1:x$vertices, mode = "numeric")
    SRT <- vector(mode = "numeric", length=x$p)
    u <- data_frame()
    u[1:x$vertices,1] <- 0
    u[1:x$vertices,2] <- NA
    w <- data_frame()
    w[1:x$vertices,1] <- 0
    w[1:x$vertices,2] <- NA
    
    #Starting solution
    S <- x$random_S
    Sstar <- x$random_S
    Pstar <- x$random_solution
    P <-  M[-Pstar]
    
    # Sstar improvement tracker
    Sstar.value.change <- Inf
    
    for (i in seq(from=1, to=x$vertices)){
      u[i,k] <- min(x$distancematrix[Pstar,i])
      w[i,k] <- min(x$distancematrix[Pstar[-ranmin(x$distancematrix[Pstar,i])],i])
    }
    
    tic()
    while(z!=b | S!=Sstar){
      
      Srt <- 0.5
      
      #STEP 1
      while(z < b & Srt >= 0){
        
        z <- z + 1
        q <- q + 1
        r <- P[q]
        for(j in 1:length(Pstar)){
          I <- which(x$distancematrix[,r] < u$V1)
          K <- which(x$distancematrix[,r] >= u$V1 & x$distancematrix[,Pstar[j]] == u$V1)
          SRT[j] <- sum(x$distancematrix[I,r]-u$V1[I]) + sum(pmin(x$distancematrix[K,r],w$V1[K])-u$V1[K])
        }
        minindex <- ranmin(SRT)
        t <- Pstar[minindex]
        Srt <- SRT[minindex]
        
      }
      
      if(Srt < 0){
        k <- k + 1
        Sstar <- Sstar + Srt
        Pstar[Pstar==t] <- r
        P[P==r] <- t
        z <- 0
        
        #If (dit > ui^k-1)
        GI <- which(x$distancematrix[,t]>u$V1)
        u$V2[GI] <- pmin(x$distancematrix[GI,r],u$V1[GI])
        
        cond1 <- x$distancematrix[GI,r] <= u$V1[GI]
        w$V2[GI][cond1] <- u$V1[GI][cond1]
        cond2 <- x$distancematrix[GI,r] > u$V1[GI] & x$distancematrix[GI,t] > w$V1[GI] 
        w$V2[GI][cond2] <- pmin(x$distancematrix[GI,r][cond2] , w$V1[GI][cond2])
        cond3 <- x$distancematrix[GI,r] > u$V1[GI] & x$distancematrix[GI,t] == w$V1[GI]
        
        if(length(GI)>1){
          dc <- apply(X = x$distancematrix[GI,Pstar], MARGIN = 1, FUN = ranmin)[cond3]
        }else{
          dc <- ranmin(y = x$distancematrix[GI,Pstar])
        }
        
        
        dr <- GI[cond3]
        for(l in seq_along(dr)){
          w$V2[dr[l]] <- min(x$distancematrix[dr[l],Pstar[-dc[l]]])
        }
          
        
        #Else (dit = ui^k-1)
        EI <- which(x$distancematrix[,t]==u$V1)
        u$V2[EI] <- pmin(x$distancematrix[EI,r], w$V1[EI])
        
        cond4 <- x$distancematrix[EI,r] <= w$V1[EI]
        w$V2[EI][cond4] <- w$V1[EI][cond4]
        cond5 <- x$distancematrix[EI,r] > w$V1[EI]
        
        if(length(EI)>1){
          dc <- apply(X = x$distancematrix[EI,Pstar], MARGIN = 1, FUN = ranmin)[cond5]
        }else{
          dc <- ranmin(y = x$distancematrix[EI,Pstar])
        }
        
        
        dr <- EI[cond5]
        for(l in seq_along(dr)){
          w$V2[dr[l]] <- min(x$distancematrix[dr[l],Pstar[-dc[l]]])
        }
        
        u$V1 <- u$V2
        w$V1 <- w$V2
      
      }else{
        a <- q/b
        S <- Sstar
      }
       } #end of algorithm
    tt <- toc()
    print(str_c("FInt (rand) Test Problem ",problem, " - rep ", abc ))
    
    
    FInt_Solution[abc] <- S
    FInt_Percent[abc] <- (S-x$opt)/x$opt
    FInt_Time[abc] <- tt$toc-tt$tic
    FInt_Iteration[abc] <- k
    FInt_S_Change[[abc]] <- Sstar.value.change
    
    }
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             FInt_Solutions = FInt_Solution,
             FInt_Percents = FInt_Percent,
             FInt_Times = FInt_Time,
             FInt_Iterations = FInt_Iteration,
             FInt = FInt_S_Change)
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/Fast Interchange Solutions/FInt", problem, ".rds", sep="")
  name=str_c("E:/Project/Fast Interchange Solutions/FInt_rand", problem, ".rds", sep="")
  write_rds(sol, path = name)
  
}
