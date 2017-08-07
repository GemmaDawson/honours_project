library(tidyverse)
library(stringr)
library(tictoc)

pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"

###################################
# Using Whitaker's A Fast Algorithm For The Greedy Interchange For Large-Scale Clustering And Median Location Problems
# Implementing Greedy with CFN - Algorithm 1 (page 97 (3))
###################################

Sstar <- data_frame()

for (problem in 1:40){
  #load relevant list
  datafile <- str_c(pmedfolder, problem, ".rds")
  x <- read_rds(datafile)
  rm(datafile)
  
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
    r <- ranmin(x=c)
    
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
print(str_c("Test problem ",problem))
tt <- toc()

Sstar[problem,1] <- x$problem
Sstar[problem,2] <- x$p
Sstar[problem,3] <- x$vertices
Sstar[problem,4] <- x$edges
Sstar[problem,5] <- x$opt
Sstar[problem,6] <- S
Sstar[problem,7] <- tt$toc-tt$tic

x$greedymedians <- Pstar
x$greedyS <- S
name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Algorithm1_Greedy/Problem", problem, "Greedy1", ".rds", sep="")
write_rds(x, path = name)

# Sstar[problem,8] <- Pstar2
 }
names(Sstar) <- c("problem", "p", "vertices", "edges", "optimal", "solution", "time(sec)")
# "PMedians"
write_rds(Sstar, path = "Algorithm1_Greedy/Greedy1.rds")
