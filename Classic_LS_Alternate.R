library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(tictoc)

#Define my functions
# source("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/honours_project/ranmin.R")
source("E:/Project/honours_project/ranmin.R")

# pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"
pmedfolder <- "E:/Project/TestProblems/pmed"

# problem <- 5

###############################################################################
# Alternate Algorithm to solve p-median problem
# 
# PSUEDOCODE
# 1. Start with solution P* or randomly assign p nodes to P*
# 2. Assign every node to its closest median
# 3. For every median set, find set's median
# 4. Update P* with updated medians
# 5. Return to step 2
# 6. STOP if median set is not updated.
###############################################################################

for (problem in 5:5){
  #load relevant list
  x <- read_rds(str_c(pmedfolder, problem, "_SOL.rds"))
  
  Alt_Solution <-  vector(mode = "numeric", length=50)
  Alt_Percent <- vector(mode = "numeric", length=50)
  Alt_Time <- vector(mode = "numeric", length=50)
  Alt_Iteration <- vector(mode = "numeric", length=50)
  Alt_S_Change <- list()

  for(abc in seq_along(1:50)){
    #STEP 0 - Initialisation
    M <- data_frame(nodes = 1:x$vertices,
                    closest.median = NA)
    
    #Starting solution
    Sstar <- x$random_S
    Pstar <- x$random_solution
    Pstarold <- x$random_solution
    Pstarnew <- vector(mode = "numeric", length=x$p)
    # P <-  M[-Pstar]
    
    # Iteration Parameter
    k = 1
    
    # Sstar improvement tracker
    Sstar.value.change <- Inf
    
    tic()
    while (!setequal(Pstarold, Pstarnew) & !(k > 3 & all(tail(Sstar.value.change, n=3) == 0)) ) {
      
      # Find closest median for every node
      M$closest.median <- Pstar[apply(x$distancematrix[, Pstar], 1, which.min)]
      
      
      # Update median for each set
      for (set.ind in seq_along(1:x$p)){
        
        # vector containing nodes assigned to each median
        set.nodes <- unlist(x = M %>% 
                              filter(closest.median == Pstar[set.ind]) %>% 
                              select(nodes), use.names = F)
        
        # which node should become the new median
        if (length(set.nodes) > 1){
          Pstarnew[set.ind] <- set.nodes[ranmin(apply(x$distancematrix[set.nodes, set.nodes], 
                                                      2, 
                                                      FUN = sum))]
        } else {
          Pstarnew[set.ind] <- set.nodes
        }
        
      }
      
      # Value of updated Sstar
      Sstarnew <- sum(apply(x$distancematrix[, Pstarnew], 1, FUN=min))
      
      # +ive values = improvement (i.e. decrease in value)
      Sstar.value.change[k] <- Sstar - Sstarnew
      Sstar.value.change
      
      # Update P* & S*
      Pstarold <- Pstar
      Pstar <- Pstarnew
      Sstar <- Sstarnew
      k <- k + 1
  } #end of algorithm
    tt <- toc()
    k
    print(str_c("ALTERNATE (RAND) Test problem ",problem, " - rep ", abc ))
    
    
    Alt_Solution[abc] <- Sstar
    Alt_Percent[abc] <- (Sstar-x$opt)/x$opt
    Alt_Time[abc] <- tt$toc-tt$tic
    Alt_Iteration[abc] <- k
    Alt_S_Change[[abc]] <- Sstar.value.change
    
  }
  
  sol = list(problem = x$problem, 
             p = x$p, 
             vertices = x$vertices, 
             edges = x$edges,
             opt = x$opt,
             Alt_Solutions = Alt_Solution,
             Alt_Percents = Alt_Percent,
             Alt_Times = Alt_Time,
             Alt_Iterations = Alt_Iteration,
             Alt_S_Changes = Alt_S_Change)
  
  
  # name=str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/Classic Solutions/Fast Interchange Solutions/FInt", problem, ".rds", sep="")
  name=str_c("E:/Project/Alternate Solutions/Rand_Alt", problem, ".rds", sep="")
  write_rds(sol, path = name)
  
}
