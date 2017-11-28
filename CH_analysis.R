library(pacman)
p_load(tidyverse)
p_load(stringr)

Problem_summary <- data_frame(problem = c(1:40))

Greedy_times <- data_frame(iteration = c(1:50))
Greedy_solutions <- data_frame(iteration = c(1:50))
Greedy_accuracy <- data_frame(iteration = c(1:50))

Fast_times <- data_frame(iteration = c(1:50))
Fast_solutions <- data_frame(iteration = c(1:50))
Fast_accuracy <- data_frame(iteration = c(1:50))

# Stingy_times <- data_frame(iteration = c(1:50))
# Stingyt_solutions <- data_frame(iteration = c(1:50))
# Stingy_accuracy <- data_frame(iteration = c(1:50))

for (problem in 1:40){
  
  sol <- read_rds(str_c("E:/Project/Greedy Solutions/Greedy", problem, ".rds", sep=""))
  
  Greedy_times[, (problem+1)] <- sol$Greedy_Times
  Greedy_solutions[, (problem+1)] <- sol$Greedy_Soultions
  Greedy_accuracy[, (problem+1)] <- 1- sol$Greedy_Percents
  
  
  sol <- read_rds(str_c("E:/Project/Fast Greedy Solutions/Fast_Greedy", problem, ".rds", sep=""))
  
  Fast_times[, (problem+1)] <- sol$FastGreedy_Times
  Fast_solutions[, (problem+1)] <- sol$FastGreedy_Soultions
  Fast_accuracy[, (problem+1)] <- 1- sol$FastGreedy_Percents
  
  
  # sol <- read_rds(str_c("E:/Project/Stingy Solutions/Stingy", problem, ".rds", sep="", problem, ".rds", sep=""))
  # 
  # Stingy_times[, (problem+1)] <- sol$Stingy_Times
  # Stingy_solutions[, (problem+1)] <- sol$Stingy_Soultions
  # Stingy_accuracy[, (problem+1)] <- 1- sol$Stingy_Percents
  
  Problem_summary$PROB <- sol$problem
  Problem_summary$p <- sol$p
  Problem_summary$n <- sol$vertices
  Problem_summary$opt <- sol$opt
  
  
}

names(Greedy_times)[2:41] <- str_c("PROB", 1:40)
names(Greedy_solutions)[2:41] <- str_c("PROB", 1:40)
names(Greedy_accuracy)[2:41] <- str_c("PROB", 1:40)

names(Fast_times)[2:41] <- str_c("PROB", 1:40)
names(Fast_solutions)[2:41] <- str_c("PROB", 1:40)
names(Fast_accuracy)[2:41] <- str_c("PROB", 1:40)

# names(Stingy_times)[2:41] <- str_c("PROB", 1:40)
# names(Stingyt_solutions)[2:41] <- str_c("PROB", 1:40)
# names(Stingy_accuracy)[2:41] <- str_c("PROB", 1:40)

CH_Results <- list(Problem_summary,
                   Greedy_times,
                   Greedy_solutions,
                   Greedy_accuracy,
                   Fast_times,
                   Fast_solutions,
                   Fast_accuracy)


name=str_c("E:/Project/CH_RESULTS.rds", sep="")
write_rds(sol, path = name)