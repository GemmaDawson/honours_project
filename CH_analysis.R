library(pacman)
p_load(tidyverse)
p_load(stringr)

Problem_summary <- data_frame(problem = c(1:40))

Greedy_times <- data_frame(iteration = c(1:50))
Greedy_solutions <- data_frame(iteration = c(1:50))
Greedy_percents <- data_frame(iteration = c(1:50))

Fast_times <- data_frame(iteration = c(1:50))
Fast_solutions <- data_frame(iteration = c(1:50))
Fast_percents <- data_frame(iteration = c(1:50))

for (problem in 1:40){
  
  sol <- read_rds(str_c("E:/Project/Greedy Solutions/Greedy", problem, ".rds", sep=""))
  
  Greedy_times[, (problem+1)] <- sol$Greedy_Times
  
  sol_fast <- read_rds(str_c("E:/Project/Fast Greedy Solutions/Fast_Greedy", problem, ".rds", sep=""))
  
  
  Greedy_times[, (problem+1)] <- sol2$FastGreedy_Times
  
  
}

read_rds(str_c(pmedfolder, problem, "_SOL.rds"))

name=str_c("E:/Project/TB Interchange Solutions/TB_rand", problem, ".rds", sep="")
write_rds(sol, path = name)