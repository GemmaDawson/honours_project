library(pacman)
p_load(tidyverse)
p_load(stringr)

Problem_summary <- data_frame(problem = c(1:40))

# TB.Greedy_times <- data_frame(iteration = c(1:50))
# TB.Greedy_solutions <- data_frame(iteration = c(1:50))
# TB.Greedy_accuracy <- data_frame(iteration = c(1:50))
# TB.Random_times <- data_frame(iteration = c(1:50))
# TB.Random_solutions <- data_frame(iteration = c(1:50))
# TB.Random_accuracy <- data_frame(iteration = c(1:50))

FInt.Greedy_times <- data_frame(iteration = c(1:50))
FInt.Greedy_solutions <- data_frame(iteration = c(1:50))
FInt.Greedy_accuracy <- data_frame(iteration = c(1:50))
FInt.Random_times <- data_frame(iteration = c(1:50))
FInt.Random_solutions <- data_frame(iteration = c(1:50))
FInt.Random_accuracy <- data_frame(iteration = c(1:50))

Alt.Greedy_times <- data_frame(iteration = c(1:50))
Alt.Greedy_solutions <- data_frame(iteration = c(1:50))
Alt.Greedy_accuracy <- data_frame(iteration = c(1:50))
Alt.Random_times <- data_frame(iteration = c(1:50))
Alt.Random_solutions <- data_frame(iteration = c(1:50))
Alt.Random_accuracy <- data_frame(iteration = c(1:50))

for (problem in 1:40){
  
  # sol <- read_rds(str_c("E:/Project/TB Interchange Solutions/TB_greedy", problem, ".rds", sep=""))
  # 
  # TB.Greedy_times[, (problem+1)] <- sol$TB_Times
  # TB.Greedy_solutions[, (problem+1)] <- sol$TB_Soultions
  # TB.Greedy_accuracy[, (problem+1)] <- 1- sol$TB_Percents
  # 
  # sol <- read_rds(str_c("E:/Project/TB Interchange Solutions/TB_rand", problem, ".rds", sep=""))
  # 
  # TB.Random_times[, (problem+1)] <- sol$TB_Times
  # TB.Random_solutions[, (problem+1)] <- sol$TB_Soultions
  # TB.Random_accuracy[, (problem+1)] <- 1- sol$TB_Percents
  
  
  
  sol <- read_rds(str_c("E:/Project/Fast Interchange Solutions/FInt_greedy", problem, ".rds", sep=""))
  
  FInt.Greedy_times[, (problem+1)] <- sol$FInt_Times
  FInt.Greedy_solutions[, (problem+1)] <- sol$FInt_Solutions
  FInt.Greedy_accuracy[, (problem+1)] <- sol$FInt_Percents
  
  # sol <- read_rds(str_c("E:/Project/Fast Interchange Solutions/FInt_rand", problem, ".rds", sep=""))
  #   
  # FInt.Random_times[, (problem+1)] <- sol$FInt_Times
  # FInt.Random_solutions[, (problem+1)] <- sol$FInt_Solutions
  # FInt.Random_accuracy[, (problem+1)] <- sol$FInt_Percents
  
  
  sol <- read_rds(str_c("E:/Project/Alternate Solutions/ALT_greedy", problem, ".rds", sep=""))

  Alt.Greedy_times[, (problem+1)] <- sol$Alt_Times
  Alt.Greedy_solutions[, (problem+1)] <- sol$Alt_Solutions
  Alt.Greedy_accuracy[, (problem+1)] <- sol$Alt_Percents
  
  
  sol <- read_rds(str_c("E:/Project/Alternate Solutions/ALT_rand", problem, ".rds", sep=""))
  
  Alt.Random_times[, (problem+1)] <- sol$Alt_Times
  Alt.Random_solutions[, (problem+1)] <- sol$Alt_Solutions
  Alt.Random_accuracy[, (problem+1)] <- sol$Alt_Percents
  
  
  
  Problem_summary$PROB <- sol$problem
  Problem_summary$p <- sol$p
  Problem_summary$n <- sol$vertices
  Problem_summary$opt <- sol$opt
  
  
}

# names(TB.Greedy_times)[2:41] <- str_c("PROB", 1:40)
# names(TB.Greedy_solutions)[2:41] <- str_c("PROB", 1:40)
# names(TB.Greedy_accuracy)[2:41] <- str_c("PROB", 1:40)
# 
# names(TB.Random_times)[2:41] <- str_c("PROB", 1:40)
# names(TB.Random_solutions)[2:41] <- str_c("PROB", 1:40)
# names(TB.Random_accuracy)[2:41] <- str_c("PROB", 1:40)



names(FInt.Greedy_times)[2:41] <- str_c("PROB", 1:40)
names(FInt.Greedy_solutions)[2:41] <- str_c("PROB", 1:40)
names(FInt.Greedy_accuracy)[2:41] <- str_c("PROB", 1:40)

# names(FInt.Random_times)[2:41] <- str_c("PROB", 1:40)
# names(FInt.Random_solutions)[2:41] <- str_c("PROB", 1:40)
# names(Fint.Random_accuracy)[2:41] <- str_c("PROB", 1:40)



names(Alt.Greedy_times)[2:41] <- str_c("PROB", 1:40)
names(Alt.Greedy_solutions)[2:41] <- str_c("PROB", 1:40)
names(Alt.Greedy_accuracy)[2:41] <- str_c("PROB", 1:40)

names(Alt.Random_times)[2:41] <- str_c("PROB", 1:40)
names(Alt.Random_solutions)[2:41] <- str_c("PROB", 1:40)
names(Alt.Random_accuracy)[2:41] <- str_c("PROB", 1:40)


LS_Results <- list(Problem_summary,
                   FInt.Greedy_times,
                   FInt.Greedy_solutions,
                   FInt.Greedy_accuracy,
                   Alt.Greedy_times,
                   Alt.Greedy_solutions,
                   Alt.Greedy_accuracy,
                   Alt.Random_times,
                   Alt.Random_solutions,
                   Alt.Random_accuracy)


name=str_c("E:/Project/LS_RESULTS.rds", sep="")
write_rds(sol, path = name)