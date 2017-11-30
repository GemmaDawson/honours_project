library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(openxlsx)


# sol.folder <- "D:/Project/27Nov_Solutions/"
sol.folder <- "E:/Project/"
sol.files <- list.files(sol.folder)

############################
CH.files <- sol.files[grep(pattern = "CH", x=sol.files, fixed = F)]

CH.df <- setNames(data.frame(matrix(ncol = 14, nrow = 20)), 
         c("PROBLEM", "NETWORK.SIZE", "MEDIANS", "EDGES", "OPTIMAL",
           "TIME.MEAN", "TIME.MIN", "TIME.MAX", "TIME.SD", "HIT.RATE",
           "GAP.MEAN", "GAP.BEST", "GAP.WORST", "GAP.SD"))

CH.df[,1] <- as.character(CH.df[,1])
CH.df[,2:13] <- 0

for (i in 1:2){
# for (i in seq_along(CH.files)){
  files.in.folder <- list.files((str_c(sol.folder, CH.files[i])))
  
  for (j in seq_along(files.in.folder)){
    prob.sol <- read_rds(str_c(sol.folder, CH.files[i], "/", files.in.folder[j]))
    
    prob.sol[[6]][prob.sol[[6]] == 0] <- NA
    prob.sol[[7]][prob.sol[[7]] == 0] <- NA
    prob.sol[[8]][prob.sol[[8]] == 0] <- NA
    
    # TIME - BASIC STATS
    time.mean <- round(mean(prob.sol[[8]], na.rm=T), digits = 1)
    time.min <- round(min(prob.sol[[8]], na.rm=T), digits = 1)
    time.max <- round(max(prob.sol[[8]], na.rm=T), digits = 1)
    time.sd <- round(sd(prob.sol[[8]], na.rm=T), digits = 2)
    
    # OPTIMAL HIT RATE
    # How many solutions out of the 50 are optimal
    hit.rate <- round(sum(prob.sol[[6]]==prob.sol$opt, na.rm = T)/50*100, digits = 1)
    
    # GAP TO OPTIMAL - BASIC STATS
    gap.mean <- round(mean(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
    gap.best <- round(min(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
    gap.worst <- round(max(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
    gap.sd <- round(sd(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 2)
    
    CH.df[j,] <- c(prob.sol[[1]], prob.sol[[2]],
                   prob.sol[[3]], prob.sol[[4]], prob.sol[[5]],
                   time.mean, time.min, time.max, time.sd,
                   hit.rate,         
                   gap.mean, gap.best, gap.worst, gap.sd)
    assign(CH.files[i], CH.df)
  }

  
}

#############################
LS.files <- sol.files[grep(pattern = "LS", x=sol.files, fixed = F)]

LS.df <- setNames(data.frame(matrix(ncol = 17, nrow = 20)), 
                  c("PROBLEM", "NETWORK.SIZE", "MEDIANS", "EDGES", "OPTIMAL",
                    "TIME.MEAN", "TIME.MIN", "TIME.MAX", "TIME.SD",
                    "GAP.MEAN", "GAP.BEST", "GAP.WORST", "GAP.SD",
                    "IT.MEAN", "IT.MIN", "IT.MAX", "IT.SD"))

LS.df[,1] <- as.character(LS.df[,1])
LS.df[,2:17] <- 0

start.sol <- c("greedy", "rand")

for (i in seq_along(LS.files)){
  all.files <- list.files((str_c(sol.folder, LS.files[i])))
  
  
  
  for (k in 1:2){
    files.in.folder <- all.files[grep(pattern = start.sol[k], x=all.files, fixed = F)]
    
    for (j in seq_along(files.in.folder)){
      prob.sol <- read_rds(str_c(sol.folder, LS.files[i], "/", files.in.folder[j]))
      
      # TIME - BASIC STATS
      time.mean <- round(mean(prob.sol[[8]], na.rm=T), digits = 1)
      time.min <- round(min(prob.sol[[8]], na.rm=T), digits = 1)
      time.max <- round(max(prob.sol[[8]], na.rm=T), digits = 1)
      time.sd <- round(sd(prob.sol[[8]], na.rm=T), digits = 2)
      
      # GAP TO OPTIMAL - BASIC STATS
      gap.mean <- round(mean(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
      gap.best <- round(min(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
      gap.worst <- round(max(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
      gap.sd <- round(sd(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 2)
      
      # ITERATIONS - BASIC STATS
      it.mean <- round(mean(prob.sol[[9]], na.rm=T), digits = 1)
      it.min <- round(min(prob.sol[[9]], na.rm=T), digits = 1)
      it.max <- round(max(prob.sol[[9]], na.rm=T), digits = 1)
      it.sd <- round(sd(prob.sol[[9]], na.rm=T), digits = 2)
      
      LS.df[j,] <- c(prob.sol[[1]], prob.sol[[2]],
                     prob.sol[[3]], prob.sol[[4]], prob.sol[[5]],
                     time.mean, time.min, time.max, time.sd,
                     gap.mean, gap.best, gap.worst, gap.sd,
                     it.mean, it.min, it.max, it.sd)
      assign(str_c(LS.files[i], start.sol[k], sep=" "), LS.df)
    }
  }
  
  
  
  
}


############################


# CH.files
# [1] "CH Fast Greedy Solutions" "CH Greedy Solutions"      "CH Stingy Solutions"

Fast.sol.summary <- read_rds(str_c(sol.folder, CH.files[1], "sol.summary", sep="\\"))
Fast.sol.accuracy <- read_rds(str_c(sol.folder, CH.files[1], "sol.accuracy", sep="\\"))

Greedy.sol.summary <- read_rds(str_c(sol.folder, CH.files[2], "sol.summary", sep="\\"))
Greedy.sol.accuracy <- read_rds(str_c(sol.folder, CH.files[2], "sol.accuracy", sep="\\"))

# Stingy.sol.summary <- read_rds(str_c(sol.folder, CH.files[3], "sol.summary", sep="\\"))
# Stingy.sol.accuracy <- read_rds(str_c(sol.folder, CH.files[3], "sol.accuracy", sep="\\"))



# abc <- Fast.sol.summary
# algo <- "Fast Greedy Algorithm"
# # 
abc <- Greedy.sol.summary
algo <- "Greedy Algorithm"


# png(filename = str_c(sol.folder, "\\", algo, ".Med.v.Time.png"))
ggplot() +
  geom_path(data=abc, size=2,aes(x=p, y=av.time, colour=as.factor(vertices))) +
  geom_point(data=abc, size=5,aes(x=p, y=av.time, colour=as.factor(vertices))) +
  labs(title=str_c(algo, " \nNumber of Medians vs Average Computational Time", sep=""), 
       y="Average Time (seconds)", 
       x="Number of Medians (p)",
       colour="Network Size\n(nodes)") +
  scale_colour_hue(l=50)
# dev.off()

ch.sol.summary <- data_frame(problem = Greedy.sol.summary$problem)
ch.sol.summary[41:80,1] <- Fast.sol.summary$problem
ch.sol.summary[1:40,2] <- data_frame(p = Greedy.sol.summary$p)
ch.sol.summary$p[41:80] <- Fast.sol.summary$p
ch.sol.summary[1:40,3] <- data_frame(nodes = Greedy.sol.summary$vertices)
ch.sol.summary$nodes[41:80] <- Fast.sol.summary$vertices
ch.sol.summary[1:40,4] <- data_frame(algo =  "Greedy")
ch.sol.summary$algo[41:80] <- "Fast Greedy"
ch.sol.summary[1:40,5] <- data_frame(av.acc = Greedy.sol.summary$av.sol.accuracy)
ch.sol.summary$av.acc[41:80] <- Fast.sol.summary$av.sol.accuracy
ch.sol.summary[1:40,6] <- data_frame(av.time = Greedy.sol.summary$av.time)
ch.sol.summary$av.time[41:80] <- Fast.sol.summary$av.time
ch.sol.summary$prob <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
                         "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                         "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                         "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                         "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
                         "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                         "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                         "31", "32", "33", "34", "35", "36", "37", "38", "39", "40")
ch.sol.summary[1:40,8] <- data_frame(algo2 =  1)
ch.sol.summary$algo2[41:80] <- 2

ch.sol.summary$abc <- as.numeric(ch.sol.summary$prob)

ggplot() +
  geom_path(data=ch.sol.summary, size=1, aes(x=abc, y=av.time, colour=as.factor(algo))) +
  geom_point(data=ch.sol.summary, size=5, aes(x=abc, y=av.time, colour=as.factor(algo))) +
  labs(title="Constructive Heuristics\nAverage Computational Time per Test Problem", 
       y="Average Time (seconds)", 
       x="Test Problem",
       colour="") +
  scale_colour_hue(l=50) +
  theme(legend.position="top") +
  scale_x_discrete(limits=ch.sol.summary$abc,
                   breaks=ch.sol.summary$abc,
                   labels=ch.sol.summary$prob)

ggplot() +
  geom_point(data=ch.sol.summary, size=5, aes(x=abc, y=av.acc, colour=as.factor(algo))) +
  geom_path(data=ch.sol.summary, size=1, aes(x=abc, y=av.acc, colour=as.factor(algo))) +
  labs(title="Constructive Heuristics\nAverage Solution Accuracy per Test Problem", 
       y="Average Solution Accuracy", 
       x="Test Problem",
       colour="") +
  scale_colour_hue(l=50) +
  theme(legend.position="top") +
  scale_x_discrete(limits=ch.sol.summary$abc,
                   breaks=ch.sol.summary$abc,
                   labels=ch.sol.summary$prob)






ggplot() +
  geom_path(data=abc, size=1, aes(x=p, y=av.time, colour=as.factor(vertices))) +
  geom_point(data=abc, size=5, aes(x=p, y=av.time, colour=as.factor(vertices))) +
  labs(title=str_c(algo, " Algorithm\nNumber of Medians vs Average Computational Time", sep=""), 
       y="Average Time (seconds)", 
       x="Number of Medians (p)",
       colour="Network Size\n(nodes)") +
  scale_x_discrete()
  scale_colour_hue(l=50)



ggplot() +
  geom_path(data=abc, size=2,aes(x=p, y=av.sol.accuracy, colour=as.factor(vertices))) +
  geom_point(data=abc, size=5,aes(x=p, y=av.sol.accuracy, colour=as.factor(vertices)))



ggplot() +
  geom_point(data=abc, size=5,aes(x=problem, y=av.sol.accuracy, colour=as.factor(vertices))) +


ggplot() +
  geom_point(data=abc, size=5,aes(x=problem, y=av.time, colour=as.factor(vertices))) +
  scale_x_discrete(limits=abc$problem,
                   breaks=abc$problem,
                   labels=c(1:40))

c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40)
  
###############################################
pqr <- Fast.sol.accuracy
pqr <- Greedy.sol.accuracy
pqr <- Stingy.sol.accuracy

ggplot() +
  geom


