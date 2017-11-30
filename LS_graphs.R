library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(openxlsx)

sol.folder <- "E:/Project/"
sol.files <- list.files(sol.folder)

############################
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
      it.mean <- round(mean(prob.sol[[9]][1:10], na.rm=T), digits = 1)
      it.min <- round(min(prob.sol[[9]][1:10], na.rm=T), digits = 1)
      it.max <- round(max(prob.sol[[9]][1:10], na.rm=T), digits = 1)
      it.sd <- round(sd(prob.sol[[9]][1:10], na.rm=T), digits = 2)
      
      LS.df[j,] <- c(prob.sol[[1]], prob.sol[[2]],
                     prob.sol[[3]], prob.sol[[4]], prob.sol[[5]],
                     time.mean, time.min, time.max, time.sd,
                     gap.mean, gap.best, gap.worst, gap.sd,
                     it.mean, it.min, it.max, it.sd)
      assign(str_c(LS.files[i], start.sol[k], sep=" "), LS.df)
    }
  }
  
  
  
  
}


TBgreedy <- LS.df
TBrand <- LS.df

Fintgreedy <- LS.df
Fintrand <- LS.df

Altgreedy <- LS.df
Altrand <- LS.df



# plot time versus median percent of network
TBgreedy$percentmedians <- as.numeric(TBgreedy$NETWORK.SIZE)/as.numeric(TBgreedy$MEDIANS)*100
TBrandom$percentmedians <- as.numeric(TBrandom$NETWORK.SIZE)/as.numeric(TBrandom$MEDIANS)*100
Fintgreedy$percentmedians <- as.numeric(Fintgreedy$NETWORK.SIZE)/as.numeric(Fintgreedy$MEDIANS)*100
Fintrandom$percentmedians <- as.numeric(Fintrandom$NETWORK.SIZE)/as.numeric(Fintrandom$MEDIANS)*100
Altgreedy$percentmedians <- as.numeric(Altgreedy$NETWORK.SIZE)/as.numeric(Altgreedy$MEDIANS)*100
Altrandom$percentmedians <- as.numeric(Altrandom$NETWORK.SIZE)/as.numeric(Altrandom$MEDIANS)*100


# #########
xyz <- Fintrand

ggplot(xyz, aes(x=percentmedians, y=as.numeric(TIME.MEAN), colour = MEDIANS)) +
  geom_point(shape=16, size = 4) +
  geom_line(size = 1) +
  ylab("Computational Time (seconds)") +
  xlab("Percent of Network to Medians") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow=1,byrow=TRUE)) +
  expand_limits(y=c(0:15))


################
xyz <- Fintrand




TBgreedy2 <- data_frame(Algorithm = "Teitz and Bart",
                        Solution="Greedy",
                 Problem = c(1:40),
                 Time = TBgreedy$TIME.MEAN,
                 Gap = TBgreedy$GAP.MEAN)

TBrandom2 <- data_frame(Algorithm = "Teitz and Bart",
                        Solution="Random",
                        Problem = c(1:40),
                        Time = TBrandom$TIME.MEAN,
                        Gap = TBrandom$GAP.MEAN)

Fintgreedy2 <- data_frame(Algorithm = "Fast Interchange",
                          Solution="Greedy",
                 Problem = c(1:40), 
                 Time = Fintgreedy$TIME.MEAN,
                 Gap = Fintgreedy$GAP.MEAN)

Fintrandom2 <- data_frame(Algorithm = "Fast Interchange",
                          Solution="Random",
                          Problem = c(1:40), 
                          Time = Fintrandom$TIME.MEAN,
                          Gap = Fintrandom$GAP.MEAN)

Altgreedy2 <- data_frame(Algorithm = "Alternate",
                          Solution="Greedy",
                          Problem = c(1:40), 
                         Time = Altgreedy$TIME.MEAN,
                         Gap = Altgreedy$GAP.MEAN)

Altrandom2 <- data_frame(Algorithm = "Alternate",
                          Solution="Random",
                         Problem = c(1:40),
                         Time = Altrandom$TIME.MEAN,
                         Gap = Altrandom$GAP.MEAN)

# Altgreedy2 <- data_frame(Algorithm = "Alternate",
#                          Solution="Greedy",
#                  Problem = c(1:20), 
#                  Gap.to.Optimal = Altgreedy$GAP.MEAN,
#                  Time = Altgreedy$TIME.MEAN)


data.compare <- rbind(TBgreedy2, TBrandom2, Fintgreedy2, Fintrandom2, Altgreedy2, Altrandom2)

data.compare$Algorithm <- as.factor(data.compare$Algorithm)

Teitz and Bart
Fast Interchange
Alternate


ggplot(data.compare[data.compare$Solution == "Random",], 
       aes(x = as.numeric(Problem), y = as.numeric(Time), 
                             colour = Algorithm, 
                             group = Algorithm)) +
  geom_point(shape=16, size = 4) +
  geom_line(size = 1) +
  ylab("Computational Time (seconds)") +
  xlab("Test Problem") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", 
                               title.hjust = 0.5, 
                               nrow=1, byrow=TRUE))  +
  scale_x_continuous(breaks=seq(1:40))+
  scale_colour_hue(l=40, breaks=c("Teitz and Bart", "Fast Interchange", "Alternate"))



+
  scale_colour_hue(l=40, breaks=c("Teitz and Bart", "Fast Interchange", "Alternate")) 


+
  scale_x_continuous(breaks=seq(1:40))



ggplot(accuracy.compare, aes(x = Problem, y = as.numeric(Gap.to.Optimal), 
                             colour = Algorithm, 
                             group = Algorithm)) +
  geom_point(shape=16, size = 4) +
  geom_line(size = 1) +
  ylab("Gap to Optimal (%)") +
  xlab("Test Problem") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", 
                               title.hjust = 0.5, 
                               nrow=1, byrow=TRUE)) +
  scale_colour_hue(l=40, breaks=c("Teitz and Bart", "Fast Interchange", "Alternate")) +
  scale_x_continuous(breaks=seq(1:40))


Grtime <- data_frame(Algorithm = "Greedy",
                 Problem = c(1:40),
                 Gap.to.Optimal = `CH Greedy Solutions`$TIME.MEAN)

FGtime <- data_frame(Algorithm = "Fast Greedy",
                 Problem = c(1:40), 
                 Gap.to.Optimal = `CH Fast Greedy Solutions`$TIME.MEAN)

Sttime <- data_frame(Algorithm = "Stingy Greedy",
                 Problem = c(1:20), 
                 Gap.to.Optimal = `CH Stingy Solutions`$TIME.MEAN[1:20])
time.compare <- rbind(Grtime, FGtime, Sttime)

ggplot(time.compare, aes(x = Problem, y = as.numeric(Gap.to.Optimal), 
                             colour = Algorithm, 
                             group = Algorithm)) +
  geom_point(shape=16, size = 4) +
  geom_line(size = 1) +
  ylab("Computational Time (seconds)") +
  xlab("Test Problem") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", 
                               title.hjust = 0.5, 
                               nrow=1, byrow=TRUE)) +
  scale_colour_hue(l=40, breaks=c("Greedy", "Fast Greedy", "Stingy Greedy")) +
  scale_x_continuous(breaks=seq(1:40))

time.compare2 <- rbind(Grtime, FGtime)
cpall3 <- c("#AE3121", "#007700")

ggplot(time.compare2, aes(x = Problem, y = as.numeric(Gap.to.Optimal), 
                         colour = Algorithm, 
                         group = Algorithm)) +
  geom_point(shape=16, size = 4) +
  geom_line(size = 1) +
  ylab("Computational Time (seconds)") +
  xlab("Test Problem") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", 
                               title.hjust = 0.5, 
                               nrow=1, byrow=TRUE)) +
  scale_colour_manual(values=cpall3, breaks=c("Greedy", "Fast Greedy")) +
  scale_x_continuous(breaks=seq(1:40))
  

