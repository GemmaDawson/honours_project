library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(openxlsx)

sol.folder <- "E:/Project/"
sol.files <- list.files(sol.folder)

############################
CH.files <- sol.files[grep(pattern = "CH", x=sol.files, fixed = F)]

CH.df <- setNames(data.frame(matrix(ncol = 13, nrow = 20)), 
                  c("PROBLEM", "MEDIANS", "Network Size", "EDGES", "OPTIMAL",
                    "TIME.MEAN", "TIME.MIN", "TIME.MAX", "TIME.SD",
                    "GAP.MEAN", "GAP.BEST", "GAP.WORST", "GAP.SD"))

CH.df[,1] <- as.character(CH.df[,1])
CH.df[,2:13] <- 0

for (i in 1:3){
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
    # hit.rate <- round(sum(prob.sol[[6]]==prob.sol$opt, na.rm = T)/50*100, digits = 1)
    
    # GAP TO OPTIMAL - BASIC STATS
    gap.mean <- round(mean(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
    gap.best <- round(min(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
    gap.worst <- round(max(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 1)
    gap.sd <- round(sd(((prob.sol[[6]] - prob.sol$opt)/prob.sol$opt)*100, na.rm=T), digits = 2)
    
    CH.df[j,] <- c(prob.sol[[1]], prob.sol[[2]],
                   prob.sol[[3]], prob.sol[[4]], prob.sol[[5]],
                   time.mean, time.min, time.max, time.sd,
                   gap.mean, gap.best, gap.worst, gap.sd)

    assign(CH.files[i], CH.df)
  }
}


# plot time versus median percent of network
CH.df$percentmedians <- as.numeric(CH.df$MEDIANS)/as.numeric(CH.df$`Network Size`)

ggplot(CH.df, aes(x=percentmedians*100, y=as.numeric(TIME.MEAN), colour = `Network Size`)) +
  geom_point(shape=16, size = 4) +
  geom_line(size = 1) +
  ylab("Computational Time (seconds)") +
  xlab("Percent of Network to Medians") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow=1,byrow=TRUE))

ggsave(filename = "C:/Users/Gemma.Dawson/Documents/GitHub/honours_project/percentmedvstime_greedy.png",
       plot = x)

unique(CH.df$PROBLEM[CH.df$PROBLEM != c("pmed21", "pmed22")])

CH.df <- CH.df[1:20,]

plot(x = CH.df$`Network Size`, y = CH.df$TIME.MEAN)
# plot accuracy versus network size

ggplot(CH.df, aes(x=percentmedians*100, y=as.numeric(CH.df$TIME.MEAN), colour = `Network Size`, group=`Network Size`)) +
  geom_point(shape=16, size = 4) +
  geom_line() +
  ylab("Computational Time (seconds)") +
  xlab("Percent of Network to Medians") + 
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow=1,byrow=TRUE))


# OVERVIEW OF CH


Gr <- data_frame(Algorithm = "Greedy",
                 Problem = c(1:40),
                 Gap.to.Optimal = `CH Greedy Solutions`$GAP.MEAN,
                 Time = `CH Greedy Solutions`$TIME.MEAN)

FG <- data_frame(Algorithm = "Fast Greedy",
                 Problem = c(1:40), 
                 Gap.to.Optimal = `CH Fast Greedy Solutions`$GAP.MEAN,
                 Time =`CH Fast Greedy Solutions`$Time.MEAN)

St <- data_frame(Algorithm = "Stingy Greedy",
                 Problem = c(1:20), 
                 Gap.to.Optimal = `CH Stingy Solutions`$GAP.MEAN[1:20],
                 Time = `CH Stingy Solutions`$Time.MEAN[1:20])


accuracy.compare <- rbind(Gr, FG, St)

accuracy.compare$Algorithm <- as.factor(accuracy.compare$Algorithm)

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
  scale_colour_hue(l=40, breaks=c("Greedy", "Fast Greedy", "Stingy Greedy")) +
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
  

