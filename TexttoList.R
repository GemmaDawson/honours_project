library(tidyverse)
library(stringr)
library(tictoc)
library(igraph)

problemfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/Beasleys ORLibrary 40 pmedian problems"
setwd("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems")

optfile = read.table("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/Beasleys ORLibrary 40 pmedian problems/pmedopt.txt", header = T)
# pmedinfo = data.frame(NULL)

tic()
for (i in 1:40){
  #Create filename
  filename = str_c("C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/Beasleys ORLibrary 40 pmedian problems/pmed",i,".txt", sep="")
  
  #Read file
  prob = read.table(filename, header = F, sep = " ")
  
  #Check file length against vertices length
  stopifnot(nrow(prob)== prob[1,3]+1)
  
  #Yay!  Found I need to clean this data
  #It appears that edges are repeated 
  #& instructions from Beasley's website require using the second edge presented
  dets <- prob[1,2:4]
  prob <- prob[-1,]
  prob$V5 <- 1:nrow(prob)
  prob <- data_frame(n1=pmin(prob$V2, prob$V3), n2=pmax(prob$V2, prob$V3), c=prob$V4, r=prob$V5)
  prob <- arrange(prob, -r)
  prob <- prob[-which(duplicated(prob[,1:2])),]
  dets[[2]] <- nrow(prob)
  
  #Create edgelist
  el = as.matrix(prob[1:nrow(prob),1:2])
  
  #Create undirected cost with edge costs
  g = graph_from_edgelist(el, directed = F) %>%  
    set_edge_attr("cost", value = prob$c)
  
  #Create distane matrix from graph
  d = distances(g, weights = E(g)$cost)
  
  #Create a list for the test problem
  x = list(problem = as.character(optfile[i,1]), 
           p = dets[[3]], 
           vertices = dets[[1]], 
           edges = dets[[2]],
           opt = optfile[i,2],
           graph = g,
           distancematrix = d)
  
  name=str_c(as.character(optfile[i,1]), ".rds", sep="")
  write_rds(x, path = name)
  rm(prob, g, el, d, x, name)
}
toc()
# 10.41 sec elapsed

# 16.07 sec elapsed



# ##################################3  
# This code took # 47.64 sec 
#   
#   #Apply Floydd's algorithm to generate cost-matrix in dataframe format
#   # Let n be the number of vertices
#   vertices = prob[1,1]
#   # Set c(i,j)=infinity for i=1,...,n j=1,...,n.
#   probmat = matrix(data = NA, nrow = vertices, ncol = vertices)
#   # c(i,i)=0 for i=1,...,n
#         for (j in seq_along(1:vertices)){
#           probmat[j,j] = 0
#         }
#   
#   # Read each edge line in the data file IN TURN:
#   # if the three numbers in the line are i,j,k then
#   # set c(i,j)=k and c(j,i)=k
#         for(j in seq.int(2,nrow(prob),1)){
#           probmat[prob[j,1],prob[j,2]] = prob[j,3]
#           probmat[prob[j,2],prob[j,1]] = prob[j,3]
#         }
#   
#   # assign(str_c("pmed", i), probmat)


