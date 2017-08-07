#Create an Excel file with each problem's edges, nodes, & p 

library(tidyverse)
library(stringr)
library(openxlsx)


pmedfolder = "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/HONPR2C Coding/TestProblems/pmed"

allprob <- set_names(as_data_frame(matrix(ncol=5, nrow = 40)), c("name", "nodes", "edges", "p", "optimal"))

for (problem in 1:40){
  #load relevant list
  datafile <- str_c(pmedfolder, problem, ".rds")
  x <- read_rds(datafile)
  rm(datafile)
  
  allprob$name[problem] <- x$problem
  allprob$p[problem] <- x$p
  allprob$nodes[problem] <- x$vertices
  allprob$edges[problem] <- x$edges
  allprob$optimal[problem] <- x$opt
}

wb <- createWorkbook()
addWorksheet(wb, sheetName = "overview")
writeData(wb, sheet = "overview", x=allprob, rowNames = T, )
saveWorkbook(wb, 
             file= "C:/Users/Gemma/Documents/UNISA/Honours/Project 2017/testproblemoverview.xlsx", 
             overwrite=T)
