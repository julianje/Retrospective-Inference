library(tidyverse)
setwd("~/Documents/Projects/DEEP/Retrospective-Inference/Data/")

file <- "Balls_3_Clean/Balls_3_Clean_DetectedObjects.csv"

signal <- 0.5
outputfile <- paste(substr(file,1,nchar(file)-4),"_",signal,".csv",sep="")
read_csv(file) %>%
  sample_frac(signal) %>% arrange(frame) %>%
  write.csv(outputfile, row.names=F, quote=F)
