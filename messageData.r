library(stringr)
library(plyr)
library(xts) # also pulls in zoo
library(timeDate)
library(chron)
library(descr)
library(data.table)
rm(list=ls())
##Detect problems with ward lookup
#############################

workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

setwd(workd)
mexports <- list.files(path=workd, pattern ="message_2014_[0-9].*")

currentFile <-  tail(mexports,1)[1]

mesDat <- read.csv(file.path(paste0("./", currentFile, "/messages-export.csv") ) )

mps_dt = data.table(mesDat)
rm(mesDat)

mps_dt[,list(count=length(timestamp)),by=list(from_addr,content)]

mps_dt[,list(count=length(timestamp)),by=list(from_addr,content)]
