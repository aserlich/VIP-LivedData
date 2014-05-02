rm(list=ls())
library(stringr)
library(plyr)
library(xts) # also pulls in zoo
library(timeDate)
library(chron)
library(descr)
library(reshape2)
library(ggplot2)
library(xtable)
library(epicalc)
library(stringr)
source("/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/genericPKFunctions.r")
set.seed(20140501)
#########################
#Allocate all smart groups for Phase 3
#May 1, 2014
#############################
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData"

setwd(workd)
exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")
currentFile <-  tail(exports,1)[1]
setwd(file.path(workd,currentFile))

cat(sprintf("Now loading the exports file from most recent file \\begin{verbatim}"), sprintf(currentFile), sprintf("\\end{verbatim}"), "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="", colClasses="character")

setwd(workd)

exportFile[ , c("A", "C0", "D")] <- NA

exportFile[exportFile$is_registered %in% c("true"), c("A", "C0", "D") ] <-  1
exportFile[!(exportFile$is_registered %in% c("true")), c("A", "C0", "D") ] <- 0


exportFile$pushAllocation[exportFile$is_registered %in% c("true")] <- replicate(sum(exportFile$C0), sample(0:2, size=1))
exportFile$C1 <- NA
exportFile$C1[exportFile$pushAllocation == 1 & !is.na(exportFile$pushAllocation)] <- 1
exportFile$C1[exportFile$pushAllocation != 1 & !is.na(exportFile$pushAllocation)] <- 0

exportFile$C2 <- NA
exportFile$C2[exportFile$pushAllocation == 2 & !is.na(exportFile$pushAllocation)] <- 1
exportFile$C2[exportFile$pushAllocation != 2 & !is.na(exportFile$pushAllocation)] <- 0

head(exportFile[exportFile$is_registered %in% c("true"), c("C1" , "C2", "pushAllocation")], 100)

