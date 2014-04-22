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
#########################
#Sample groups for incentivization
#April 22, 2014
#############################
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

setwd(workd)
exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")
currentFile <-  tail(exports,1)[1]
setwd(paste0(workd,currentFile))

cat(sprintf("Now loading the exports file from most recent file \\begin{verbatim}"), sprintf(currentFile), sprintf("\\end{verbatim}"), "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="")

#do we want twitter people and what about people that just answered the engagement question?
elecObserveIncentive <- exportFile[exportFile$extras.is_registered %in% c("true") & exportFile$extras.delivery_class %in% c("ussd", "twitter"),
                                   c("msisdn", "extras.is_registered", "extras.delivery_class")] 

#check
elecObserveIncentive[sample(nrow(elecObserveIncentive), 30), c("msisdn", "extras.is_registered", "extras.delivery_class")]

#divide the population into two groups of equal size (unless they are odd numbers. The the incentivzed group has one more person
set.seed(20140422)
noInc <- sample(nrow(elecObserveIncentive),floor(nrow(elecObserveIncentive)/2))

elecObserveIncentive$group <- NA
elecObserveIncentive$group[noInc] <- "noIncentive"
elecObserveIncentive$group[-noInc] <- "Incentive"

#this .csv shoudl be part of the data repository on github and should be located in the main repository like the wardnums experiment
#every additional push we do, which may add more users, will have to ignore the users already assigned to a groups
#write.csv
