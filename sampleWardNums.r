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
##Detect problems with ward lookup
######################################
#File to recontact those registered with no look up info
########################################
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

setwd(workd)

exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")

currentFile <-  tail(exports,1)[1]
setwd(paste0(workd,currentFile))

cat("Now loading the exports file from most recent file", currentFile, "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="")

recontact <- exportFile[exportFile$extras.is_registered %in% c("true") & exportFile$extras.delivery_class %in% c("ussd")  & is.na(exportFile$extras.raw_user_address), ]

#check
recontact[sample(nrow(recontact), 30), c("extras.raw_user_address", "extras.is_registered", "extras.delivery_class")]

numberstoSample <- recontact$msisdn

groups <- 3
elements <- ifelse( nrow(recontact) < 10000, floor(nrow(recontact)/3), 3333)


set.seed(20140413)
recontactSample <- as.data.frame(matrix(sample(numberstoSample,size=(groups*elements)),nrow=elements*groups, ncol=1))
names(recontactSample) <-  c("numbers")
recontactSample$group <- NA
recontactSample[1:elements, "group" ] <- "group1"
recontactSample[(elements+1):(elements+3334), "group"] <- "group2"
recontactSample[(elements*2+1):nrow(recontactSample),"group" ] <- "group3"

#> nrow(recontactSample[recontactSample$group%in% c("group1"),])


write.csv(recontactSample, file=paste0("recontactSample", Sys.Date(), ".csv"))

setwd("..")
