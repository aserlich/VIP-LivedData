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
#yes, we want twitter users. We can't contacts people who just answered the enagement questions. 
elecObserveIncentive <- exportFile[exportFile$is_registered %in% c("true") & exportFile$delivery_class %in% c("ussd", "twitter") & !(exportFile$msisdn %in% c("unknown")),
                                   c("msisdn", "is_registered", "delivery_class", "USSD_number", "key")]

rm(exportFile)

#check
elecObserveIncentive[sample(nrow(elecObserveIncentive), 30), c("msisdn", "is_registered", "delivery_class", "USSD_number")]
elecObserveIncentive$msisdn <- gsub(" ", "", elecObserveIncentive$msisdn)
nrow(elecObserveIncentive) == length(unique(elecObserveIncentive$msisdn))

source(file.path(workd, "analyzeYAL.r"))

cat("There are", nrow(mobiUser), "mobi users")
cat("There are", nrow(elecObserveIncentive), "VUMI users")

elecObserveIncentive <- rbind(elecObserveIncentive, mobiUser)

#remove those without valid length phone numbers
elecObserveIncentive <- elecObserveIncentive[str_length(elecObserveIncentive$msisdn)==12, ]

cat("There are", nrow(elecObserveIncentive), "total users with no errors in msisdn")


#deal with duplicates
elecObserveIncentive$duplicate <- 0
elecObserveIncentive$duplicate[duplicated(elecObserveIncentive$msisdn, fromLast=TRUE) | duplicated(elecObserveIncentive$msisdn, fromLast=FALSE)] <- 1

#elecObserveIncentive[duplicated(elecObserveIncentive$msisdn, fromLast=TRUE) | duplicated(elecObserveIncentive$msisdn, fromLast=FALSE), ]

#divide the population into two groups of equal size (unless they are odd numbers. The the incentivzed group has one more person
set.seed(20140422)
nondups <- nrow(elecObserveIncentive) - sum(elecObserveIncentive$duplicate)

dups <- sum(elecObserveIncentive$duplicate)

noIncNonDups <- sample(nondups,floor(nondups/2))
noIncDups  <- sample(dups,floor(dups/2))

elecObserveIncentive <- elecObserveIncentive[elecObserveIncentive$duplicate==0, ]

#I think this has been solved by the unique id and the bug found, but we could still have a duplicate from mobi
if(dups >0) {
elecObserveIncentiveDup <- elecObserveIncentive[elecObserveIncentive$duplicate==1, ]
#put half of theduplicates in one and have in the other. 
}

elecObserveIncentive$group <- NA
elecObserveIncentive$group[noIncNonDups] <- "noIncentive"
elecObserveIncentive$group[-noIncNonDups] <- "Incentive"

elecObserveIncentive <- elecObserveIncentive[with(elecObserveIncentive, order(group, delivery_class, USSD_number)), ]

#this .csv shoudl be part of the data repository on github and should be located in the main repository like the wardnums experiment
#every additional push we do, which may add more users, will have to ignore the users already assigned to a groups
#only call this file once a day
#write.csv
write.csv(elecObserveIncentive, file = file.path(workd, paste0("monitorPush", Sys.Date(), ".csv")), row.names = FALSE)
