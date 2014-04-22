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

mesDat <- read.csv(file.path(paste0("./", currentFile, "/messages-export.csv") ), stringsAsFactors=FALSE )

##identify unique phone numbers
getUser <- function(df){
    phNum <- grepl("+27",as.character(df$to_addr), fixed=TRUE)
    phNumMatch <- df[phNum, "to_addr"]
    return(unique(phNumMatch))
     }

#get each individual user's record
getUserRecords <- function(user, df){
    return(df[df$to_addr==user | df$from_addr==user, ])
}

users <- getUser(mesDat)

#may need to use map/reduce or iterate so I don't crash R
userRecords <- lapply(users[1:500], getUserRecords, mesDat)

####
#find users who got the SMS to join the system
#include foreign languages
####
joinMsg <- c("Tnx 4 volunteering 2 be a citizen reporter for the '14 elections! Start by answering questions or report election activity! Dial back in to *120*7692*2# begin!")

#get people who have joined
subset1 <- function(x){
   nrow( x[x$content %in%joinMsg,]) >0
   }

joiners <- Filter(subset1,userRecords)

#order joiners messages by TimeStamp
joinersOrder <- llply(joiners, function(x) x[order(x$timestamp),])


subset2 <- function(x, stillNot = c("Still not my address", "Cha, asiyilo ikheli lami","Nog nie my address nie")
){
    return((sum(grepl(stillNot[1],x$content, fixed=TRUE)) +
    sum(grepl( stillNot[2], x$content, fixed=TRUE)) +
    sum(grepl(stillNot[3],x$content, fixed=TRUE))) >0)
   }

 
joinersStillNot <- Filter(subset2,joinersOrder)

extractStillNo <- function(x, stillNot = c("Still not my address", "Cha, asiyilo ikheli lami","Nog nie my address nie")
){
    c((which(grepl(stillNot[1],x$content, fixed=TRUE)),
    sum(grepl( stillNot[2], x$content, fixed=TRUE)) +
    sum(grepl(stillNot[3],x$content, fixed=TRUE))) >0)
   }




#remove everyone who has been allocated a ward
#or has tried to enter and address twice and has gotten "still not my address"

#find any messages that contain that string
rightCell <- which

#then find the number associated with that string


subset3 <- function(x, notMy = c("
                      
#Then see if anyone hit that number

#If they hit any other number, then they got a ward assignment

subset2 <- function(x){
   rownames(x[x$content %in%joinMsg,])
   }

#get last time they were asked
tail(which(joinersOrder[[1]]$content %in%joinMsg),1)           

## listifyData <- funct(df, phNum)
##                                     # listOfPeople <- lapply(
##     #
## }

## mps_dt = data.table(mesDat)
## rm(mesDat)

## mps_dt[,list(count=length(timestamp)),by=list(from_addr,content)]

## mps_dt[,list(count=length(timestamp)),by=list(from_addr,content)]
