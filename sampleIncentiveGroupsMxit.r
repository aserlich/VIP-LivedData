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
#Sample groups for incentivization for Mxit and everyone else not included in the first incentvization
#April 23, 2014
#############################
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData"

setwd(workd)
exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")
currentFile <-  tail(exports,1)[1]
setwd(file.path(workd,currentFile))

cat(sprintf("Now loading the exports file from most recent file \\begin{verbatim}"), sprintf(currentFile), sprintf("\\end{verbatim}"), "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="", colClasses="character")

setwd(workd)

alreadyContacted <- list.files(path=workd, pattern ="monitorPush.*2014.*csv$")

for(f in 1:length(alreadyContacted)) {
    if(f==1){
       ac <- read.csv(alreadyContacted[[f]], header=TRUE, stringsAsFactors=FALSE, colClasses="character")[ , "msisdn"]
   } else {
       ac2 <- read.csv(alreadyContacted[[f]], header=TRUE, stringsAsFactors=FALSE, colClasses="character")[, "msisdn"]
       ac <- c(ac,ac2)
   }
}

#ac$msisdn 
#append all csvs so that we don't resample them


#Mxit only--keep people on mxit

awQuest <- c("answerwin_question_gender", "answerwin_question_age", "answerwin_question_2009election", "answerwin_question_race")


#answerin_complete is not valid for mxit
#this omits veryone that was in a former push
elecObserveIncentive <- exportFile[exportFile$is_registered %in% c("true") &
                                   exportFile$delivery_class %in% c("gtalk", "twitter", "ussd", "mxit") &
                                   !(exportFile$msisdn %in% c("unknown")),
                                   c(c("msisdn", "is_registered", "delivery_class", "USSD_number", "key", "created_at"), awQuest)]

elecObserveIncentive$awComplete <- apply(elecObserveIncentive[, awQuest], 1, function(x) sum(!is.na(x)))

#add in mobi users
mobiPath <- file.path(workd, "analyzeYAL.r")
source(mobiPath)
mobiUser[, c("created_at", "awComplete", awQuest)] <- NA
elecObserveIncentive <- rbind(elecObserveIncentive, mobiUser)

#remove those that have already been contacted including mobi users


cat("There are", nrow(mobiUser), "mobi users")

#head(elecObserveIncentive[ , c("msisdn", "awComplete", "is_registered")]


#rm(exportFile)

#check
#elecObserveIncentive[sample(nrow(elecObserveIncentive), 30), c("msisdn", "is_registered", "delivery_class", "USSD_number")]
elecObserveIncentive$msisdn <- gsub(" ", "", elecObserveIncentive$msisdn)
#nrow(elecObserveIncentive) == length(unique(elecObserveIncentive$msisdn))

#remove those without valid length phone numbers
elecObserveIncentive$validNum <- 0
elecObserveIncentive$validNum[str_length(elecObserveIncentive$msisdn)==12 & !(elecObserveIncentive$msisdn) %in% c("unknown")] <- 1

#those who have completed answer and win, who don't have a valid number, these people get 2
elecObserveIncentive$validNumComp <- elecObserveIncentive$validNum
elecObserveIncentive$validNumComp[elecObserveIncentive$validNumComp==0 & elecObserveIncentive$awComplete==4] <- 2

elecObserveIncentive <- elecObserveIncentive[elecObserveIncentive$validNum ==1, ] 

#we have about fifteen percent of people who are entering invalid numbers when the finish the A&W section

#head(elecObserveIncentive[ , c("msisdn", "awComplete", "is_registered", "validNum")]


elecObserveIncentive <- elecObserveIncentive[, -which(names(elecObserveIncentive) %in% awQuest)]

#NOTE THERE WAS A BUG in this FILE on the earilire pushes for non USSD channels
##27th push and we could have accidently recontacted several people in different incentive groups or the same
#This should apply to a small number of people.
##   mobi    mxit twitter    ussd 
##     36    1053       1     166 

##   mobi    mxit twitter    ussd 
##    36    1142       4     166 
elecObserveIncentive <- elecObserveIncentive[!(elecObserveIncentive$msisdn %in% ac), ]


cat("There are", nrow(elecObserveIncentive[elecObserveIncentive$validNumComp==1,]), "total users with no errors in msisdn")
cat("There are", nrow(elecObserveIncentive), "total new users")
print(table(elecObserveIncentive$delivery_class))

#these people have both USSD numbers and are being delivered by mxit
#elecObserveIncentive[!is.na(elecObserveIncentive$USSD_number), ]

#divide the population into two groups of equal size (unless they are odd numbers. The the incentivzed group has one more person
set.seed(20140424)
obs <- nrow(elecObserveIncentive)
noIncNonDups <- sample(obs,floor(obs/2))

elecObserveIncentive$group <- NA
elecObserveIncentive$group[noIncNonDups] <- "noIncentive"
elecObserveIncentive$group[-noIncNonDups] <- "Incentive"

elecObserveIncentive <- elecObserveIncentive[with(elecObserveIncentive, order(group, delivery_class)), ]

cat("There are", nrow(elecObserveIncentive[elecObserveIncentive$validNumComp==1,]), "total users with no errors in msisdn")
cat("There are", nrow(elecObserveIncentive), "total new users")
print(table(elecObserveIncentive$delivery_class))


#this .csv shoudl be part of the data repository on github and should be located in the main repository like the wardnums experiment
#every additional push we do, which may add more users, will have to ignore the users already assigned to a groups
#only call this file once a day
#write.csv
write.csv(elecObserveIncentive, file = file.path(workd, paste0("monitorPushCombined2", Sys.Date(), ".csv")), row.names = FALSE)
