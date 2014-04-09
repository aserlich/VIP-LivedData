library(stringr)
library(plyr)
library(xts) # also pulls in zoo
library(timeDate)
library(chron)
##Detect problems with ward lookup
#############################
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")

currentFile <-  tail(exports,1)[1]
setwd(paste0(workd,currentFile))

cat("Now loading the exports file from most recent file", currentFile, "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="")

numObs <- nrow(exportFile)
numAdd <- nrow(exportFile[!is.na(exportFile$extras.raw_user_address), ])
      
#String match
#exportFile$extras.raw_user_address

#basic regular expression
basicAddress <- "^[0-9]{1,5}[[:space:]]?[[:alpha:]]+"

#test regular expresssion
#grep("536 jules street malven jhb", pattern = basicAddress, value=TRUE)

#add column to matrix
exportFile$wellFormed <- unlist(sapply(exportFile$extras.raw_user_address, simplify=TRUE,
      function(x) grepl(x, pattern=basicAddress)
      ))

#Create subset
addressSubset <- exportFile[( !is.na(exportFile$extras.raw_user_address)| !is.na(exportFile$extras.ward)),
           c("extras.raw_user_address","extras.raw_user_address_2", "extras.ward", "wellFormed")]

#Digit fields
numGoodWards <- length(na.omit(addressSubset$extras.ward))
numGoodAddresses <-sum(addressSubset$wellFormed[!is.na(addressSubset$extras.ward)], na.rm =TRUE)
wfform <- nrow(addressSubset[addressSubset$wellFormed==1 & !is.na(addressSubset$extras.ward), ])
mismatch2 <- nrow(addressSubset[addressSubset$wellFormed==1 & is.na(addressSubset$extras.ward), ])

cat(sprintf("The dataset currently has %d observations, of which %d have filled out the address field, which is %f", numObs, numAdd, numAdd/numObs), "\n\n")
cat(sprintf("Of the addresses, there are currently %d addresses where the ward looked up went through
of those, %d addresses that meet basic formatting requirements of **number** then **text**, which constitutes %f of all ward matches.

There are currently %d cases where the address is not well formed but there is still a ward match
and %d where the address is well formed but there is not a match.",
            numGoodWards, numGoodAddresses, numGoodAddresses/numGoodWards, wfform, mismatch2))

#output a csv of the address concerns
write.csv(addressSubset, file=paste0("AddressSubset","_", Sys.time(), ".csv"))



###############################
##visualize count of incoming messages by hour
##################################

#x <- as.xts(1:3, timeDate(exportFile$created_at))


#strptime(exportFile$created_at[1], "Y%-%m-%d %H:%M")



setwd(workd)
