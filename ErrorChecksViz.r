library(stringr)
library(plyr)
##Detect problems with ward lookup
#############################
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")

currentFile <-  tail(exports,1)[1]
setwd(paste0(workd,currentFile))

cat("Now loading the exports file from most recent file", currentFile)

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE)

numObs <- nrow(exportFile)
numAdd <- nrow(exportFile[!is.na(exportFile$extras.ward), ])
      
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
addressSubset <- exportFile[(exportFile$wellFormed==1 | !is.na(exportFile$extras.ward)),
           c("extras.raw_user_address", "extras.ward", "wellFormed")]

#Digit fields
numGoodWards <- length(na.omit(addressSubset$extras.ward))
numGoodAddresses <-nrow(addressSubset[addressSubset$wellFormed==1,])
mismatch1 <- nrow(addressSubset[addressSubset$wellFormed==0 & !is.na(addressSubset$extras.ward), ])
mismatch2 <- nrow(addressSubset[addressSubset$wellFormed==1 & is.na(addressSubset$extras.ward), ])

cat(sprintf("The dataset currently has %d observations, of which %d have filled out the address field\n", numObs, numAdd))
cat(sprintf("There are currently %d addresses where the ward looked up went through
and %d addresses that meet basic formatting requirements of **number** then **text**
 There are currently %d cases where the address is not well formed but there is still a ward match and %d where the address is well formed but there is not a match", numGoodWards, numGoodAddresses, mismatch1, mismatch2))

#output a csv of the address concerns
write.csv(addressSubset, file=paste0("AddressSubset","_", Sys.time(), ".csv"))



###############################
##visualize count of incoming messages by hour
##################################
setwd(workd)
