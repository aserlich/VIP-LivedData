library(stringr)
library(plyr)
##Detect problems with ward lookup
workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

##visualize count of incomign messages by hour

exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")

currentFile <-  tail(exports,1)[1]
setwd(paste0(workd,currentFile))

cat("Now loading the exports file from most recent file", currentFile)

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE)

numObs <- nrow(exportFile)
numAdd <- nrow(exportFile[!is.na(exportFile$extras.ward), ])
      
cat(sprintf("The dataset currently %d observations, of which %d have filled out the address field\n", numObs, numAdd))

#String match
exportFile$extras.raw_user_address

basicAddress <- "[0-9]{1,5}[:space:]?[a-z]"

grep("536 jules street malven jhb", pattern = basicAddress, value=TRUE)
      
exportFile$wellFormed <- unlist(sapply(exportFile$extras.raw_user_address, simplify=TRUE,
      function(x) grepl(x, pattern=basicAddress)
      ))

exportFile[exportFile$wellFormed==1,
           c("extras.raw_user_address", "extras.ward")]
grep("66 Jozi" , pattern =")
