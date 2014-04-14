library(stringr)
library(plyr)
library(xts) # also pulls in zoo
library(timeDate)
library(chron)
library(descr)
library(reshape2)
library(ggplot2)
library(xtable)
rm(list=ls())
##Detect problems with ward lookup
#############################



workd <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/"

setwd(workd)

dir.create(paste0("tex_", Sys.time()))

texFile <- tail(list.files(path=workd, pattern ="tex_2014.*"),1)[1]

sink(file = paste0(texFile, "/dashboardDump.txt"), append = FALSE, type = c("output", "message"),
     split = TRUE)

invisible(file.copy(from="VIPTexTemplate.tex", to=paste0(texFile,"/VIPTexTemplate", Sys.Date(),".tex")))

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

#create combined Variable for USSD channels and other channels
exportFile$combDeliverClass <- NA
exportFile$combDeliverClass[exportFile$extras.delivery_class %in% c("gtalk","twitter","mxit")]  <-  exportFile$extras.delivery_class[exportFile$extras.delivery_class %in% c("gtalk","twitter","mxit")]
exportFile$combDeliverClass[exportFile$extras.delivery_class %in% c("ussd")] <- as.character(exportFile$extras.USSD_number[exportFile$extras.delivery_class %in% c("ussd")])

#add column to matrix
exportFile$wellFormed <- unlist(sapply(exportFile$extras.raw_user_address, simplify=TRUE,
      function(x) grepl(x, pattern=basicAddress)
      ))

#Create subset
addressSubset <- exportFile[( !is.na(exportFile$extras.raw_user_address)| !is.na(exportFile$extras.ward)),
           c("extras.raw_user_address","extras.raw_user_address_2", "extras.ward", "wellFormed", "combDeliverClass", "created_at")]

#Digit fields
numGoodWards <- length(na.omit(addressSubset$extras.ward))
numGoodAddresses <-sum(addressSubset$wellFormed[!is.na(addressSubset$extras.ward)], na.rm =TRUE)
wfform <- nrow(addressSubset[addressSubset$wellFormed==1 & !is.na(addressSubset$extras.ward), ])
mismatch2 <- nrow(addressSubset[addressSubset$wellFormed==1 & is.na(addressSubset$extras.ward), ])

##Engagement question
numEng <- length(na.omit(exportFile$extras.engagement_question))
cat(sprintf("Of %d observations, %d or %f of the observations have answered the engagement question", numObs,numEng, numEng/numObs), "\n")
cat("Here is the breakdown of the answer to that question")
print(xtable(table(exportFile$extras.engagement_question)))
print(xtable(table(exportFile$extras.engagement_question)/numEng))
cat("\n\n")

##Registration
numReg = sum(exportFile$extras.is_registered %in% c("true"), na.rm=TRUE)
cat(sprintf("Of %d that answered the engagement question, %d or %f of the those **engaged** have answered the registeration question and agreed to the T&Cs.
So we are losing %f here", numEng,numReg, numReg/numEng, (numEng-numReg)/numEng), "\n")

cat("Of those that answer the registration question --- that is look at the T&Cs, who joins")

exportFile$extras.is_registered <- as.factor(exportFile$extras.is_registered)
exportFile$extras.USSD_number<- as.factor(exportFile$extras.USSD_number)



#exportFile[sample(nrow(exportFile),200), c("extras.delivery_class", "extras.USSD_number","combDeliverClass")]

#regbyChannel <- with(exportFile, crosstab(extras.is_registered, extras.USSD_number, chisq=TRUE))
regbyChannel <- with(exportFile[!is.na(exportFile$extras.delivery_class),], crosstab(extras.is_registered, extras.USSD_number, chisq=TRUE, prop.c=TRUE, prop.r=TRUE))

regbyChannel2 <- with(exportFile[!is.na(exportFile$combDeliverClass),], crosstab(extras.is_registered, combDeliverClass, chisq=TRUE, prop.c=TRUE, prop.r=TRUE))


regbyChannelxtab <- xtable(regbyChannel2, caption="Accept T&Cs by Channel") #usepackage{float} in tex

colnames(regbyChannelxtab) <- c("xtab", "c4279", "c1", "c2", "c3", "c8", "chash", "gtalk", "mxit", "twit", "TOTAL")

print(regbyChannelxtab,  table.placement="H", include.rownames = FALSE, size = "footnotesize")
#plot(regbyChannel)

regbyChannel <- with(exportFile[!is.na(exportFile$extras.delivery_class),], crosstab(extras.is_registered, extras.USSD_number, chisq=TRUE, prop.c=TRUE, prop.r=TRUE))


cat(sprintf("The dataset currently has %d observations, of which %d have filled out the address field, which is %f.", numObs, numAdd, numAdd/numObs), "\n\n")
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

#strptime(exportFile$created_at[1], format="%Y-%m-%d %R", tz="Africa/Johannesburg")

exportFile$posixTime <- as.POSIXct(format(strptime(exportFile$created_at, format="%Y-%m-%d %R", tz="GMT"),tz="Africa/Johannesburg", usetz=TRUE))

MyDatesTable <- table(cut(exportFile$posixTime, breaks="hour"))



databyChannelUSSD <- as.data.frame.matrix(table(cut(exportFile$posixTime, breaks="hour"), as.factor(exportFile$combDeliverClass)))

#need to expand code to automatically match channels
names(databyChannelUSSD) <-c("c4279", "channel1", "channel2", "channel3", "channel8", "channelonlyhash", "gtalk", "mxit", "twitter")

databyChannelUSSD$date <- as.POSIXct(rownames(databyChannelUSSD))

#update by channel as needed -- currently for USSD only
tcByChannel <- as.data.frame.matrix(table(cut(exportFile$posixTime, breaks="hour"), paste(as.factor(exportFile$combDeliverClass), exportFile$extras.is_registered)))
tcByChannel <- tcByChannel[,1:ncol(tcByChannel)-1]

channels <- as.vector(apply(as.matrix(c("c4279", "channel1", "channel2", "channel3","channel8","channelonlyhash", "gtalk", "mxit" , "twitter", "missing")),1,rep,3)) #trasch code
stateReg <- rep(c("false", "NA", "true"),3)
tcByChannel$date <- as.POSIXct(rownames(tcByChannel))


## newNames <- vector()
## for(i in 1:length(channels)){
##     newNames[i] <- paste0(channels[i], stateReg[i])
## }

## names(tcByChannel) <- newNames
## tcByChannel$date <- as.POSIXct(rownames(tcByChannel))

## tcbyChMelted <- melt(tcByChannel, id = "date")
## tcbyPlot <- ggplot(data = tcbyChMelted, aes(x = date, y = value, color = variable)) +
##   geom_line()



mcplt <- ggplot() + 
    geom_line(data = databyChannelUSSD, aes(x = date, y = channel1, color = "Channel1")) +
             geom_line(data = databyChannelUSSD, aes(x = date, y =c4279, color = "Channel2"))  +
             geom_line(data = databyChannelUSSD, aes(x = date, y =channel2, color = "Channel2"))  +
             geom_line(data = databyChannelUSSD, aes(x = date, y =channel3, color = "Channel3"))  +
             geom_line(data = databyChannelUSSD, aes(x = date, y =gtalk, color = "gtalk"))     +
             geom_line(data = databyChannelUSSD, aes(x = date, y =mxit, color = "mxit"))      +
             geom_line(data = databyChannelUSSD, aes(x = date, y =twitter, color = "twitter"))   +
             xlab('posixTime') +
             ylab('frequency') +
             labs(color="Channels")



pdf(file = paste0(workd, texFile,"/multichannelPlot.pdf"), width= 6, height = 4)
print(mcplt)
dev.off()

## pdf(file = paste0(workd, texFile,"/tcByCHannelPlot.pdf"), width= 6, height = 4)
## print(tcbyPlot)
##dev.off()
 sink(file = NULL)
setwd(workd)


