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
library(car)
library(sjPlot)

source("/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/genericPKFunctions.r")
##Detect problems with ward lookup
#############################

##FUNCTIONS
rename_NA <- function(x){
  rownames(x)[is.na(rownames(x))] <- "NA"
  colnames(x)[is.na(colnames(x))] <- "NA"
  x
}

###
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

cat(sprintf("Now loading the exports file from most recent file \\begin{verbatim}"), sprintf(currentFile), sprintf("\\end{verbatim}"), "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="")

numObs <- nrow(exportFile)
numAdd <- nrow(exportFile[!is.na(exportFile$raw_user_address), ])
      
#String match
#exportFile$raw_user_address

#basic regular expression
basicAddress <- "^[0-9]{1,5}[[:space:]]?[[:alpha:]]+"

#test regular expresssion
#grep("536 jules street malven jhb", pattern = basicAddress, value=TRUE)

#create combined Variable for USSD channels and other channels
exportFile$combDeliverClass <- NA
exportFile$combDeliverClass[exportFile$delivery_class %in% c("gtalk","twitter","mxit")]  <-  exportFile$delivery_class[exportFile$delivery_class %in% c("gtalk","twitter","mxit")]
exportFile$combDeliverClass[exportFile$delivery_class %in% c("ussd")] <- as.character(exportFile$USSD_number[exportFile$delivery_class %in% c("ussd")])

#add column to matrix
exportFile$wellFormed <- unlist(sapply(exportFile$raw_user_address, simplify=TRUE,
      function(x) grepl(x, pattern=basicAddress)
      ))

#Create subset
addressSubset <- exportFile[( !is.na(exportFile$raw_user_address)| !is.na(exportFile$ward)),
           c("raw_user_address","raw_user_address_2", "ward", "wellFormed", "combDeliverClass", "created_at")]

#Digit fields
numGoodWards <- length(na.omit(addressSubset$ward))
numGoodAddresses <-sum(addressSubset$wellFormed[!is.na(addressSubset$ward)], na.rm =TRUE)
wfform <- nrow(addressSubset[addressSubset$wellFormed==1 & !is.na(addressSubset$ward), ])
mismatch2 <- nrow(addressSubset[addressSubset$wellFormed==1 & is.na(addressSubset$ward), ])

##Engagement question
numEng <- length(na.omit(exportFile$engagement_question))
cat(sprintf("Of %d observations, %d or %f of the observations have answered the engagement question", numObs,numEng, numEng/numObs), "\n")
cat("Here is the breakdown of the answer to that question")
print(xtable(table(exportFile$engagement_question)))
print(xtable(table(exportFile$engagement_question)/numEng))
cat("\n\n")

##Registration
##THERE IS  A BUG HERE BECAUSE WE HAVE A FEW PEOPLE WHO DON"T HAVE A DELIVERY CLASS
numReg = sum(exportFile$is_registered %in% c("true"), na.rm=TRUE)
cat(sprintf("Of %d that answered the engagement question, %d or %f of the those **engaged** have answered the registeration question and agreed to the T&Cs.
So we are losing %f here", numEng,numReg, numReg/numEng, (numEng-numReg)/numEng), "\n")

cat("Of those that answer the registration question --- let us look at the T&Cs, who joins")

exportFile$is_registered <- as.factor(exportFile$is_registered)
exportFile$USSD_number<- as.factor(exportFile$USSD_number)

#exportFile[sample(nrow(exportFile),200), c("delivery_class", "USSD_number","combDeliverClass")]

#regbyChannel <- with(exportFile, crosstab(is_registered, USSD_number, chisq=TRUE))
regbyChannel <- with(exportFile[!is.na(exportFile$delivery_class),], crosstab(is_registered, USSD_number, chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE))

regbyChannel2 <- with(exportFile[!is.na(exportFile$combDeliverClass),], crosstab(is_registered, combDeliverClass, chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE))

#*120*7692*2#	Standard Rates
#*120*4729#2	Free
#*120*7692*3#	Stand a chance to win

threechannels <- c("*120*7692*2#", "*120*4729#", "*120*7692*3#")

ussdex <- exportFile[exportFile$USSD_number %in% threechannels, ]


notthree <- -which(unique(ussdex$USSD_number) %in% threechannels)

ussdex$ussdChannel <- car::Recode(ussdex$USSD_number,
                                  " '*120*7692*2#' = 'Standard Rates' ;
                                    '*120*4729#' = 'Free' ;
                                    '*120*7692*3#' = 'Lottery'
                                     "
)

##CSOS

cat(sprintf("\\section{CSOs}"), "\nHere are the results of the CSOs", "\n")

#csos$msisdn <- paste0("+", csos$Mobile..)

engCsos <- exportFile[exportFile$msisdn %in% giveCSONums() , ]
engKP <- exportFile[exportFile$msisdn %in% giveKPNums() , ]

cat(sprintf("There are %d professional CSOS in the VIP dataset", nrow(engCsos)))
cat(sprintf("There are %d KP Observers in the VIP dataset", nrow(engKP)))
                 

#############
###Viz stuff
###########
library(scales)


awQuest <- c("answerwin_question_gender", "answerwin_question_age", "answerwin_question_2009election", "answerwin_question_race")


## ## ggplot(ussdex[!is.na(ussdex$answerwin_question_race), ], aes(as.factor(answerwin_question_race), fill=USSD_number)) + geom_bar(position = "dodge") +  scale_y_continuous(labels = percent)
## ##     xlab("Average Slope (Degrees)")
## ## +ylab("Plots (#)")
Pause <- function () { 
    cat("Hit <enter> to continue...")
    readline()
    invisible()
}


## ggplot(ussdex[!is.na(ussdex$answerwin_question_race), ], aes(as.factor(answerwin_question_race), fill=USSDChannel)) + facet_grid(~USSDChannel)


## for(i in 1:length(awQuest)) {
##     sjp.grpfrq(as.factor(ussdex[!is.na(ussdex[, awQuest[i]]),awQuest[i] ] ),
##                as.factor(ussdex[!is.na(ussdex[, awQuest[i]]), "ussdChannel" ] ),
##                startAxisAt=0, na.rm=TRUE,
##                useFacetGrid=TRUE)
##   Pause()


## }
## sjp.grpfrq(as.factor(ussdex$answerwin_question_race[!is.na(ussdex$answerwin_question_race)]), as.factor(ussdex$ussdChannel[!is.na(ussdex$answerwin_question_race)]), startAxisAt=0, na.rm=TRUE)


## sjp.grpfrq(as.factor(ussdex$answerwin_question_race[!is.na(ussdex$answerwin_question_race)]), as.factor(ussdex$ussdChannel[!is.na(ussdex$answerwin_question_race)]), startAxisAt=0, na.rm=TRUE,
## useFacetGrid=TRUE)


#df <- ddply(ussdex[!is.na(ussdex$answerwin_question_race), ], .(test1), transform, p = Freq/sum(Freq))
# and plot
#ggplot(df, aes(test2, p))+geom_bar()+facet_grid(~test1)



############
regbyChannelxtab <- xtable(regbyChannel2, caption="Accept T&Cs by Channel") #usepackage{float} in tex

#doesn't work -- coudl change the factor in delivery class
#colnames(regbyChannelxtab) <- c("xtab", "c4279", "c1", "c2", "c3", "c8", "chash", "gtalk", "mxit", "twit", "TOTAL") 

print(regbyChannelxtab,
      table.placement="H", include.rownames = FALSE, size = "footnotesize", rotate.colnames=TRUE)
#plot(regbyChannel)

regbyChannel <- with(exportFile[!is.na(exportFile$delivery_class),], crosstab(is_registered, USSD_number,
                                                                                     chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)
                     )

cat(sprintf("The dataset currently has %d observations, of which %d have filled out the address field, which is %f.", numObs, numAdd, numAdd/numObs), "\n\n")
cat(sprintf("Of the addresses, there are currently %d addresses where the ward looked up went through
of those, %d addresses that meet basic formatting requirements of **number** then **text**, which constitutes %f of all ward matches.

There are currently %d cases where the address is not well formed but there is still a ward match
and %d where the address is well formed but there is not a match.",
            numGoodWards, numGoodAddresses, numGoodAddresses/numGoodWards, wfform, mismatch2), "/n")

#########################
######RANDOMIZATION######
########################
cat(sprintf("\\section{Randomization Test}"), "\nHere are the results of the randomizaton \n
There are three groups A, B, C. A got the location lookup bypassed, B had to redo the location but it wasn't primed in the message, and C had to redo and the message was primed")

rgroupsPath <- "/Volumes/Optibay-1TB/RSA_RCT/Randomization_Test"
rgroupFile <- "AllGroups646.csv"

rgroupsdf <- read.csv(file=file.path(rgroupsPath, rgroupFile), header=TRUE, stringsAsFactors=FALSE)
rgroupsdf$MSISDN <- as.character(rgroupsdf$MSISDN)
rgroupsdf$msisdn <- paste0("+",rgroupsdf$MSISDN)

t1 <- merge(exportFile, rgroupsdf, by="msisdn", all.x=TRUE, all.y=TRUE)
#get rid of the NAs -- assume they are a bug
t1 <- t1[!is.na(t1$delivery_class),]
#just aribritarily remove one of the duplicate records for analytical purposes 
t2 <- t1[!duplicated(t1$msisdn, fromLast=FALSE), ]

t2$wardMatch <- !is.na(t2$ward)
t2$awComp <- !is.na(t2$answerwin_complete)

print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(answerwin_question_2009election, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Answered the Election Answer and Win question by Randomization group"
    ),
       table.placement="H")

print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(awComp, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Answered the entire answer and win section by Randomization group"
    ),
       table.placement="H")


print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(wellFormed, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Is the address well formed by Randomization group"
    ),
     table.placement="H"
      )

print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(wardMatch, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Ward lookup when through by Randomization group"
   ),
       table.placement="H"
      )

###Addrees Look Uo
##first do manual cleanup

## #if there is no second address use the first address
## addressSubset$cleanAddress <- NA
## addressSubset$cleanAddress[!is.na(addressSubset$raw_user_address_2)] <- tolower(addressSubset$raw_user_address_2[!is.na(addressSubset$raw_user_address_2)])
 
## addressSubset$cleanAddress[is.na(addressSubset$raw_user_address_2) & !is.na(addressSubset$raw_user_address)]  <-
##                                   tolower(addressSubset$raw_user_address[is.na(addressSubset$raw_user_address_2) & !is.na(addressSubset$raw_user_address)])


## cAdd <- addressSubset[!is.na(addressSubset$cleanAddress),]

## mainstr <- "[0-9]{1,4}[[:blank:]]?main.*pretoria>*"

## #need text before or after can recoup. Everyone with nothing gets wiped
## wardstr <- "ward[[:blank:]]?[[0-9]{1,2}.*"

## wardstr2 <- "(^.*)(ward[[:blank:]]?[0-9]{1,2})(.*$)"

## #cAdd[grepl(mainstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]
## #cAdd[grepl(wardstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]

## sprintf("The number of main streets or something similar in pretoria is %d", nrow(cAdd[grepl(mainstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]
##                                                                                  )
##         )

## sprintf("The number of people who enetered their ward # is %d", nrow(cAdd[grepl(wardstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]
##                                                                                  )
##         )

## wards <- str_match(cAdd$cleanAddress, wardstr2)

## cAdd$ward <- wards[,3]
## cAdd$otherWardInfo <- paste(wards[,2], wards[,4], sep =" ")

##wards[!is.na(wards[,1]), ]
##cAdd[!is.na(cAdd$ward), ]
##doesn't likt the word village or stand
##if there is a second address, use the second address
##configure adi's flags

##duplicate records
##duplicatedR <- t1[duplicated(t1$msisdn, fromLast=TRUE) | duplicated(t1$msisdn, fromLast=FALSE) , ]
##head(duplicatedR[,c("msisdn", "delivery_class", "USSD_number")], 100)
##write.csv(duplicatedR[,c("msisdn", "delivery_class", "USSD_number")],
##                      file.path(rgroupsPath, "duplicatesinVumi.csv"))
##dupRec <- length(unique(duplicatedR$msisdn))
###########

##output a csv of the address concerns
write.csv(addressSubset, file=paste0("AddressSubset","_", Sys.time(), ".csv"))

########
#answer and win questions
##########
registered <- exportFile[exportFile$is_registered %in% c("true"),  ]

cat(sprintf("\\section{Answer And Win}"), "\nHere is the data from the Answer and Win Section:")


registered$awComplete <- apply(registered[, awQuest], 1, function(x) sum(!is.na(x)))


for(qu in 1:length(awQuest)){
    regxtable <- tab1(registered[, awQuest[qu]], graph=FALSE)$output.table
    capt <- paste(gsub("_", " ",awQuest[qu]),sprintf(": %d People have given answer to this question", sum(regxtable[1:(nrow(regxtable)-2),1]) ) )
    print(xtable(rename_NA(regxtable),
                 caption = capt
                 ),
          table.placement="H"
          )
}

cat(sprintf("\\subsection{Answer And Win by Channel}"), "\nHere is the data from the Answer and Win Section by channel:")


for(qu in 1:length(awQuest)){
    ctabchannel <- with(registered, crosstab(get(awQuest[qu]), combDeliverClass,
                                           chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)
                        )
    
    print(xtable(ctabchannel,
                 caption = gsub("_", " ",awQuest[qu])
                 ),
                 table.placement="H", include.rownames = FALSE, size = "footnotesize", rotate.colnames=TRUE
          )
}


awFin <- length(registered$awComplete[registered$awComplete==4])
cat(sprintf("At total of %d people have finished the answer and win section, answering all the questions", awFin ))


########
#VIP questions
##########

cat(sprintf("\\section{VIP Section}"), "\nHere is the data from the VIP Section:\n")
cat(sprintf("%d people have completed all of the questions in the VIP section", length(na.omit(registered$vip_complete)) ), "\n") 


vipLabels <- c("Attended protest",
"Registered to vote",
"Likely to vote",
"Political party",
"Community protests",
"Violent protests",
"Neighbours",
"Neighbours judgement",
"Zuma performance",
"Local gov performance",
"Party Contact",
"Attended Campaign rally")


vipQuiz <- paste0("vip_question_", seq(1,12))

for(vp in 1:length(vipQuiz)){
    regxtable <- tab1(registered[, vipQuiz[vp]], graph=FALSE)$output.table
    capt <- paste(gsub("_", " ", vipQuiz[vp]), "--", vipLabels[vp],sprintf(": %d People have given answer to this question", sum(regxtable[1:(nrow(regxtable)-2),1]) ) )
    print(xtable(rename_NA(regxtable),
                 caption = capt
                 ),
                 table.placement="H"
          )
}


########
#Whats up?
##########
cat(sprintf("\\section{What's up}"), "\nHere is the data from the What's up section")

cat(sprintf("%d people have completed all of the questions in the What's Up section", length(na.omit(registered$whatsup_complete)) ) ) 

wutUp <- grep("^whatsup_question_", names(exportFile), value=TRUE)

for(wu in 1:length(wutUp)){
    regxtable <- tab1(registered[, wutUp[wu]], graph=FALSE)$output.table
    print(xtable(rename_NA(regxtable),caption = gsub("_", " ", wutUp[wu])),  table.placement="H")
}


###############################
##visualize count of incoming messages by hour
##################################

#x <- as.xts(1:3, timeDate(exportFile$created_at))

#strptime(exportFile$created_at[1], format="%Y-%m-%d %R", tz="Africa/Johannesburg")

exportFile$posixTime <- as.POSIXct(format(strptime(exportFile$created_at, format="%Y-%m-%d %R", tz="GMT"),tz="Africa/Johannesburg", usetz=TRUE))

MyDatesTable <- table(cut(exportFile$posixTime, breaks="hour"))

databyChannelUSSD <- as.data.frame.matrix(table(cut(exportFile$posixTime, breaks="hour"), as.factor(exportFile$combDeliverClass)))

#need to expand code to automatically match channels
names(databyChannelUSSD) <-c("vippoice2014", "c4279", "channel1", "channel2", "channel3", "channel8", "channelonlyhash", "c8864", "gtalk", "mxit", "twitter", "vipvoig_gmail")

databyChannelUSSD$date <- as.POSIXct(rownames(databyChannelUSSD))

#update by channel as needed -- currently for USSD only
tcByChannel <- as.data.frame.matrix(table(cut(exportFile$posixTime, breaks="hour"), paste(as.factor(exportFile$combDeliverClass), exportFile$is_registered)))
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


y(from="VIPTexTemplate.tex", to=paste0(texFile,"/VIPTexTemplate", Sys.Date(),".tex")))

exports <- list.files(path=workd, pattern ="contact_2014_[0-9].*")

currentFile <-  tail(exports,1)[1]
setwd(paste0(workd,currentFile))

cat(sprintf("Now loading the exports file from most recent file \\begin{verbatim}"), sprintf(currentFile), sprintf("\\end{verbatim}"), "\n\n")

exportFile <- read.csv("contacts-export.csv", header=TRUE, stringsAsFactors=FALSE,  na.strings="")

numObs <- nrow(exportFile)
numAdd <- nrow(exportFile[!is.na(exportFile$raw_user_address), ])
      
#String match
#exportFile$raw_user_address

#basic regular expression
basicAddress <- "^[0-9]{1,5}[[:space:]]?[[:alpha:]]+"

#test regular expresssion
#grep("536 jules street malven jhb", pattern = basicAddress, value=TRUE)

#create combined Variable for USSD channels and other channels
exportFile$combDeliverClass <- NA
exportFile$combDeliverClass[exportFile$delivery_class %in% c("gtalk","twitter","mxit")]  <-  exportFile$delivery_class[exportFile$delivery_class %in% c("gtalk","twitter","mxit")]
exportFile$combDeliverClass[exportFile$delivery_class %in% c("ussd")] <- as.character(exportFile$USSD_number[exportFile$delivery_class %in% c("ussd")])

#add column to matrix
exportFile$wellFormed <- unlist(sapply(exportFile$raw_user_address, simplify=TRUE,
      function(x) grepl(x, pattern=basicAddress)
      ))

#Create subset
addressSubset <- exportFile[( !is.na(exportFile$raw_user_address)| !is.na(exportFile$ward)),
           c("raw_user_address","raw_user_address_2", "ward", "wellFormed", "combDeliverClass", "created_at")]

#Digit fields
numGoodWards <- length(na.omit(addressSubset$ward))
numGoodAddresses <-sum(addressSubset$wellFormed[!is.na(addressSubset$ward)], na.rm =TRUE)
wfform <- nrow(addressSubset[addressSubset$wellFormed==1 & !is.na(addressSubset$ward), ])
mismatch2 <- nrow(addressSubset[addressSubset$wellFormed==1 & is.na(addressSubset$ward), ])

##Engagement question
numEng <- length(na.omit(exportFile$engagement_question))
cat(sprintf("Of %d observations, %d or %f of the observations have answered the engagement question", numObs,numEng, numEng/numObs), "\n")
cat("Here is the breakdown of the answer to that question")
print(xtable(table(exportFile$engagement_question)))
print(xtable(table(exportFile$engagement_question)/numEng))
cat("\n\n")

##Registration
##THERE IS  A BUG HERE BECAUSE WE HAVE A FEW PEOPLE WHO DON"T HAVE A DELIVERY CLASS
numReg = sum(exportFile$is_registered %in% c("true"), na.rm=TRUE)
cat(sprintf("Of %d that answered the engagement question, %d or %f of the those **engaged** have answered the registeration question and agreed to the T&Cs.
So we are losing %f here", numEng,numReg, numReg/numEng, (numEng-numReg)/numEng), "\n")

cat("Of those that answer the registration question --- let us look at the T&Cs, who joins")

exportFile$is_registered <- as.factor(exportFile$is_registered)
exportFile$USSD_number<- as.factor(exportFile$USSD_number)

#exportFile[sample(nrow(exportFile),200), c("delivery_class", "USSD_number","combDeliverClass")]

#regbyChannel <- with(exportFile, crosstab(is_registered, USSD_number, chisq=TRUE))
regbyChannel <- with(exportFile[!is.na(exportFile$delivery_class),], crosstab(is_registered, USSD_number, chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE))

regbyChannel2 <- with(exportFile[!is.na(exportFile$combDeliverClass),], crosstab(is_registered, combDeliverClass, chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE))

#*120*7692*2#	Standard Rates
#*120*4729#2	Free
#*120*7692*3#	Stand a chance to win

threechannels <- c("*120*7692*2#", "*120*4729#", "*120*7692*3#")

ussdex <- exportFile[exportFile$USSD_number %in% threechannels, ]


notthree <- -which(unique(ussdex$USSD_number) %in% threechannels)

ussdex$ussdChannel <- car::Recode(ussdex$USSD_number,
                                  " '*120*7692*2#' = 'Standard Rates' ;
                                    '*120*4729#' = 'Free' ;
                                    '*120*7692*3#' = 'Lottery'
                                     "
)

##CSOS

cat(sprintf("\\section{CSOs}"), "\nHere are the results of the CSOs", "\n")

#csos$msisdn <- paste0("+", csos$Mobile..)

engCsos <- exportFile[exportFile$msisdn %in% giveCSONums() , ]
engKP <- exportFile[exportFile$msisdn %in% giveKPNums() , ]

cat(sprintf("There are %d professional CSOS in the VIP dataset", nrow(engCsos)))
cat(sprintf("There are %d KP Observers in the VIP dataset", nrow(engKP)))
                 

#############
###Viz stuff
###########
library(scales)


awQuest <- c("answerwin_question_gender", "answerwin_question_age", "answerwin_question_2009election", "answerwin_question_race")


## ## ggplot(ussdex[!is.na(ussdex$answerwin_question_race), ], aes(as.factor(answerwin_question_race), fill=USSD_number)) + geom_bar(position = "dodge") +  scale_y_continuous(labels = percent)
## ##     xlab("Average Slope (Degrees)")
## ## +ylab("Plots (#)")
Pause <- function () { 
    cat("Hit <enter> to continue...")
    readline()
    invisible()
}


## ggplot(ussdex[!is.na(ussdex$answerwin_question_race), ], aes(as.factor(answerwin_question_race), fill=USSDChannel)) + facet_grid(~USSDChannel)


## for(i in 1:length(awQuest)) {
##     sjp.grpfrq(as.factor(ussdex[!is.na(ussdex[, awQuest[i]]),awQuest[i] ] ),
##                as.factor(ussdex[!is.na(ussdex[, awQuest[i]]), "ussdChannel" ] ),
##                startAxisAt=0, na.rm=TRUE,
##                useFacetGrid=TRUE)
##   Pause()


## }
## sjp.grpfrq(as.factor(ussdex$answerwin_question_race[!is.na(ussdex$answerwin_question_race)]), as.factor(ussdex$ussdChannel[!is.na(ussdex$answerwin_question_race)]), startAxisAt=0, na.rm=TRUE)


## sjp.grpfrq(as.factor(ussdex$answerwin_question_race[!is.na(ussdex$answerwin_question_race)]), as.factor(ussdex$ussdChannel[!is.na(ussdex$answerwin_question_race)]), startAxisAt=0, na.rm=TRUE,
## useFacetGrid=TRUE)


#df <- ddply(ussdex[!is.na(ussdex$answerwin_question_race), ], .(test1), transform, p = Freq/sum(Freq))
# and plot
#ggplot(df, aes(test2, p))+geom_bar()+facet_grid(~test1)



############
regbyChannelxtab <- xtable(regbyChannel2, caption="Accept T&Cs by Channel") #usepackage{float} in tex

#doesn't work -- coudl change the factor in delivery class
#colnames(regbyChannelxtab) <- c("xtab", "c4279", "c1", "c2", "c3", "c8", "chash", "gtalk", "mxit", "twit", "TOTAL") 

print(regbyChannelxtab,
      table.placement="H", include.rownames = FALSE, size = "footnotesize", rotate.colnames=TRUE)
#plot(regbyChannel)

regbyChannel <- with(exportFile[!is.na(exportFile$delivery_class),], crosstab(is_registered, USSD_number,
                                                                                     chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)
                     )

cat(sprintf("The dataset currently has %d observations, of which %d have filled out the address field, which is %f.", numObs, numAdd, numAdd/numObs), "\n\n")
cat(sprintf("Of the addresses, there are currently %d addresses where the ward looked up went through
of those, %d addresses that meet basic formatting requirements of **number** then **text**, which constitutes %f of all ward matches.

There are currently %d cases where the address is not well formed but there is still a ward match
and %d where the address is well formed but there is not a match.",
            numGoodWards, numGoodAddresses, numGoodAddresses/numGoodWards, wfform, mismatch2), "/n")

#########################
######RANDOMIZATION######
########################
cat(sprintf("\\section{Randomization Test}"), "\nHere are the results of the randomizaton \n
There are three groups A, B, C. A got the location lookup bypassed, B had to redo the location but it wasn't primed in the message, and C had to redo and the message was primed")

rgroupsPath <- "/Volumes/Optibay-1TB/RSA_RCT/Randomization_Test"
rgroupFile <- "AllGroups646.csv"

rgroupsdf <- read.csv(file=file.path(rgroupsPath, rgroupFile), header=TRUE, stringsAsFactors=FALSE)
rgroupsdf$MSISDN <- as.character(rgroupsdf$MSISDN)
rgroupsdf$msisdn <- paste0("+",rgroupsdf$MSISDN)

t1 <- merge(exportFile, rgroupsdf, by="msisdn", all.x=TRUE, all.y=TRUE)
#get rid of the NAs -- assume they are a bug
t1 <- t1[!is.na(t1$delivery_class),]
#just aribritarily remove one of the duplicate records for analytical purposes 
t2 <- t1[!duplicated(t1$msisdn, fromLast=FALSE), ]

t2$wardMatch <- !is.na(t2$ward)
t2$awComp <- !is.na(t2$answerwin_complete)

print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(answerwin_question_2009election, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Answered the Election Answer and Win question by Randomization group"
    ),
       table.placement="H")

print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(awComp, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Answered the entire answer and win section by Randomization group"
    ),
       table.placement="H")


print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(wellFormed, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Is the address well formed by Randomization group"
    ),
     table.placement="H"
      )

print(xtable(
    with(t2[!is.na(t2$Name), ], crosstab(wardMatch, Name,
                                         chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)),
    caption = "Ward lookup when through by Randomization group"
   ),
       table.placement="H"
      )

###Addrees Look Uo
##first do manual cleanup

## #if there is no second address use the first address
## addressSubset$cleanAddress <- NA
## addressSubset$cleanAddress[!is.na(addressSubset$raw_user_address_2)] <- tolower(addressSubset$raw_user_address_2[!is.na(addressSubset$raw_user_address_2)])
 
## addressSubset$cleanAddress[is.na(addressSubset$raw_user_address_2) & !is.na(addressSubset$raw_user_address)]  <-
##                                   tolower(addressSubset$raw_user_address[is.na(addressSubset$raw_user_address_2) & !is.na(addressSubset$raw_user_address)])


## cAdd <- addressSubset[!is.na(addressSubset$cleanAddress),]

## mainstr <- "[0-9]{1,4}[[:blank:]]?main.*pretoria>*"

## #need text before or after can recoup. Everyone with nothing gets wiped
## wardstr <- "ward[[:blank:]]?[[0-9]{1,2}.*"

## wardstr2 <- "(^.*)(ward[[:blank:]]?[0-9]{1,2})(.*$)"

## #cAdd[grepl(mainstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]
## #cAdd[grepl(wardstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]

## sprintf("The number of main streets or something similar in pretoria is %d", nrow(cAdd[grepl(mainstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]
##                                                                                  )
##         )

## sprintf("The number of people who enetered their ward # is %d", nrow(cAdd[grepl(wardstr, cAdd$cleanAddress), c("cleanAddress", "ward","wellFormed") ]
##                                                                                  )
##         )

## wards <- str_match(cAdd$cleanAddress, wardstr2)

## cAdd$ward <- wards[,3]
## cAdd$otherWardInfo <- paste(wards[,2], wards[,4], sep =" ")

##wards[!is.na(wards[,1]), ]
##cAdd[!is.na(cAdd$ward), ]
##doesn't likt the word village or stand
##if there is a second address, use the second address
##configure adi's flags

##duplicate records
##duplicatedR <- t1[duplicated(t1$msisdn, fromLast=TRUE) | duplicated(t1$msisdn, fromLast=FALSE) , ]
##head(duplicatedR[,c("msisdn", "delivery_class", "USSD_number")], 100)
##write.csv(duplicatedR[,c("msisdn", "delivery_class", "USSD_number")],
##                      file.path(rgroupsPath, "duplicatesinVumi.csv"))
##dupRec <- length(unique(duplicatedR$msisdn))
###########

##output a csv of the address concerns
write.csv(addressSubset, file=paste0("AddressSubset","_", Sys.time(), ".csv"))

########
#answer and win questions
##########
registered <- exportFile[exportFile$is_registered %in% c("true"),  ]

cat(sprintf("\\section{Answer And Win}"), "\nHere is the data from the Answer and Win Section:")


registered$awComplete <- apply(registered[, awQuest], 1, function(x) sum(!is.na(x)))


for(qu in 1:length(awQuest)){
    regxtable <- tab1(registered[, awQuest[qu]], graph=FALSE)$output.table
    capt <- paste(gsub("_", " ",awQuest[qu]),sprintf(": %d People have given answer to this question", sum(regxtable[1:(nrow(regxtable)-2),1]) ) )
    print(xtable(rename_NA(regxtable),
                 caption = capt
                 ),
          table.placement="H"
          )
}

cat(sprintf("\\subsection{Answer And Win by Channel}"), "\nHere is the data from the Answer and Win Section by channel:")


for(qu in 1:length(awQuest)){
    ctabchannel <- with(registered, crosstab(get(awQuest[qu]), combDeliverClass,
                                           chisq=TRUE, prop.c=TRUE, prop.r=TRUE, plot=FALSE)
                        )
    
    print(xtable(ctabchannel,
                 caption = gsub("_", " ",awQuest[qu])
                 ),
                 table.placement="H", include.rownames = FALSE, size = "footnotesize", rotate.colnames=TRUE
          )
}


awFin <- length(registered$awComplete[registered$awComplete==4])
cat(sprintf("At total of %d people have finished the answer and win section, answering all the questions", awFin ))

########
#VIP questions
##########

cat(sprintf("\\section{VIP Section}"), "\nHere is the data from the VIP Section:\n")
cat(sprintf("%d people have completed all of the questions in the VIP section", length(na.omit(registered$vip_complete)) ), "\n") 


vipLabels <- c("Attended protest",
"Registered to vote",
"Likely to vote",
"Political party",
"Community protests",
"Violent protests",
"Neighbours",
"Neighbours judgement",
"Zuma performance",
"Local gov performance",
"Party Contact",
"Attended Campaign rally")


vipQuiz <- paste0("vip_question_", seq(1,12))

for(vp in 1:length(vipQuiz)){
    regxtable <- tab1(registered[, vipQuiz[vp]], graph=FALSE)$output.table
    capt <- paste(gsub("_", " ", vipQuiz[vp]), "--", vipLabels[vp],sprintf(": %d People have given answer to this question", sum(regxtable[1:(nrow(regxtable)-2),1]) ) )
    print(xtable(rename_NA(regxtable),
                 caption = capt
                 ),
                 table.placement="H"
          )
}

########
#Whats up?
##########
cat(sprintf("\\section{What's up}"), "\nHere is the data from the What's up section")

cat(sprintf("%d people have completed all of the questions in the What's Up section", length(na.omit(registered$whatsup_complete)) ) ) 

wutUp <- grep("^whatsup_question_", names(exportFile), value=TRUE)

for(wu in 1:length(wutUp)){
    regxtable <- tab1(registered[, wutUp[wu]], graph=FALSE)$output.table
    print(xtable(rename_NA(regxtable),caption = gsub("_", " ", wutUp[wu])),  table.placement="H")
}


###############################
##visualize count of incoming messages by hour
##################################

#x <- as.xts(1:3, timeDate(exportFile$created_at))

#strptime(exportFile$created_at[1], format="%Y-%m-%d %R", tz="Africa/Johannesburg")

exportFile$posixTime <- as.POSIXct(format(strptime(exportFile$created_at, format="%Y-%m-%d %R", tz="GMT"),tz="Africa/Johannesburg", usetz=TRUE))

MyDatesTable <- table(cut(exportFile$posixTime, breaks="hour"))

databyChannelUSSD <- as.data.frame.matrix(table(cut(exportFile$posixTime, breaks="hour"), as.factor(exportFile$combDeliverClass)))

#need to expand code to automatically match channels
names(databyChannelUSSD) <-c("vippoice2014", "c4279", "channel1", "channel2", "channel3", "channel8", "channelonlyhash", "c8864", "gtalk", "mxit", "twitter", "vipvoig_gmail")

databyChannelUSSD$date <- as.POSIXct(rownames(databyChannelUSSD))

#update by channel as needed -- currently for USSD only
tcByChannel <- as.data.frame.matrix(table(cut(exportFile$posixTime, breaks="hour"), paste(as.factor(exportFile$combDeliverClass), exportFile$is_registered)))
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
