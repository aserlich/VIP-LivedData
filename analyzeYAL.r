library(plyr)

##This code prepares the YAL data to be integarated with the the main vumi go data for push over sms to be election monitors

path3 <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/YAL_LivedData"

exports <- list.files(path=path3, pattern ="yalExport_2014.*csv$")

myfile <-  tail(exports,1)[1]

yaldf <- read.csv(file=file.path(path3,myfile), header=TRUE, stringsAsFactor=FALSE, colClasses="character")

questions <- ( unique(yaldf$question))

dfUn <- data.frame(token=unique(yaldf$token),matrix(ncol=length(questions), nrow=length(unique(yaldf$token))))
names(dfUn)[2:length(names(dfUn))] <- make.names(unique(yaldf$question))

## for(i in 1:nrow(yaldf)){
##     qNum <- match(yaldf$question[i], questions)
##     dfUn[yaldf[i, "token"]== dfUn$token , (qNum+1)] <- yaldf$option[i]
##     print(cat("row", i))
## }

phonNums <- unique(yaldf$token)
phonNums <- phonNums[!grepl("^([a-z]|01|[5-9]|1)", phonNums)]
phonNums2 <- ifelse(grepl("^0[6-9]", phonNums), gsub("^0([6-9])", "\\+27\\1", phonNums), paste0("+", phonNums))

mobiUser <- data.frame(msisdn = as.character(phonNums2), is_registered="true", delivery_class="mobi", USSD_number=NA, key="NA")

