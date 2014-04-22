library(plyr)

##This code prepares the YAL data to be integarated with the the main vumi go data for push over sms to be election monitors

path <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/YAL_LivedData"
myfile <- "survey_results_april22.csv"
yaldf <- read.csv(file=file.path(path,myfile), header=TRUE, stringsAsFactor=FALSE)

phonNums <- unique(yaldf$token)
phonNums <- phonNums[!grepl("^([a-z]|01|[5-9]|1)", phonNums)]
phonNums2 <- ifelse(grepl("^0[6-9]", phonNums), gsub("^0([6-9])", "\\+27\\1", phonNums), paste0("+", phonNums))

mobiUser <- data.frame(msisdn = phonNums2, is_registered="true", delivery_class="mobi", USSD_number=NA, key="NA")

