###FILE TO READ in ELECTION OBSERVATION DATA
## AND VALIDATE ID BASED ON CHECK SUM
## AND EXPORT ONLY THOSE WITH EITHER A VALID ID OR A SCHOOL
rm(list=ls())
library(car)
library(plyr)
library(epicalc) #contains tab1
library(descr) #contains crosstab

digits <- function(x) {
    if(length(x) > 1 ) {
        lapply(x, digits)
    } else {
        n <- nchar(x)
        rev( x %/% 10^seq(0, length.out=n) %% 10 )
    }
}

#Validates ID Number according to the Procedure Described at http://xml-fx.com/services/saidvalidator.aspx
validateid<-function(idnumberstring){
    if(is.na(idnumberstring)){
        return(FALSE)
    } else {
                                        #Calculate sum of odd digits
        step1<-sum(as.numeric(substr(idnumberstring,1,1)),
               as.numeric(substr(idnumberstring,3,3)),
               as.numeric(substr(idnumberstring,5,5)),
               as.numeric(substr(idnumberstring,7,7)),
               as.numeric(substr(idnumberstring,9,9)),
               as.numeric(substr(idnumberstring,11,11))
               )
                                        #Merge the even digits into a single number, double it, and sum its digits
        step2<-sum(
            digits(
                as.numeric(paste0(substr(idnumberstring,2,2),
                          substr(idnumberstring,4,4),
                          substr(idnumberstring,6,6),
                          substr(idnumberstring,8,8),
                          substr(idnumberstring,10,10),
                          substr(idnumberstring,12,12)))*2
            )
        )
                                        #Subtract the trailing digit of the sum of Steps 1 and 2 from 10 to Get the Check Digit
    checkdigit<-10-tail(digits(step1+step2),1)

        if (checkdigit==as.numeric(substr(idnumberstring,13,13))) {
            return(TRUE)
	} else {
            return(FALSE)
        }
    }
}

path2 <- "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/ObserverRecruitment"

myfile <- list()
myfile[[1]] <- "obs-contacts-export-incentivized_2014_04_24_8:34.csv"
myfile[[2]] <- "incentivesMxitGroup2014-04-27.csv"
myfile[[3]] <- "NoIncentiveGroup2014-04-26.csv"
myfile[[4]] <- "NoIncentivesMxitgroup2014-04-28.csv"
myfile[[5]] <- "noIncentiveGroup2014-04-28.csv"
inc <- read.csv(file=file.path(path2,myfile[[1]][1]), header=TRUE, stringsAsFactor=FALSE, na.strings="")
for(f in 1:(length(myfile)-1)) {
    inc <- rbind(inc,
                 read.csv(file=file.path(path2,myfile[[f+1]][1]), header=TRUE, stringsAsFactor=FALSE, na.strings="")
                 )
    print(cat("this is the nmber of row:", nrow(inc), myfile[[f+1]][1]))
                 
}

inc$name[inc$name == "noIncentiveMxitgroup"] <- "noIncentive"

inc[!(inc$name %in% c("noIncentive", "Incentive")), c("key", "name", "msisdn")]
inc$name[!(inc$name %in% c("noIncentive", "Incentive"))] <- NA

inc$obstownsuburb[!is.na(inc$obstownsuburb)]
length(inc$obsvotingstation[!is.na(inc$obsvotingstation)])
length(inc$obsidnumber[!is.na(inc$obsidnumber)])
inc$obsidnumber[!is.na(inc$obsidnumber)]

inc$obsidnumber <- toupper(inc$obsidnumber)
inc$obsidnumber2 <- gsub(" ", "", inc$obsidnumber)
inc$finalID <- as.character((str_match(inc$obsidnumber2,"^.*([0-9]{13}).*$")[,2]))
inc$validid <- unlist(sapply(inc$finalID, validateid))

#EC 2. FS 3. GP 4. KZN 5. Limp 6.MP 7. NCape 8 N West 9. WCape

inc2 <- inc[inc$validid  | (!is.na(inc$obsvotingstation) & str_length(inc$obsvotingstation) > 5), ]

prov <- c("limp", "wcape", "ec", "fs", "kzn", "mp", "nwest", "ncape", "gp")
error <- setdiff(unique(inc2$obsprovince),prov )

inc2$province2 <- car::Recode(inc2$obsprovince,
                         "
  'limp'='Limpopo';
  'ec'='Eastern Cape';
  'fs'= 'Free State';
  'kzn' = 'Kwazulu-Natal';
  'mp' = 'Mpumalanga';
  'nwest' = 'Northwest Province';
  'wcape' = 'Western Cape';
  'ncape'= 'Northern Cape';
  'gp' = 'Gauteng';
   error = NA
 "
                         , as.factor.result=TRUE)

tab1(inc2$province2, graph=FALSE)

with(inc2, crosstab(province2, validid))

inc2 <- inc2[order(inc2$validid, decreasing=TRUE),]

inc2[ , c("registeredVoter", "stationNameIEC", "stationIDIEC", "manualMatch")] <- NA

inc2 <- inc2[ , -which(names(inc2) %in% c("surname", "email_address", "dob", "bbm_pin", "wechat_id"))]

write.csv(inc2, file = file.path(path2, paste0("incentivegroup", Sys.Date(), '.csv')))

