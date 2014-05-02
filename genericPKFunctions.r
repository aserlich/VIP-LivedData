phoneNumPattern <- function() {
    return("\\+27{0-9}{9}")
}

giveKPNums <- function() {
    fileDir <- "/Volumes/Optibay-1TB/RSA_RCT/QA/CSOs"
    load(file.path(fileDir,"KPobservers.RData"))
    kpNums <-paste0("+",
                    str_trim(
                        unlist(
                            sapply(KPobservers$msisdn,
                                   function(x) str_split(x, "/{1,2}")
                                   )
                            )
                        )
                    )

validKPNums <- kpNums[grepl(pattern=phoneNumPattern(), kpNums)]
return(validKPNums)
}

giveCSONums <- function() {
    csodir <- "/Volumes/Optibay-1TB/RSA_RCT/QA/CSOs"
    csos <- read.csv(file.path(csodir, "Total CSO Observers - Total CSO Observers.csv"))
    csos$msisdn <- paste0("+", csos$Mobile..)
    validCSONums <- csos$msisdn[grepl(pattern=phoneNumPattern(),  csos$msisdn)]
    return(validCSONums)
}
