#First edition Grant Buckles
#Rewritte aserlich
#file to read in observer list and pump out various things
#May 1 2013

#rm(list=ls())
library(xlsx)
#library(RGoogleDocs)
 
## auth = getGoogleAuth("aaron.erlich@gmail.com", gpass)
## sheets.con = getGoogleDocsConnection(auth, "...", service = "wise")
## a = getDocs(sheets.con)
## ts = getWorksheets(a$`Copy of Knowledge Pele Observers_MASTER LIST`, sheets.con)
## ts2 <- getWorksheets(

fileDir <- "/Volumes/Optibay-1TB/RSA_RCT/QA/CSOs"
fileName <- "Knowledge Pele Observers_MASTER LIST_20140501.xlsx"

sheetNames <- c("Limpopo","Gauteng","Western Cape","Eastern Cape","KZN")
myRows <- c("Name", "Surname","Confirmed.Cell.No..4.PM", "GIS.Block")

all.tabs<-NULL

for (j in 1:length(sheetNames)){
  obsDat <- read.xlsx(file = file.path(fileDir, fileName), sheetName=sheetNames[j], colClasses="character", na.strings="", keepFormulas=FALSE)
  obsDat <- obsDat[, myRows]
  print(nrow(obsDat))
  tab <- rep(sheetNames[j],nrow(obsDat)) 
  s.frame <- seq(1:nrow(obsDat))
  df <- data.frame(tab,s.frame,obsDat) 
  all.tabs <- rbind(all.tabs, df) 
  rm(obsDat, tab, s.frame, df) 
}

allTabs2 <- all.tabs[!is.na(all.tabs$Name) & !is.na(all.tabs$Confirmed.Cell.No..4.PM), ]
table(allTabs2$tab)


names(allTabs2) <- c("province", "provNum", "name", "surname", "msisdn", "GIS_Block")
table(allTabs2$GIS_Block)

#one person has two phone numbers
KPobservers <- allTabs2
rm(allTabs2)
save(KPobservers, file = file.path(fileDir,"KPobservers.RData"))


