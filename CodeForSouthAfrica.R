library(RCurl)
library(stringr)
library(rjson)
library(parallel)
options(stringsAsFactors=F)
library(sp)
votingstations<-read.csv("~/Downloads/IEC - Voting station list - 2014 - Voting stations.csv")
testdata<-read.csv("~/Downloads/Test_LookupDataset - Sheet1.csv",colClasses="character")
colnames(testdata)[4]<-"Station"
colnames(testdata)[5]<-"NationalID"
#Lose the Unneeded Columns
votingstations<-votingstations[,c(1,3,4,6,7,8)]

#From https://stat.ethz.ch/pipermail/r-help/2011-March/270786.html; Splits the digits out of a number
digits <- function(x) {
	if(length(x) > 1 ) {
		lapply(x, digits)
	} else {
		n <- nchar(x)
		rev( x %/% 10^seq(0, length.out=n) %% 10 )
	}
}
#Validates ID Number according to the Procedure Described at http://xml-fx.com/services/saidvalidator.aspx
validateid<-function(idnumberstring)
{
	#Calculate sum of odd digits
	step1<-sum(as.numeric(substr(idnumberstring,1,1)),as.numeric(substr(idnumberstring,3,3)),as.numeric(substr(idnumberstring,5,5)),as.numeric(substr(idnumberstring,7,7)),as.numeric(substr(idnumberstring,9,9)),as.numeric(substr(idnumberstring,11,11)))
	#Merge the even digits into a single number, double it, and sum its digits 
	step2<-sum(digits(as.numeric(paste0(substr(idnumberstring,2,2),substr(idnumberstring,4,4),substr(idnumberstring,6,6),substr(idnumberstring,8,8),substr(idnumberstring,10,10),substr(idnumberstring,12,12)))*2))
	#Subtract the trailing digit of the sum of Steps 1 and 2 from 10 to Get the Check Digit
	checkdigit<-10-tail(digits(step1+step2),1)
	if (checkdigit==as.numeric(substr(idnumberstring,13,13)))
	{
		return(T)
	}
	return(F)
}



#This function takes a vector of prepared strings, where each one contains the appropriately formatted city+province info, calls the API and returns the result - Note that the ward result is not necessarily very trustworthy in that the town name often does not uniquely identify a ward
#Pause interval, in seconds, is to slow down API calls; the function sleeps for pauseinterval before excecuting
doAPICall<-function(sometown,pauseinterval=0)
{
	Sys.sleep(pauseinterval)
	call<-paste0("wards.code4sa.org/?address=",sometown)
	#Donwload
	res<-try(getURL(call))
	#On failure, simply retry
	if (class(res)=="try-error")
	{
		res<-try(getURL(call))
	}
	#If it fails twice, assume something is wrong
	if (class(res)=="try-error")
	{
		return(NA)
	}
	#Parse results and return
	parsed<-unlist(lapply(res,fromJSON),recursive=F,use.names=F)
	parsed<-lapply(parsed,unlist)
	parsed<-do.call(rbind,parsed)
	if (str_detect(res,"address not found"))
	{
		return(NA)
	}
	return(rbind(parsed[,c("province","coords1","coords2","address","ward")]))
}
#Takes a Row and Combines/Formats the Town/Province Info - Strips out any non alphanumeric, other than the ! - Bad Idea? Expand permissible character set?
townTransform<-function(town,province)
{
	town<-paste0(str_extract_all(town,"([A-Z]|[a-z]|[0-9]|!)+")[[1]],collapse="+")
	province<-paste0(str_extract_all(province,"([A-Z]|[a-z]|[0-9]|!)+")[[1]],collapse="+")
	return(paste(town,province,sep="+"))
}

#For debugging
if (F){
CompDist=150
Percentile=0.65
MaxDist=40
UncommonThresh=400
LWeight=0.5
GeoWeight=0.2
MatchedWeight=35
UncommonWeight=50
TownWeight=15}

#It is assumed that a row includes fields named "Province", "Town","Station",and "NationalID"
#Settings:
#CompDist - Should be large - used to generate a radius of comparison matches
#Percentile - Among All Stations within MaxDist of the Town given, a match must be in the X percentile of all matches.  That is, if Percentile = 0.10, then only the 10% of station names that are closest, in terms of Levenshtein Distance, to the station name given will be considered for matching - Turns out that this should be set to a very large value
#MaxDist - the maximum geographical distance to the matching station.  Should not be too small. 
#UncommonThresh - extra "credit" is given for matches to include unusual words.  This threshold sets the number of occurrences in the corpus as a whole (e.g., the number of polling station names containing the word) for a word to be considered unusual.

#The best match and confidence are selected by a score, that is a weighted sum of the Levenshtein Distance, Geographical Distance, Total Words Nearly Matched (e.g., extra credit is given for each word that is closely matched), Total Unusual Words Matched, and Geographic Placement in the Same Town as the User Input.  The default weights here are nearly arbitrary - ideally they would be calibrated on data.  Note that geographic distance is squared, so its coefficient should remain relativey small


doEntry<-function(somerow,CompDist=150,Percentile=0.65,MaxDist=40,UncommonThresh=200,LWeight=0.5,GeoWeight=0.2,MatchedWeight=35,UncommonWeight=50,TownWeight=15,googleKey="AIzaSyBrdERRoZCaxMlqvE_CUZO-4ZrLQ3K22Mc")
{
	somerow<-as.vector(somerow)
	#Case Doesn't Matter Here - So We'll Work only in Uppercase
	station<-toupper(somerow["Station"])
	#Make the API call - Passing both the Town and Province Given my the User
	APIres<-doAPICall(townTransform(somerow[,"Town"],somerow[,"Province"]))
	wrongProvince<-F
	provinceOnly<-F
	#If the call fails, try the API call removing the Province - in case the user entered that incorrectly
	if (is.na(APIres))
	{
		APIres<-doAPICall(townTransform(somerow[,"Town"],""))
		if (!is.na(APIres))
		{
			wrongProvince<-T
		}	
	}
	#If you still can't identify the town, just use the Province as a whole as the location.  In this case, the kilometer distance and town metrics are useful, so the weights on these are taken to zero.
	if (is.na(APIres))
		{
			APIres<-doAPICall(townTransform(somerow[,"Province"],""))
			MaxDist<-150
			GeoWeight<-0
			TownWeight<-0
			provinceOnly<-T
		}
		#Sometimes the API returns multiple results - never more than two.  If these are close to one another, just use the first one - otherwise die (potentially undesirable behavior)
	if (nrow(APIres)>1)
	{
		if (abs(as.numeric(APIres[1,"coords1"])-as.numeric(APIres[2,"coords1"])<0.05)&(abs(as.numeric(APIres[1,"coords2"])-as.numeric(APIres[2,"coords2"])<0.05)))
			{
				APIres<-rbind(APIres[1,])
			}
			else
			{return(NA)}
		}
		#Identify all Stations within 1 degree latitude and 1 degree longitude of the geolocated result (deliberately broad bounding box) - This is done to draw a distribution of Levenshtein distances for comparison purposes
		nearby<-(abs(votingstations[,"Latitude"]-as.numeric(APIres[,"coords1"])<1)&abs(votingstations[,"Longitude"]-as.numeric(APIres[,"coords2"])<1))
		tocheck<-votingstations[nearby,]
		mypoint<-c(as.numeric(APIres[,"coords2"]),as.numeric(APIres[,"coords1"]))
		#Calculate distances in KM from the coordinates
		dists<-spDistsN1(as.matrix(tocheck[,c("Longitude","Latitude")]),mypoint,longlat=T)
		tocheck<-tocheck[dists<CompDist,]
		dists<-dists[dists<CompDist]
		#Calcualte L distance to all of the names within CompDist KM
		LevenshteinDistance<-adist(station,tocheck[,"Voting.Station.Name"], costs=list("insertions"=1,"deletions"=0.4,"substitutions"=1))
		#Reject the distances above the Percentile cutoff
		probable<-(LevenshteinDistance<=quantile(LevenshteinDistance,Percentile))&(dists<MaxDist)
		finalists<-cbind(tocheck[probable,],LevenshteinDistance[probable],dists[probable])
		colnames(finalists)[7]<-"LDist"
		colnames(finalists)[8]<-"GeoDist"
		#Split the user input into words - count how many of these distinct words match closely to words in the original name
		SplitName<-strsplit(station,"\\s+")[[1]]
		wordmatches<-lapply(as.list(SplitName),agrep,finalists[,"Voting.Station.Name"],max.distance=2)
		matchedwords<-rep.int(0,nrow(finalists))
		for (i in 1:length(wordmatches))
		{
			matchedwords[wordmatches[[i]]]<-matchedwords[wordmatches[[i]]]+1
		}
		finalists<-cbind(finalists,matchedwords)
		#Count how many of the uncommon words match
		totalmatches<-unlist(lapply(lapply(as.list(SplitName),agrep,votingstations[,"Voting.Station.Name"],max.distance=2),length))
		uncommonwords<-SplitName[totalmatches<UncommonThresh]
		if (length(uncommonwords)!=0){
		uncommonmatches<-lapply(as.list(uncommonwords),agrep,finalists[,"Voting.Station.Name"],max.distance=2)
		matcheduncommon<-rep.int(0,nrow(finalists))
		for (i in 1:length(uncommonmatches))
		{
			matcheduncommon[uncommonmatches[[i]]]<-matcheduncommon[uncommonmatches[[i]]]+1
		}} else{matcheduncommon<-rep(0.5,nrow(finalists))}
		finalists<-cbind(finalists,matcheduncommon)
		finalists[,"matchedwords"]<-finalists[,"matchedwords"]/length(SplitName)
		if (length(uncommonwords)!=0){
		finalists[,"matcheduncommon"]<-finalists[,"matcheduncommon"]/length(uncommonwords)}
		#Calculate the score for the first time - this includes everything other than the town match
		score<-finalists[,"LDist"]*LWeight+finalists[,"GeoDist"]*GeoWeight+(1-finalists[,"matchedwords"])*MatchedWeight+(1-finalists[,"matcheduncommon"])*UncommonWeight
		#Select only the top 10% as the next step requires an expensive API call
		finalists<-finalists[score<=quantile(score,0.10),]
		score<-score[score<=quantile(score,0.10)]
		if (nrow(finalists)>12)
		{
			finalists<-cbind(finalists,score)
			finalists<-finalists[order(finalists[,"score"]),]
			finalists<-finalists[1:12,]
			finalists<-finalists[,-ncol(finalists)]
		}
		#Now go to the Google API to find the towns in which the Voting Station Finalists are Locatedd
		latlng<-apply(finalists[,c("Latitude","Longitude")],1,paste0,collapse=",")
		urls<-paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=",latlng,"&sensor=false&key=",googleKey)
		googleRes<-getURL(urls)
		towns<-unlist(lapply(googleRes,extractGoogleTown),use.names=F)
		mytown<-extractGoogleTown(getURL(paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=",as.numeric(APIres[,"coords1"]),",",as.numeric(APIres[,"coords2"]),"&sensor=false&key=",googleKey)))
		#Does the town from the user input match the town of the polling place?
		if (!is.na(mytown)){
		TownMatch<-towns==mytown}
		else{TownMatch<-rep(F,nrow(finalists))}
		TownMatch[is.na(TownMatch)]<-F
		finalists<-cbind(finalists,TownMatch)
		#Calculate and Return the Final Score
		score<-finalists[,"LDist"]*LWeight+finalists[,"GeoDist"]*GeoWeight+(1-finalists[,"matchedwords"])*MatchedWeight+(1-finalists[,"matcheduncommon"])*UncommonWeight+(!finalists[,"TownMatch"])*TownWeight
		if (nrow(finalists)>1){
		return(data.frame(finalists[which.min(score),],"score"=min(score,na.rm=T),"nextbest"=sort(score)[2],wrongProvince,provinceOnly))}
		if (nrow(finalists)==1)
		{
			return(data.frame(finalists[which.min(score),],"score"=min(score,na.rm=T),"nextbest"=NA,wrongProvince,provinceOnly))
		}
		return(NA)
}
#Function takes Google JSON output and extracts the name of the town 
extractGoogleTown<-function(googleresult)
{
	pos<-str_locate(googleresult,"\"sublocality\", \"political\"")[,"end"]+1
	potential<-substr(googleresult,pos,pos+300)
	namestart<-str_locate(potential,"\"long_name\" : \"")[,"end"]+1
	nameend<-str_locate(potential,"\",\n")[,"start"]-1
	return(substr(potential,namestart,nameend))
}
#Run the code
system.time({
results<-NULL
for (i in 1:nrow(testdata)){
	print(i)
	results<-rbind(results,doEntry(testdata[i,]))
}})->timetocompletion