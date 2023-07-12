# Extract units from EML Metadata
rm(list=ls())
userName<-Sys.getenv("USERNAME")
setwd(paste("C:/users",userName,"box sync/EMLUnits",sep='/'))
#setwd("D:/box sync/EMLUnits")

library(tidyverse)
# Read in PackageIds and keywords from XML Resultset into a data frame
library(xml2)

extractUnit<-function(x){
  unit<-as.character(xml_text(xml_children(xmlUnitsList[x])))
  #print(paste(packageId,unit,sep=" "))
  #print(unit)
#  unit<-tolower(trimws(unit))
  if (length(unit)== 0){
    unit<-'none'
  }
  dfx<-data.frame(packageId,unit,stringsAsFactors=F)
  return(dfx)
}
  


EMLlist<-list.files(path="EMLCorpus",
                    pattern='*',full.names=TRUE,ignore.case=TRUE)
firstData<-TRUE
for (inXMLFile in EMLlist){
xmldata<-read_xml(inXMLFile)
xmlIdList<-xml_find_all(xmldata,"//eml:eml/@packageId")
packageId<-as.character(xml_text(xmlIdList[1]))

xmlUnitsList<-xml_find_all(xmldata,"//attribute//unit")
if (length(xmlUnitsList) > 0){
df1 <- extractUnit(1)
if (length(xmlUnitsList) > 1){
for (i in 2:length(xmlUnitsList)) {
  #print(i)
  df2<-extractUnit(i)
  if (length(df2) > 0){
    df1<-rbind(df1,df2)
  }
}
}
if (firstData){ 
  df3<-df1
}else {
  df3<-rbind(df3,df1)
  }
firstData<-FALSE
}
} # end for

#eliminate blank units
df3<-df3[df3$unit !="",]
# eliminate whitespace
df3$unit<-trimws(df3$unit)

#write.csv(df3,"RawUnitList.csv",row.names=F)

# Now summarize by packageID
df3$unitUseCount<-1
ag1<-aggregate(unitUseCount~packageId+unit,data=df3,sum)

# now summarize by scope
ag1$scope<-gsub("[[:digit:]\\.]","",ag1$packageId)

ag1a<-aggregate(unitUseCount~scope+unit,ag1,sum)


ag1a<-ag1a[order(ag1a$scope,ag1a$unitUseCount,ag1a$unit,decreasing=c(FALSE,TRUE,FALSE),method="radix"),]


# Use Web service to find the QUDT Units that match known units
ag1a$qudtUnit=""

for(obsNum in seq(1:nrow(ag1a))){
  print(paste("http://www.vcrlter.virginia.edu/data/test_unitsws1.php?rawunit=",url_escape(ag1a[obsNum,]$unit),sep=""))
  qudtUnit<-readLines(con=paste("http://www.vcrlter.virginia.edu/data/test_unitsws1.php?rawunit=",url_escape(ag1a[obsNum,]$unit),sep=""),warn=F)
print(qudtUnit)
    if (qudtUnit != "No_Match" & !is.na(qudtUnit)){
    ag1a[obsNum,]$qudtUnit<-qudtUnit
  }
}
ag1a$qudtURI<-""
ag1a$qudtURI<-ifelse(ag1a$qudtUnit=="","",paste0("http://qudt.org/vocab/unit/",ag1a$qudtUnit))
ag1a$site<-toupper(gsub("knb-lter-","",ag1a$scope))


write.csv(ag1a[,c(1,3,2,4,5)],"SiteBySiteUnits.csv", row.names=F)

library(openxlsx)


wb<-createWorkbook()
for (mySite in levels(as.factor(ag1a$site))){
  print(mySite)
  myDf<-ag1a[ag1a$site == mySite,c(2,4,3,5,6)]
  addWorksheet(wb,mySite)
  writeDataTable(wb,sheet=mySite,myDf)
  setColWidths(wb,mySite,cols=c(1:6),widths="auto")
}

saveWorkbook(wb,"SiteBySiteUnits.xlsx",overwrite=TRUE)



