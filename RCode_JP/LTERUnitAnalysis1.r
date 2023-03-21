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
  print(paste(packageId,unit,sep=" "))
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

xmlUnitsList<-xml_find_all(xmldata,"//unit")
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

write.csv(df3,"RawUnitList.csv",row.names=F)

# Now summarize by packageID
df3$unitUseCount<-1
ag1<-aggregate(unitUseCount~packageId+unit,data=df3,sum)

# now summarize by scope
ag1$scope<-gsub("[[:digit:]\\.]","",ag1$packageId)

ag1$scopeUseCount<-ag1$unitUseCount
ag1$pkgUseCount<-1
ag2<-aggregate(scopeUseCount~scope+unit,data=ag1,sum)
ag2a<-aggregate(pkgUseCount~scope+unit,data=ag1,sum)
ag2<-merge(ag2,ag2a)
# now summarize by keyword
ag2$scopeUses<-1
ag3<-aggregate(scopeUses~unit,ag2,sum)
ag2$totalUses<-ag2$scopeUseCount
ag4<-aggregate(totalUses~unit,ag2,sum)
ag5<-aggregate(pkgUseCount~unit,ag2,sum)

m1<-merge(ag3,ag5)
m2<-merge(m1,ag4)

m2<-m2[order(m2$scopeUses,m2$pkgUseCount,m2$totalUses,m2$unit,decreasing=T),]

write.csv(m2,"UnitUseSummary.csv",row.names=F)



