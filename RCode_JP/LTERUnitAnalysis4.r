# Try to figure out QUDT codes for units list based on pseudounits
# John Porter, March 2023

rm(list=ls())
#setwd("D:/Box Sync/EMLUnits")
setwd("C:/users/john/Box Sync/EMLUnits")
library(readxl)

df1<-read.csv("PseudoUnitsAll.csv")
df1<-df1[!duplicated(df1$pseudounit),]
df2<-read_excel("SingularAbbrevsTablewQUDT.xlsx",sheet=1)

df1$qudtUnit<-df1$pseudounit

for (i in 1:nrow(df1)){
  for (j in 1:nrow(df2)){
    #    print(paste("_",df3$other[j],"_",sep=""))
    df1$qudtUnit[i]<-gsub(df2$singular[j],paste("-",df2$QUDT[j],"-",sep=""),df1$qudtUnit[i],)
  }
}

# get rid of multiple dashes - simplify to single dash
df1$qudtUnit<-gsub("----","-",df1$qudtUnit)
df1$qudtUnit<-gsub("---","-",df1$qudtUnit)
df1$qudtUnit<-gsub("--","-",df1$qudtUnit)
df1$qudtUnit<-gsub("-2","2",df1$qudtUnit)
df1$qudtUnit<-gsub("-3","3",df1$qudtUnit)
# get rid of trailing and leading dashes
df1$qudtUnit<-gsub("^-","",df1$qudtUnit,perl=T)
df1$qudtUnit<-gsub("-$","",df1$qudtUnit,perl=T)

# eliminate 2nd -PER- and replace with - 
for (i in 1:nrow(df1)){
  splt<-strsplit(df1$qudtUnit[[i]],"PER-")
  if (length(splt[[1]]) > 2){
    #print(length(splt[[1]]))
  out<-paste(splt[[1]][[1]],"PER-",sep="")
  for (j in 2:length(splt[[1]])){
    out<-paste(out,splt[[1]][[j]],sep="")
  }
  df1$qudtUnit[[i]]<-out
  }
  }
  
df1$qudtUnit<-gsub("\\([A-Z,a-z,\\-]*PER-[A-Z,a-z,\\-]+\\)-PER-\\([A-Z,a-z,\\-]+\\)",
                  paste("\\1","-","\\2",sep=""),df1$qudtUnit,perl=T)
df1$qudtUnit<-trimws(df1$qudtUnit)

# get rid of dashes after known prefixes
scaleList<-c("Pico","Nano","Micro","Milli","Kilo","Mega","Giga")
for (i in 1:nrow(df1)){
  for (j in 1:length(scaleList)){
  df1$qudtUnit[i]<-gsub(paste(scaleList[[j]],"-",sep=""),scaleList[[j]],df1$qudtUnit[i], ignore.case=T) 
}}


# see if the URL resolves
# download the latest QUDT list of units in Turtle format
#qudtUnitsText<-readLines("https://qudt.org/2.1/vocab/unit")
# use the march 2023 release
qudtUnitsText<-readLines("https://raw.githubusercontent.com/qudt/qudt-public-repo/v2.1.25/vocab/unit/VOCAB_QUDT-UNITS-ALL-v2.1.ttl")
qudtUnitList<-unlist(qudtUnitsText[grep("unit:",qudtUnitsText)])
qudtDf<-data.frame(qudtUnitList)
colnames(qudtDf)<-"qudtUnit"
qudtDf$qudtUnit<-trimws(gsub("unit:","",qudtDf$qudtUnit))
qudtDf$qudtUnitName<-qudtDf$qudtUnit

m1<-merge(df1,qudtDf,by="qudtUnit",all.x=TRUE)
# write out a URI
m1$qudtUri<-ifelse(is.na(m1$qudtUnitName),NA,paste("http://qudt.org/vocab/unit",m1$qudtUnit,sep='/'))
m1<-m1[order(m1$numOrgs,m1$TotalUses,m1$unit,decreasing=T),]

write.csv(m1[,c(1,3,19,2,13,14,17)],"UnitswQUDT.csv",row.names=F)

m1Qudt<-m1[!is.na(m1$qudtUri),]
print("")
print("All")
print(sum(m1Qudt$TotalUses))
print(sum(m1$TotalUses))
print(sum(m1Qudt$TotalUses)/sum(m1$TotalUses))
print("")
print("EDI")
print(sum(m1Qudt$ediTotalUses))
print(sum(m1$ediTotalUses))
print(sum(m1Qudt$ediTotalUses)/sum(m1$ediTotalUses))
print("")
print("NEON")
print(sum(m1Qudt$neonTotalUses))
print(sum(m1$neonTotalUses))
print(sum(m1Qudt$neonTotalUses)/sum(m1$neonTotalUses))
print("")
print("DataOne")
print(sum(m1Qudt$dataoneTotalUses))
print(sum(m1$dataoneTotalUses))
print(sum(m1Qudt$dataoneTotalUses)/sum(m1$dataoneTotalUses))