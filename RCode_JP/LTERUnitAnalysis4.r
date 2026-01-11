# Try to figure out QUDT codes for units list based on pseudounits

# PRIOR PROGRAM
# LTERUnitAnalysis3.r
#INPUTS 
# PseudoUnitsAll.csv from LTERUnitAnalysis3.r
# SingularAbbrevsTableWQUST.xlsx - manually prepared
#OUTPUTS
# UnitswQUDT.csv

# John Porter, August 2023
# Added permutations of unit elements to generate alternative QUDT units,

rm(list=ls())
setwd("K:/Box Sync/EMLUnits")
#setwd("C:/users/john/Box Sync/EMLUnits")
#setwd("D:/users/jhp7e/Box Sync/EMLUnits")
#setwd("C:/users/jhp7e/Box Sync/EMLUnits")
library(readxl)
library(gtools)

df1a<-read.csv("PseudoUnitsAll.csv")
df1a<-df1a[order(df1a$pseudounit,df1a$TotalUses,decreasing=T),]
# summarize them by pseudounit
ag0<-aggregate(unit~pseudounit,df1a,head,1)
ag1<-aggregate(TotalUses~pseudounit,df1a,sum)
ag2<-aggregate(ediTotalUses~pseudounit,df1a,sum)
ag3<-aggregate(neonTotalUses~pseudounit,df1a,sum)
ag4<-aggregate(dataoneTotalUses~pseudounit,df1a,sum)
ag1<-merge(ag0,ag1)
df1<-merge(ag1,ag2)
df1<-merge(df1,ag3)
df1<-merge(df1,ag4)
##df1<-df1a[!duplicated(df1a$pseudounit),]
df2<-read_excel("SingularAbbrevsTablewQUDT.xlsx",sheet=1)

df1$qudtUnit<-df1$pseudounit

for (i in 1:nrow(df1)){
  for (j in 1:nrow(df2)){
    #    print(paste("_",df3$other[j],"_",sep=""))
    df1$qudtUnit[i]<-gsub(df2$singular[j],paste("-",df2$QUDT[j],"-",sep=""),df1$qudtUnit[i],)
  }
}

# remove parenthetical parts
df1$qudtUnit<-gsub("\\(.*\\)","",df1$qudtUnit,perl=T)
# Get rid of trailing-NA
df1$qudtUnit<-gsub("-NA-$","",df1$qudtUnit,perl=T)
df1$qudtUnit<-gsub("-NA$","",df1$qudtUnit,perl=T)
# get rid of multiple dashes - simplify to single dash
df1$qudtUnit<-gsub("-----","-",df1$qudtUnit)
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
scaleList<-c("Pico","Nano","Micro","Milli","Centi","Deci","Deca","Hecto","Kilo","Mega","Giga","Tera")
for (i in 1:nrow(df1)){
  for (j in 1:length(scaleList)){
  df1$qudtUnit[i]<-gsub(paste(scaleList[[j]],"-",sep=""),scaleList[[j]],df1$qudtUnit[i], ignore.case=F) 
}}

# create a list of alternative QUDT units that reorder the elements in the originally generated unit string
# e.g., GM-PER-SEC-M3 to GM-PER-SEC-M3 permuting both left and right sides of PER
generateAltQUDT<-function(origUnit){
  # note Github Copilot helped to write this function
  altUnitList<-c()
  # don't work with 0 length or ones that are too long
  if (nchar(origUnit)==0 | nchar(origUnit) > 40){
    return(altUnitList)
  }
  splt<-strsplit(origUnit,"-PER-")
  # count the number of elements in each part of splt
  if (length(splt[[1]]) == 2){
    leftPart<-strsplit(splt[[1]][1],"-")[[1]]
    leftPart<-unique(leftPart)
    rightPart<-strsplit(splt[[1]][2],"-")[[1]]
    rightPart<-unique(rightPart)
    # don't generate permutations if there are too many elements
    if ((length(leftPart) + length(rightPart)) > 10){
      return(altUnitList)
    }
    # generate all permutations of rightPart
    rightPartPerms<-permutations(n=length(rightPart),r=length(rightPart),v=rightPart)
    leftPartPerms<-permutations(n=length(leftPart),r=length(leftPart),v=leftPart)
    for (j in 1:nrow(leftPartPerms)){
      for (i in 1:nrow(rightPartPerms)){
        altUnit<-paste(paste(leftPartPerms[j,],collapse='-'),"-PER-",paste(rightPartPerms[i,],collapse='-'),sep="")
        if (altUnit != origUnit){
          altUnitList<-c(altUnitList,altUnit)
        }
      }
    }
  } else {
    leftPart<-strsplit(splt[[1]][1],"-")[[1]]
    leftPart<-unique(leftPart)
    if (length(leftPart)>0 & length(leftPart < 10)){
    if (leftPart[1]!=""){
    # we don't want to permute PER as part of the unit so remove it from the permutation 
    # and just add it back later
      isPERstart<-leftPart[1]=="PER"
     if (isPERstart) {
        leftPart<-leftPart[-1]
     }
     if (length(leftPart)==0){
       return(altUnitList)
     }
#     print(paste("Length of leftPart is ",length(leftPart),sep=""))
     leftPartPerms<-permutations(n=length(leftPart),r=length(leftPart),v=leftPart)
     for (i in 1:nrow(leftPartPerms)){
       altUnit<-paste(leftPartPerms[i,],collapse='-')
        if (isPERstart){
          altUnit<-paste("PER-",altUnit,sep="")
        }
        if (altUnit != origUnit){
          altUnitList<-c(altUnitList,altUnit)
        }
     }
    }}
  }
 if (length(altUnitList)>0){print(paste(origUnit,": ",length(altUnitList)," alternative QUDT units generated",sep=""))}
  return(altUnitList)
}

# Loop through df1 and add lines for alternative QUDT units  written by Github Copilot
for (k in 1:nrow(df1)){
  altQUDTList<-generateAltQUDT(df1$qudtUnit[k])
  if (length(altQUDTList) > 0){
    for (m in 1:length(altQUDTList)){
      newRow<-df1[k,]
      newRow$qudtUnit<-altQUDTList[m]
      df1<-rbind(df1,newRow)
    }
  }
}

# see if the URL resolves
# download the latest QUDT list of units in Turtle format
#qudtUnitsText<-readLines("https://qudt.org/2.1/vocab/unit")
# use the march 2023 release
#qudtUnitsText<-readLines("https://raw.githubusercontent.com/qudt/qudt-public-repo/v2.1.25/vocab/unit/VOCAB_QUDT-UNITS-ALL-v2.1.ttl")
# use the current release (3.1.9 as of Dec. 2025)
qudtUnitsText<-readLines("https://qudt.org/vocab/unit")
qudtUnitList<-unlist(qudtUnitsText[grep("unit:",qudtUnitsText)])
qudtDf<-data.frame(qudtUnitList)
colnames(qudtDf)<-"qudtUnit"
qudtDf$qudtUnit<-trimws(gsub("unit:","",qudtDf$qudtUnit))
qudtDf$qudtUnitName<-qudtDf$qudtUnit

m1<-merge(df1,qudtDf,by="qudtUnit",all.x=TRUE)
# write out a URI
m1$qudtUri<-ifelse(is.na(m1$qudtUnitName),NA,paste("http://qudt.org/vocab/unit",m1$qudtUnit,sep='/'))


# split into matching and non-matching sets, then eliminate duplicates based on unit within each set, 
# then recombine and eliminate duplicates based on unit, keeping those that match
m1Qudt<-m1[!is.na(m1$qudtUri),]
m1NoQudt<-m1[is.na(m1$qudtUri),]
m1Qudt<-m1Qudt[!duplicated(m1Qudt$unit),]
m1NoQudt<-m1NoQudt[!duplicated(m1NoQudt$unit),]
m1<-rbind(m1Qudt,m1NoQudt)
m1<-m1[!duplicated(m1$unit) | !is.na(m1$qudtUri),]
m1<-m1[order(m1$TotalUses,decreasing=T),]

# eliminate duplicate rows introduced by alternative QUDT units based on the raw units when both match
m1Qudt<-m1Qudt[!duplicated(m1Qudt$unit),]
write.csv(m1[,c(1,3,9,2,4)],"UnitswQUDT.csv",row.names=F)

print("")
print("***** Distinct PseudoUnits and QUDT units ****")
print("")
print("Number of Distinct Pseudounits matched")
print(nrow(m1Qudt[!duplicated(m1Qudt$pseudounit),]))
print("Number of Distinct QUDT units matched")
print(nrow(m1Qudt[!duplicated(m1Qudt$qudtUnit),]))

print("")
print("***** Uses of Units in Metadata ****")
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

