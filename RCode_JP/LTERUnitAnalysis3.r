# Create a column of pseudo-units that simplifies the form of units and sort by pseudo-unit
# 

# PRIOR PROGRAM
# IntegrateUnitAnalysis1.r 
#INPUTS 
# IntegratedUnitSummaryLowerCase.csv from IntegrateUnitAnalysis1.r
# SingularAbbrevsTable.xlsx - manually prepared file of substitutions
#OUTPUTS
# PseudoUnitsAll.csv  - all pseudounits
# PseudoUnitsTop.csv - only frequently observed pseudounits

# John Porter, August 2023

rm(list=ls())
setwd("C:/Users/John/Box Sync/EMLUnits")

df1<-read_csv("IntegratedUnitSummaryLowerCase.csv")
df1$pseudounit<-df1$unit

# convert spaces to _
df1$pseudounit<-gsub(" ","_",df1$pseudounit)
tail(df1$pseudounit)

# convert / to _per_
df1$pseudounit<-gsub("/","_per_",df1$pseudounit)

# read a data frame of standard units in their plural and singular forms
df2<-read_excel("SingularAbbrevsTable.xlsx",1)

# first search for abbreviations and change to full names
df3<-df2[!is.na(df2$other),]
df4<-df2[!is.na(df2$plural),]
for (i in 1:nrow(df1)){
  for (j in 1:nrow(df3)){
#    print(paste("_",df3$other[j],"_",sep=""))
    df1$pseudounit[i]<-gsub(paste("_",df3$other[j],"_",sep=""),paste("_",df3$singular[j],"_",sep=""),df1$pseudounit[i],perl=T)
    df1$pseudounit[i]<-gsub(paste("_",df3$other[j],"$",sep=""),paste("_",df3$singular[j],sep=""),df1$pseudounit[i],perl=T)
    df1$pseudounit[i]<-gsub(paste("^",df3$other[j],"_",sep=""),paste(df3$singular[j],"_",sep=""),df1$pseudounit[i],perl=T)
    df1$pseudounit[i]<-gsub(paste("^",df3$other[j],"\\W*$",sep=""),df3$singular[j],df1$pseudounit[i],perl=T)
  }
  for (j in 1:nrow(df4)){
    df1$pseudounit[i]<-gsub(df4$plural[j],df4$singular[j],df1$pseudounit[i])
  }
}
df1$pseudounit<-gsub("_","",df1$pseudounit)
ag1<-aggregate(TotalUses~pseudounit,df1,sum)
colnames(ag1)<-c("pseudounit","GroupTotalUses")
df1<-merge(df1,ag1)

df1<-df1[order(df1$GroupTotalUses,df1$pseudounit,df1$TotalUses,decreasing=T),]

df5<-df1[df1$numOrgs > 1,]
write.csv(df1[,c(1:2,15,2:14)],"PseudoUnitsAll.csv",row.names=F)
write.csv(df5[,c(1:2,15,2:14)],"PseudoUnitsTop.csv",row.names=F)

