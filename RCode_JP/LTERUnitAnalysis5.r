rm(list=ls())
setwd("C:/Users/John/Box Sync/EMLUnits")
library(readxl)

df1<-read.csv("PseudoUnitsAll.csv")
df1a<-df1[,c(1:2)]

df2<-read.csv("UnitswQUDT.csv")
df2a<-df2[,c("pseudounit","qudtUnit","qudtUri")]


mg1<-merge(df1a,df2a,by="pseudounit",all.x=T)
mg1<-mg1[!duplicated(mg1),]

qudtOnlyDf<-mg1[!is.na(mg1$qudtUri),]

#write.csv(qudtOnlyDf,"qudtOnlyOrigUnits.csv",row.names=F)

# NTL units
ntlDf1<-read_excel("NTL_units_only_edited.xlsx",sheet=1)
ntlDf1$unitLowerCase<-tolower(ntlDf1$unit_text)

ntlDf2<-ntlDf1[,c(9,3,5)]
colnames(ntlDf2)<-c("unitLowerCase","ntl_Unit","ntl_QUDT")

Mg2<-merge(ntlDf2,qudtOnlyDf,by.x="unitLowerCase",by.y="unit",all=T)

# CAP units
capDf1<-read_excel("cap_ntl_units_joined.xlsx",sheet=1)
capDf1$unitLowerCase<-tolower(capDf1$unit_text)

capDf2<-capDf1[,c(7,2,3)]
colnames(capDf2)<-c("unitLowerCase","cap_Unit","cap_QUDT")
capDf2$cap_QUDT<-ifelse(capDf2$cap_QUDT=="NA",NA,capDf2$cap_QUDT)

Mg2<-merge(capDf2,Mg2,by.x="unitLowerCase",by.y="unitLowerCase",all=T)

# SBC units
sbcDf1<-read_excel("sbc_UnitDictionary_2023_wQUDT_reformatted.xlsx",sheet=1)
sbcDf1$unitLowerCase<-tolower(sbcDf1$unit_text)

sbcDf2<-sbcDf1[,c(16,2,4)]
colnames(sbcDf2)<-c("unitLowerCase","sbc_Unit","sbc_QUDT")
sbcDf2$sbc_QUDT<-ifelse(is.na(sbcDf2$sbc_QUDT),NA,paste("https://qudt.org/vocab/unit/",sbcDf2$sbc_QUDT,sep=""))


Mg2<-merge(sbcDf2,Mg2,by.x="unitLowerCase",by.y="unitLowerCase",all=T)

# MCR Units
mcrDf1<-read_excel("MCRUnits.xlsx",sheet=1)
mcrDf1$unitLowerCase<-tolower(mcrDf1$unit_text)

mcrDf2<-mcrDf1[,c(16,2,4)]
colnames(mcrDf2)<-c("unitLowerCase","mcr_Unit","mcr_QUDT")
mcrDf2$mcr_QUDT<-ifelse(is.na(mcrDf2$mcr_QUDT),NA,paste("https://qudt.org/vocab/unit/",mcrDf2$mcr_QUDT,sep=""))

Mg2<-merge(mcrDf2,Mg2,by.x="unitLowerCase",by.y="unitLowerCase",all=T)

# NEON Units
neonDf1<-read_excel("neon_UnitUseSummaryNEON.xlsx",sheet=1)
neonDf1$unitLowerCase<-tolower(neonDf1$unit)

neonDf2<-neonDf1[,c(10,1,2)]
colnames(neonDf2)<-c("unitLowerCase","neon_Unit","neon_QUDT")

Mg2<-merge(neonDf2,Mg2,by.x="unitLowerCase",by.y="unitLowerCase",all=T)



Mg2$neonHasQUDT<-ifelse(is.na(Mg2$neon_QUDT),0,1)
Mg2$mcrHasQUDT<-ifelse(is.na(Mg2$mcr_QUDT),0,1)
Mg2$sbcHasQUDT<-ifelse(is.na(Mg2$sbc_QUDT),0,1)
Mg2$capHasQUDT<-ifelse(is.na(Mg2$cap_QUDT),0,1)
Mg2$ntlHasQUDT<-ifelse(is.na(Mg2$ntl_QUDT),0,1)
Mg2$autoHasQUDT<-ifelse(is.na(Mg2$qudtUri),0,1)

Mg2$TotalQUDT<-rowSums(Mg2[,c(15:20)])


# select only cases where the unit is used by one or more sites
Mg2$neonHasUnit<-ifelse(is.na(Mg2$neon_Unit),0,1)
Mg2$mcrHasUnit<-ifelse(is.na(Mg2$mcr_Unit),0,1)
Mg2$sbcHasUnit<-ifelse(is.na(Mg2$sbc_Unit),0,1)
Mg2$capHasUnit<-ifelse(is.na(Mg2$cap_Unit),0,1)
Mg2$ntlHasUnit<-ifelse(is.na(Mg2$ntl_Unit),0,1)
Mg2$autoHasUnit<-ifelse(is.na(Mg2$pseudounit),0,1)
Mg2$TotalSiteUnits<-rowSums(Mg2[,c(22:26)])

# Get rid of units not identified by these sites and any duplicates
Mg2<-Mg2[Mg2$TotalSiteUnits > 0,]
Mg2<-Mg2[!duplicated(Mg2),]


Mg2<-Mg2[order(Mg2$TotalSiteUnits,Mg2$TotalQUDT ,Mg2$unitLowerCase),]

# Write out some stats
sum(Mg2$neonHasQUDT)
sum(Mg2$mcrHasQUDT)
sum(Mg2$sbcHasQUDT)
sum(Mg2$capHasQUDT)
sum(Mg2$ntlHasQUDT)
sum(Mg2$autoHasQUDT)

notInAutoDf<-Mg2[Mg2$autoHasQUDT==0 & Mg2$TotalQUDT > 0,]

write.csv(Mg2[,c(1:14,21)],"Units_vs_QUDT_Summary.csv",row.names=F)
