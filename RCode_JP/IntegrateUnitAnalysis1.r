# Integrate EDI, NEON and DataONE unit lists, with and without case differences
rm(list=ls())
userName<-Sys.getenv("USERNAME")
setwd(paste("C:/users",userName,"box sync/EMLUnits",sep='/'))
#setwd("D:/box sync/EMLUnits")

library(tidyverse)
library(xlsx)

ediDf1<-read.xlsx("UnitUseSummary.xlsx",1)
colnames(ediDf1)<-c("unit","ediScopeUses","ediPackageUses","ediTotalUses")
neonDf1<-read.xlsx("UnitUseSummaryNEON.xlsx",1)
colnames(neonDf1)<-c("unit","neonScopeUses","neonPackageUses","neonTotalUses")
dataoneDf1<-read.xlsx("dataone-units.xlsx",1)
dataoneDf1<-dataoneDf1[,c(2,3)]
colnames(dataoneDf1)<-c("unit","dataoneTotalUses")

m1<-merge(ediDf1,neonDf1,all=T)
m2<-merge(m1,dataoneDf1,all=T)
# set NAs to 0
m2[is.na(m2)]<-0

# subtract EDI totals from DataOne  - doesn't work - get NEGATIVE DataONE values
#m2$dataoneTotalUses<-m2$dataoneTotalUses-m2$ediTotalUses

m2$orgList<-ifelse(m2$ediTotalUses >0,"EDI","")
m2$orgList<-ifelse(m2$neonTotalUses >0,trimws(paste(m2$orgList,"NEON",sep=" ")),m2$orgList)
m2$orgList<-ifelse(m2$dataoneTotalUses >0,trimws(paste(m2$orgList,"DataOne",sep=" ")),m2$orgList)
m2$numOrgs<-str_count(m2$orgList,"\\w+") 

m2$totalScopes<-m2$ediScopeUses+m2$neonScopeUses
m2$totalPackages<-m2$ediPackageUses+m2$neonPackageUses
m2$TotalUses<-m2$ediTotalUses+m2$neonTotalUses+m2$dataoneTotalUses
m2<-m2[order(m2$numOrgs,m2$TotalUses,m2$totalPackages,decreasing=T),]

write.csv(m2,"IntegratedUnitSummaryMixedCase.csv",row.names=F)

# Now do the same thing but convert all units to lower case and recalculate sums
m2$unit<-tolower(m2$unit)
ag1<-aggregate(cbind(ediScopeUses,ediPackageUses,ediTotalUses,neonScopeUses,neonPackageUses,neonTotalUses,dataoneTotalUses)~unit,m2,sum)
ag1$orgList<-ifelse(ag1$ediTotalUses >0,"EDI","")
ag1$orgList<-ifelse(ag1$neonTotalUses >0,trimws(paste(ag1$orgList,"NEON",sep=" ")),ag1$orgList)
ag1$orgList<-ifelse(ag1$dataoneTotalUses >0,trimws(paste(ag1$orgList,"DataOne",sep=" ")),ag1$orgList)
ag1$numOrgs<-str_count(ag1$orgList,"\\w+") 

ag1$totalScopes<-ag1$ediScopeUses+ag1$neonScopeUses
ag1$totalPackages<-ag1$ediPackageUses+ag1$neonPackageUses
ag1$TotalUses<-ag1$ediTotalUses+ag1$neonTotalUses+ag1$dataoneTotalUses
ag1<-ag1[order(ag1$numOrgs,ag1$TotalUses,ag1$totalPackages,decreasing=T),]

write.csv(m2,"IntegratedUnitSummaryLowerCase.csv",row.names=F)