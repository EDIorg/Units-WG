rm(list=ls())
setwd("C:/Users/John/Box Sync/EMLUnits")

library(readxl)
df1<-read_excel("IntegratedUnitSummaryMixedCase.xlsx",1)
df2<-read_excel("IntegratedUnitSummaryLowerCase.xlsx",1)

summary(as.factor(df1$orgList))
summary(as.factor(df2$orgList))
summary(as.factor(df1$numOrgs))

ag1<-aggregate(TotalUses~orgList,df1,mean)
print(ag1)
ag1<-aggregate(TotalUses~numOrgs,df1,sum)
print(ag1)
