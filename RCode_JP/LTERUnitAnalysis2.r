# Summarize raw and psuedounits
rm(list=ls())
#setwd("C:/Users/John/Box Sync/EMLUnits")
setwd("D:/Box Sync/EMLUnits")

#library(readxl)
#df1<-read_excel("IntegratedUnitSummaryMixedCase.xlsx",1)
#df2<-read_excel("IntegratedUnitSummaryLowerCase.xlsx",1)

df1<-read.csv("IntegratedUnitSummaryLowerCase.csv")
df2<-read.csv("IntegratedUnitSummaryMixedCase.csv")

summary(as.factor(df1$orgList))
summary(as.factor(df2$orgList))
summary(as.factor(df1$numOrgs))

ag1<-aggregate(TotalUses~orgList,df1,mean)
print(ag1)
ag1<-aggregate(TotalUses~numOrgs,df1,sum)
print(ag1)

# clear the decks for the pseudounit analysis
rm(list=ls())

df1<-read.csv("pseudoUnitsAll.csv")

#number of units
print(nrow(df1))

# number of units used more than 20 times
print(nrow(df1[df1$TotalUses >= 20,]))

# number of units used more than 100 times
print(nrow(df1[df1$TotalUses >= 100,]))

# number of uses by units used over 100 times
sum(df1[df1$TotalUses >= 100,]$TotalUses)

# Total uses of units used
sum(df1$TotalUses)

# Percent of all units represented by units used over 100 times
100*((sum(df1[df1$TotalUses >= 100,]$TotalUses))/(sum(df1$TotalUses)))


# get distinct pseudounits
df2<-df1[!duplicated(df1$pseudounit),]

#number of distinct pseudounits
print(nrow(df2))
# number of pseudounits used more than 20 times
print(nrow(df2[df2$GroupTotalUses >= 20,]))
# number of pseudounits used more than 100 times
print(nrow(df2[df2$GroupTotalUses >= 100,]))

# number of uses by pseudounits used over 100 times
sum(df2[df2$GroupTotalUses >= 100,]$GroupTotalUses)
# Percent of all units represented by units used over 100 times
100*((sum(df2[df2$GroupTotalUses >= 100,]$GroupTotalUses))/(sum(df1$TotalUses)))
