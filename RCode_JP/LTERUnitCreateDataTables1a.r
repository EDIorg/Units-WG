# Create data tables link lower-cased raw units with QUDT units
# and QUDT labels.   Then create a separate table doing the same for UCUM 
# units derived from QUDT.
# John Porter, March 2023

# Note, run LTERUnitQUDT_UCUM_Codes1.r FIRST to generate needed tables


rm(list=ls())
#setwd("D:/Box Sync/EMLUnits")
setwd("C:/users/john/Box Sync/EMLUnits")

# Read in the raw lower-case units with psuedounits
rawDf<-read.csv("PseudoUnitsAll.csv")
# Strip off columns we don't need
rawDf<-rawDf[,c("unit","pseudounit","orgList","numOrgs","TotalUses")]

# read in the psuedunits with QUDT
qudtUriDf<-read.csv("UnitswQUDT.csv")
# Strip off columns we don't need
qudtUriDf<-qudtUriDf[,c("pseudounit","qudtUnit","qudtUri")]

# merge them based on pseudounits
rawQudtDf<-merge(rawDf,qudtUriDf,by="pseudounit",all.x=T)

write.csv(rawQudtDf[order(rawQudtDf$TotalUses,rawQudtDf$unit, 
                          decreasing=T),
          c("unit","qudtUri","TotalUses","numOrgs","orgList","pseudounit","qudtUnit")],
          "allUnitsWithOrWithoutQUDTunits.csv", row.names=F)

# eliminate units where qudtUri is empty
qudtDf<-rawQudtDf[!is.na(rawQudtDf$qudtUri),]

# read in all pseudounits as if they were units and eliminate duplicates.  Note totals set to 0
# so as not to inflate counts
pseudoQUDTDf<-qudtDf[qudtDf$unit != qudtDf$pseudounit,]
pseudoQUDTDf$unit<-pseudoQUDTDf$pseudounit
pseudoQUDTDf$numOrgs<-0
pseudoQUDTDf$TotalUses<-0
pseudoQUDTDf$orgList<-""

qudtDf<-rbind(qudtDf,pseudoQUDTDf)


# Add in ALL Multi-Letter UCUM codes from QUDT along with their qudtUri
ucumCodesDf<-read.csv("UCUMCodes.csv")
# get rid of one-letter codes - they are too ambiguous in some situations (e.g., d for Day)
ucumCodesDf<-ucumCodesDf[nchar(ucumCodesDf$ucumCode) > 1,]
ucumCodesDf$unit<-ucumCodesDf$ucumCode
ucumCodesDf$pseudounit<-""
ucumCodesDf$orgList<-""
ucumCodesDf$numOrgs<-0
ucumCodesDf$TotalUses<-0
ucumCodesDf$qudtUnit<-gsub("http://qudt.org/vocab/unit/","",ucumCodesDf$qudtUri)
# get rid of those with no Unit (UCUM code) and reorder and select columns to match qudtDf
ucumCodesDf<-ucumCodesDf[!is.na(ucumCodesDf$unit),c("pseudounit","unit","orgList","numOrgs","TotalUses","qudtUnit","qudtUri")]

# append the two dataframes
qudtDf<-rbind(qudtDf,ucumCodesDf)



# read in labels to add
inQudtLabels<-read.csv("qudtLabels.csv")
# add them
qudtDf<-merge(qudtDf,inQudtLabels,by="qudtUri")


# Add in ALL QUDT units regardless of whether they were ever used by us
qudtCodesDf<-read.csv("qudtLabels.csv")
qudtCodesDf$unit<-gsub("http://qudt.org/vocab/unit/","",qudtCodesDf$qudtUri)
qudtCodesDf$pseudounit<-""
qudtCodesDf$orgList<-""
qudtCodesDf$numOrgs<-0
qudtCodesDf$TotalUses<-0
qudtCodesDf$qudtUnit<-gsub("http://qudt.org/vocab/unit/","",qudtCodesDf$qudtUri)
# get rid of unit with http:// in units (not qudt code)
qudtCodesDf<-qudtCodesDf[!grepl("http",qudtCodesDf$unit),]

qudtDf<-rbind(qudtDf,qudtCodesDf)

# eliminate any duplicates 
qudtDf<-qudtDf[!duplicated(qudtDf),]

# reorder the columns
qudtDf<-qudtDf[order(qudtDf$TotalUses,qudtDf$unit,decreasing = T),
               c("unit","qudtUnit","qudtLabel","qudtUri",
                 "qudtDimensionVector","qudtConversionMultiplier","nonLatexDescription","latexDescription","TotalUses","numOrgs","orgList")]


# write it out
write.csv(qudtDf,"unitsWithQUDTInfo.csv",row.names=F)


