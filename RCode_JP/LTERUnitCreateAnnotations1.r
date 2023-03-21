# Extract units from EML Metadata and add annotation elements to the document
# It produces an edited EML file with annotations associated with each individual attribute and
# a text file containing an EML <annotations> block that could optionally be added to the original document. 
# John Porter, March 2023

# Important note - the program does not yet currently check for the EML version which MUST BE > 2.2.0 or greater
# annotations are not allowed in EML 2.1 and below.  

rm(list=ls())
# Optionally, set a directory where the output will be written
# userName<-Sys.getenv("USERNAME")
# setwd(paste("C:/users",userName,"Downloads",sep='/'))

# Set the EML Scope, Identifier and Revision to the package you want to process
emlScope<-'knb-lter-vcr'
emlIdentifier<-338
emlRevision<-2

######### You should not need to change anything below this line ###############################

inXMLFile<-paste('https://pasta.lternet.edu/package/metadata/eml',emlScope,emlIdentifier,emlRevision,sep='/')


library(tidyverse)
library(xml2)

# read in QUDT and UCUM for raw lowercase units over the web or from a local file copy
QUDTInfoDf<-read.csv("https://www.vcrlter.virginia.edu/data/unitsWithQUDTInfo.csv")


# set up a dataframe to be appended to from the first line of QUDTInfoDf
QUDTOutDf<-QUDTInfoDf[1,]
QUDTOutDf$packageId<-NA
QUDTOutDf$attributeId<-NA
  
xmldata<-read_xml(inXMLFile)
xmlIdList<-xml_find_all(xmldata,"//eml:eml/@packageId")
packageId<-as.character(xml_text(xmlIdList[1]))

xmlTableList<-xml_find_all(xmldata,"//dataTable")
for (myDataTable in xmlTableList){
  xmlAttributeList<-xml_find_all(myDataTable,".//attribute")
  for (myAttribute in xmlAttributeList){
    myAttributeId<-xml_attrs(myAttribute,"id")
    if (length(myAttributeId)==0){
      myAttributeId=NA
    }
    myUnit<-xml_find_first(myAttribute,".//unit")
    myUnitText<-xml_text(myUnit)[[1]][[1]]
    #myCustomUnit<-xml_find_first(myUnit,".//customUnit")
    # myStandardUnit<-xml_find_first(myAttribute,"//standardUnit")
    print(myAttributeId)
    print(myUnitText)
    myQUDTInfoDf<-QUDTInfoDf[grepl(paste("^",myUnitText,"$",sep=""),trimws(QUDTInfoDf$unit),ignore.case=T),]
    myQUDTInfoDf<-myQUDTInfoDf[!duplicated(myQUDTInfoDf$qudtUri)& !is.na(myQUDTInfoDf$qudtUri),]
    if (nrow(myQUDTInfoDf) == 1){
    myQUDTInfoDf$packageId<-packageId
    myQUDTInfoDf$attributeId<-myAttributeId
    print(myQUDTInfoDf$qudtUri)
    QUDTOutDf<-rbind(QUDTOutDf,myQUDTInfoDf)
    
    # Add annotation node to attribute
    xml_add_child(myAttribute,read_xml(charToRaw(paste('<annotation>
        <propertyURI label="has unit">http://semanticscience.org/resource/SIO000221</propertyURI>',
   paste('  <valueURI label="',trimws(myQUDTInfoDf$qudtLabel),'">',myQUDTInfoDf$qudtUri,'</valueURI>',sep=""),
   "</annotation>",sep="\n"))))
    } # end if unit found
  } # end for attributes
}# end for dataTables

# write it out to disk
write_xml(xmldata,paste(packageId,"_annotated.xml"))

# get rid of top line that has missing attribute info or, if no attribute IDs are supplied, all lines
QUDTOutDf<-QUDTOutDf[!is.na(QUDTOutDf$attributeId),]
# UCUMOutDf<-UCUMOutDf[!is.na(UCUMOutDf$attributeId),]

# Now write out the annotations
if(nrow(QUDTOutDf) > 0){
  print(length(QUDTOutDf))
myOutfileName<-paste(packageId,"UnitAnnotations.txt",sep="_")
sink(myOutfileName)
cat(paste("<!-- Unit Annotations for package",packageId,"-->\n",sep=" "))
cat("<annotations>\n")    
for (i in 1:nrow(QUDTOutDf)){
  cat(paste('<annotation references="',QUDTOutDf[i,]$attributeId,'">\n',sep=""))
  cat('  <propertyURI label="has unit">http://semanticscience.org/resource/SIO000221</propertyURI>\n',sep='')
  cat(paste('  <valueURI label="',trimws(QUDTOutDf[i,]$qudtLabel),'">',QUDTOutDf[i,]$qudtUri,'</valueURI>\n',sep=""))
  cat("</annotation>\n")
}
cat("</annotations>")
sink()
}  # end if annotation id exists
cat("Working Directory was: ")
print(getwd())
if(nrow(QUDTOutDf) > 0){print(paste("<annotations> output Written to: ",myOutfileName))}
print(paste("Modified EML written to: ",packageId,"_annotated.xml",sep=''))




