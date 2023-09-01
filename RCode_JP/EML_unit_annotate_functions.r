# Functions for creating <annotation> elements for units in Ecological Metadata Language Documents
# it requires an internet connection to work, since the unit list comes in from a remote source.
# John Porter, Sept. 2023

if(!require(xml2)){ install.packages("xml2") }  
library("xml2") 

removeAlternateIdentifierPastaDoi<-function(xmldata){
  # check to see if there is a PASTA doi in alternateIdentifier and delete it
  xmlAltIdNodeList<-xml_find_all(xmldata,"//alternateIdentifier")
  for (myAltIdNode in xmlAltIdNodeList){
    #print(xml_text(myAltIdNode))
    if (grepl("doi:10.6073/pasta",xml_text(myAltIdNode))){
      xml_remove(myAltIdNode)
    }
  }
  return(xmldata)
}
updateEMLRevision<-function(xmldata){
  # Update the Revision number of an EML document - increment it by 1
  xmlIdList<-xml_find_all(xmldata,"//eml:eml/@packageId")
  packageId<-as.character(xml_text(xmlIdList[1]))
  revTmp<-strsplit(packageId,'[.]')
  packageIdRevision<-as.numeric(revTmp[[1]][[length(revTmp[[1]])]])
  packageIdRevision<-packageIdRevision+1
  newPackageId<-revTmp[[1]][[1]]
  for (i in seq(2,length(revTmp[[1]])-1)){
    newPackageId<-paste(newPackageId,revTmp[[1]][[i]],sep=".")
  }
  newPackageId<-paste(newPackageId,packageIdRevision,sep=".")
  xml_attr(xmldata,"packageId")<-newPackageId
  
  # check to see if it is also in shortname
  xmlShortNameNode<-xml_find_first(xmldata,"//shortName")
  if (!is.na(xmlShortNameNode)){
    oldShortName<-xml_text(xmlShortNameNode)
    if (oldShortName == packageId){
      xml_text(xmlShortNameNode)<-newPackageId
    }
  }
  packageId<-newPackageId
  
  xmldata<-removeAlternateIdentifierPastaDoi(xmldata)
  return(xmldata)
}

addEMLAttributeId<-function(xmldata){
  # Check to see if there are "id" attributes for each dataset attribute
  # if it is missing, make up and id based on the attribute name and a sequential 
  # number starting with 1
  xmlTableList<-xml_find_all(xmldata,"//dataTable")
  attribNumber=1
  for (myDataTable in xmlTableList){
    xmlAttributeList<-xml_find_all(myDataTable,".//attribute")
    for (myAttribute in xmlAttributeList){
      myAttributeId<-xml_attrs(myAttribute,"id")
      if (length(myAttributeId)==0){
        # add required id if it is missing
        myAttributeName<-xml_text(xml_find_first(myAttribute,".//attributeName"))
        #print(myAttributeName)
        myAttributeName<-gsub("[^[:alnum:]]","",myAttributeName)
        #print(myAttributeName)
        myAttributeId<-paste0(myAttributeName,"-",attribNumber)
        attribNumber<-attribNumber+1
        xml_attr(myAttribute,"id")<-myAttributeId
      }
    }
  }
  return(xmldata)
  
}

annotateEMLUnits<-function(inEMLFile,incrementRevision=F,addAttributeIds=F){
  library(xml2)
  # ingest and EML file and add an <annotation> element to those <attribute>s whose units
  # can be resolved in QUDT. It returns a string containing the revised EML document. 
  #
  # inEMLFile is the name of the file or URL of the source EML document
  # incrementRevision controls whether the Revision number of the EML package is incremented by 1
  # addAttributeIds controls whether <attribute>s are checked for "id=" attributes and if absent
  # they are automatically added
  
  # read in QUDT and UCUM for raw lowercase units over the web or from a local file copy
  QUDTInfoDf<-read.csv("https://github.com/EDIorg/Units-WG/raw/main/RCode_JP/DataFiles4R/unitsWithQUDTInfo.csv")
  
  
  # set up a dataframe to be appended to from the first line of QUDTInfoDf
  QUDTOutDf<-QUDTInfoDf[1,]
  QUDTOutDf$packageId<-NA
  QUDTOutDf$attributeId<-NA
  attribNumber<-1
  
  xmldata<-read_xml(inEMLFile)
  xmlIdList<-xml_find_all(xmldata,"//eml:eml/@packageId")
  packageId<-as.character(xml_text(xmlIdList[1]))
  if (incrementRevision){
    xmldata<-updateEMLRevision(xmldata)
  }
  
  if (addAttributeIds){
    addEMLAttributeId(xmldata)
  }
  
  xmlTableList<-xml_find_all(xmldata,"//dataTable")
  for (myDataTable in xmlTableList){
    xmlAttributeList<-xml_find_all(myDataTable,".//attribute")
    for (myAttribute in xmlAttributeList){
      myAttributeId<-xml_attrs(myAttribute,"id")
      myUnit<-xml_find_first(myAttribute,".//unit")
      myUnitText<-xml_text(myUnit)[[1]][[1]]
      
      myQUDTInfoDf<-QUDTInfoDf[grepl(paste("^",myUnitText,"$",sep=""),trimws(QUDTInfoDf$unit),ignore.case=T),]
      myQUDTInfoDf<-myQUDTInfoDf[!duplicated(myQUDTInfoDf$qudtUri)
                                 & !is.na(myQUDTInfoDf$qudtUri) 
                                 & !is.na(myQUDTInfoDf$unit)
                                 & myQUDTInfoDf$unit != "NA",]
      if (nrow(myQUDTInfoDf) == 1){
        #   print(myAttributeId)
        #   print(myUnitText)
        myQUDTInfoDf$packageId<-packageId
        myQUDTInfoDf$attributeId<-myAttributeId
        # print(myQUDTInfoDf$qudtUri)
        QUDTOutDf<-rbind(QUDTOutDf,myQUDTInfoDf)
        
        # Add annotation node to attribute
        xml_add_child(myAttribute,read_xml(charToRaw(paste('<annotation>
        <propertyURI label="has unit">http://qudt.org/schema/qudt/hasUnit</propertyURI>',
                                                           paste('  <valueURI label="',trimws(myQUDTInfoDf$qudtLabel),'">',myQUDTInfoDf$qudtUri,'</valueURI>',sep=""),
                                                           "</annotation>",sep="\n"))))
      } # end if unit found
    } # end for attributes
  }# end for dataTables
  
  # write it out to disk
  #write_xml(xmldata,paste(packageId,"_annotated.xml"))
  
  return(as.character(xmldata))
} 

createUnitsAnnotationsListString<-function(inEMLFile,indent=0){
  # create an <annotations> element including multiple <annotation> elements
  # and return it as a string
  # inEMLFile is the name of a file or URL that is an EML document
  # indent is an optional number of spaces to indent each line in the <annotations>
  indentStr<-""
  if(indent > 0){
    for(i in 1:indent){
      print(i)
      indentStr<-paste0(indentStr," ")
    }}
  
  # read in QUDT and UCUM for raw lowercase units over the web or from a local file copy
  QUDTInfoDf<-read.csv("https://github.com/EDIorg/Units-WG/raw/main/RCode_JP/DataFiles4R/unitsWithQUDTInfo.csv")
  
  
  # set up a dataframe to be appended to from the first line of QUDTInfoDf
  QUDTOutDf<-QUDTInfoDf[1,]
  QUDTOutDf$packageId<-NA
  QUDTOutDf$attributeId<-NA
  
  xmldata<-read_xml(inEMLFile)
  xmlIdList<-xml_find_all(xmldata,"//eml:eml/@packageId")
  packageId<-as.character(xml_text(xmlIdList[1]))
  
  # if (incrementRevision){
  #   xmldata<-updateEMLRevision(xmldata)
  # }
  # 
  # if (addAttributeIds){
  #   addEMLAttributeId(xmldata)
  # }
  
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
      
      myQUDTInfoDf<-QUDTInfoDf[grepl(paste("^",myUnitText,"$",sep=""),trimws(QUDTInfoDf$unit),ignore.case=T),]
      myQUDTInfoDf<-myQUDTInfoDf[!duplicated(myQUDTInfoDf$qudtUri)
                                 & !is.na(myQUDTInfoDf$qudtUri) 
                                 & !is.na(myQUDTInfoDf$unit)
                                 & myQUDTInfoDf$unit != "NA",]
      if (nrow(myQUDTInfoDf) == 1){
        print(myAttributeId)
        print(myUnitText)
        myQUDTInfoDf$packageId<-packageId
        myQUDTInfoDf$attributeId<-myAttributeId
        print(myQUDTInfoDf$qudtUri)
        QUDTOutDf<-rbind(QUDTOutDf,myQUDTInfoDf)
        
      } # end if unit found
    } # end for attributes
  }# end for dataTables
  
  
  # get rid of top line that has missing attribute info or, if no attribute IDs are supplied, all lines
  QUDTOutDf<-QUDTOutDf[!is.na(QUDTOutDf$attributeId),]
  # UCUMOutDf<-UCUMOutDf[!is.na(UCUMOutDf$attributeId),]
  
  # Now write out the annotations
  if(nrow(QUDTOutDf) > 0){
    print(length(QUDTOutDf))
    annoListString<-(paste("<!-- Unit Annotations for package",packageId,"-->\n",sep=" "))
    annoListString<-paste0(annoListString,indentStr,"<annotations>\n")   
    for (i in 1:nrow(QUDTOutDf)){
      annoListString<-paste(annoListString,indentStr,'  <annotation references="',QUDTOutDf[i,]$attributeId,'">\n',sep="")
      annoListString<-paste(annoListString,indentStr,'    <propertyURI label="has unit">http://qudt.org/schema/qudt/hasUnit</propertyURI>\n',sep='')
      annoListString<-paste(annoListString,indentStr,'    <valueURI label="',trimws(QUDTOutDf[i,]$qudtLabel),'">',QUDTOutDf[i,]$qudtUri,'</valueURI>\n',sep="")
      annoListString<-paste0(annoListString,indentStr,"  </annotation>\n")
    }
    annoListString<-paste0(annoListString,indentStr,"</annotations>\n")
    
  }  # end if annotation id exists
  return(annoListString)
}
