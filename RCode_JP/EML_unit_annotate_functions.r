# Functions for creating <annotation> elements for units in Ecological Metadata Language Documents
# it requires an internet connection to work, since the unit list comes in from a remote source.
# John Porter, Aug. 2024

if(!require(xml2)){ install.packages("xml2") }  
library("xml2") 
if(!require(stringi)){ install.packages("stringi") }  
library("stringi") 
if(!require(magrittr)){ install.packages("magrittr") }  
library("magrittr") 

convertSpecialCharacters<-function(inString){
  # Convert <U+2019> and other special characters to Unicode. 
  # From: https://stackoverflow.com/questions/39847816/convert-utf8-code-point-strings-like-u0161-to-utf8
  outString<- inString %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{4})>", "\\\\u$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{5})>", "\\\\U000$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{6})>", "\\\\U00$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{7})>", "\\\\U0$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{8})>", "\\\\U$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{1})>", "\\\\u000$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{2})>", "\\\\u00$1") %>% 
    stri_replace_all_regex("<U\\+([[:alnum:]]{3})>", "\\\\u0$1") %>% 
    stri_unescape_unicode() %>% 
    stri_enc_toutf8()
#    outString<-iconv(outString,from="UTF-8",to="ASCII//TRANSLIT")
  # this conversion and conversion back should help with some odd latin characters
 #   outString<-iconv(outString,from="UTF-8",to="latin1")
 #   outString<-iconv(outString,from="latin1",to="UTF-8")
  return(outString)
}

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

getEMLVersionList<-function(xmldata){
  # returns EML version numbers as a list - e.g., eml-2.1.5 is EMLVersionList[1}=2, [2]=1, [3]=5]
  #print(xml_attrs(xmldata))
  EMLVersionString<-xml_attr(xmldata,"xmlns:eml")
  #print(EMLVersionString)
  EMLVersionString<-sub("eml://ecoinformatics.org/eml-","",EMLVersionString)
  # deal with some variants
  EMLVersionString<-sub("http://ecoinformatics.org/eml-","",EMLVersionString)
  EMLVersionString<-sub("https://ecoinformatics.org/eml-","",EMLVersionString)
  EMLVersionString<-sub("eml://eml.ecoinformatics.org/eml-","",EMLVersionString)
  EMLVersionString<-sub("http://eml.ecoinformatics.org/eml-","",EMLVersionString)
  EMLVersionString<-sub("https://eml.ecoinformatics.org/eml-","",EMLVersionString)
  #print(EMLVersionString)
  EMLVersionList<-strsplit(EMLVersionString,".",fixed=T)
  # print(EMLVersionList)
  return(EMLVersionList)
}

updateEMLVersion<-function(xmldata){
  # EML 2.2.0 and after is required. If document is EML 2.0 or 2.1, update the version to EML-2.2.0
  # earlier versions are forward compatible so it they should still work.
  emlNode<-xml_find_first(xmldata,"//eml:eml")
  EMLVersionList<-getEMLVersionList(emlNode)
  #print(EMLVersionList[[1]][2])
  if ((as.numeric(EMLVersionList[[1]][1]) < 2) ||
      ((as.numeric(EMLVersionList[[1]][1]) >=2)&& (as.numeric(EMLVersionList[[1]][2]) < 2))){
    # print("Updating EML Version to 2.2.0")
    xml_attr(emlNode,"xmlns:eml")<-"https://eml.ecoinformatics.org/eml-2.2.0"
    xml_attr(emlNode,"xsi:schemaLocation")<-"https://eml.ecoinformatics.org/eml-2.2.0 https://eml.ecoinformatics.org/eml-2.2.0/eml.xsd"
    xml_attr(emlNode,"xmlns:ds")<-"eml://ecoinformatics.org/eml-2.2.0"
    xml_attr(emlNode,"scope")<-"system"
        # print(xml_attrs(emlNode))
    xml_set_namespace(emlNode,"eml")
    # print("done Updating")
    return(xmldata)
  }
}

getPackageIdFromFile<-function(xmlFile){
  myxmldata<-read_xml(xmlFile,encoding="UTF-8")
  xmlIdList<-xml_find_all(myxmldata,"//eml:eml/@packageId")
  packageId<-as.character(xml_text(xmlIdList[1]))
  return(packageId)
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

isExistingUnitAnnotation<-function(xmldata){
  isExisting=F
  annoList<-xml_find_all(xmldata,".//annotation")
  for (myAnnotation in annoList){
    myURIText<-xml_text(xml_find_first(myAnnotation,".//propertyURI"))
    if (grepl("hasUnit",myURIText)){
      isExisting=T
      # print("Existing Annotation Found\n")
    }
  }
  return(isExisting)
}

rmExistingUnitAnnotation<-function(xmldata){
  annoList<-xml_find_all(xmldata,".//annotation")
  for (myAnnotation in annoList){
    myURIText<-xml_text(xml_find_first(myAnnotation,".//propertyURI"))
    if (grepl("hasUnit",myURIText)){
      xml_remove(myAnnotation)
    }
  }
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

annotateEMLUnits<-function(inEMLFile,incrementRevision=F,addAttributeIds=F,overWriteExisting=T,updateEMLVersion=T){
  library(xml2)
  # ingest an EML file and add an <annotation> element to those <attribute>s whose units
  # can be resolved in QUDT. It returns a string containing the revised EML document. 
  #
  # inEMLFile is the name of the file or URL of the source EML document
  # incrementRevision controls whether the Revision number of the EML package is incremented by 1
  # addAttributeIds controls whether <attribute>s are checked for "id=" attributes and if absent
  # they are automatically added
  # overWriteExisting controls whether existing unit annotations are retained or overwritten
  
  # read in QUDT and UCUM for raw lowercase units over the web or from a local file copy
  QUDTInfoDf<-read.csv("https://github.com/EDIorg/Units-WG/raw/main/RCode_JP/DataFiles4R/unitsWithQUDTInfo.csv")
  
  
  # set up a dataframe to be appended to from the first line of QUDTInfoDf
  QUDTOutDf<-QUDTInfoDf[1,]
  QUDTOutDf$packageId<-NA
  QUDTOutDf$attributeId<-NA
  attribNumber<-1
  
  xmldata<-read_xml(inEMLFile,encoding="UTF-8")

  
  xmlIdList<-xml_find_all(xmldata,"//eml:eml/@packageId")
  packageId<-as.character(xml_text(xmlIdList[[1]]))
  if (incrementRevision){
    xmldata<-updateEMLRevision(xmldata)
  }
  
  if (addAttributeIds){
    addEMLAttributeId(xmldata)
  }
  
  # update EML version to support annotations -if needed 
  if (updateEMLVersion){
    updateEMLVersion(xmldata)
  }
  
  xmlTableList<-xml_find_all(xmldata,"//dataTable")
  for (myDataTable in xmlTableList){
    xmlAttributeList<-xml_find_all(myDataTable,".//attribute")
    for (myAttribute in xmlAttributeList){
      if (xml_has_attr(myAttribute,"id")){
        myAttributeId<-xml_attrs(myAttribute,"id")
      }else{
        myAttributeId<-NA
      }  
      myUnit<-xml_find_first(myAttribute,".//unit")
      myUnitText<-xml_text(myUnit)[[1]][[1]]
      #print(myUnitText)
      myQUDTInfoDf<-QUDTInfoDf[grepl(paste("^",myUnitText,"$",sep=""),trimws(QUDTInfoDf$unit),ignore.case=T),]
      myQUDTInfoDf<-myQUDTInfoDf[!duplicated(myQUDTInfoDf$qudtUri),]
      myQUDTInfoDf<-myQUDTInfoDf[!is.na(myQUDTInfoDf$qudtUri) 
                                 & !is.na(myQUDTInfoDf$unit)
                                 & myQUDTInfoDf$unit != "NA"
                                 ,]
      #print(myQUDTInfoDf)
      if (nrow(myQUDTInfoDf) == 1){
        #   print(myAttributeId)
        #   print(myUnitText)
        myQUDTInfoDf$packageId<-packageId
        myQUDTInfoDf$attributeId<-myAttributeId
        # print(myQUDTInfoDf$qudtUri)
        QUDTOutDf<-rbind(QUDTOutDf,myQUDTInfoDf)
        
        if (isExistingUnitAnnotation(myAttribute)==T & overWriteExisting == T){
          rmExistingUnitAnnotation(myAttribute)
        }
        if (isExistingUnitAnnotation(myAttribute)== F){
          # Add annotation node to attribute
        xml_add_child(myAttribute,read_xml(charToRaw(paste('<annotation>
        <propertyURI label="has unit">http://qudt.org/schema/qudt/hasUnit</propertyURI>',
                          paste('  <valueURI label="',trimws(myQUDTInfoDf$qudtLabel),'">',myQUDTInfoDf$qudtUri,'</valueURI>',sep=""),
                          "</annotation>",sep="\n"))))
        } #end if no existing annotation 
      } # end if unit found
    } # end for attributes
  }# end for dataTables
  
  # write it out to disk
  #write_xml(xmldata,paste(packageId,"_annotated.xml"))
  outXMLString<-convertSpecialCharacters(as.character(xmldata))
  return(outXMLString)
} 

createUnitsAnnotationsListString<-function(inEMLFile,indent=0){
  # create an <annotations> element including multiple <annotation> elements
  # and return it as a string
  # inEMLFile is the name of a file or URL that is an EML document
  # indent is an optional number of spaces to indent each line in the <annotations>
  indentStr<-""
  if(indent > 0){
    for(i in 1:indent){
      # print(i)
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
        # print(myAttributeId)
        # print(myUnitText)
        myQUDTInfoDf$packageId<-packageId
        myQUDTInfoDf$attributeId<-myAttributeId
        # print(myQUDTInfoDf$qudtUri)
        QUDTOutDf<-rbind(QUDTOutDf,myQUDTInfoDf)
        
      } # end if unit found
    } # end for attributes
  }# end for dataTables
  
  
  # get rid of top line that has missing attribute info or, if no attribute IDs are supplied, all lines
  QUDTOutDf<-QUDTOutDf[!is.na(QUDTOutDf$attributeId),]
  # UCUMOutDf<-UCUMOutDf[!is.na(UCUMOutDf$attributeId),]
  
  # Now write out the annotations
  if(nrow(QUDTOutDf) > 0){
    #print(length(QUDTOutDf))
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
