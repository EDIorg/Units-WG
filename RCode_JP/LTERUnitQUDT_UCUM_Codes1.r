# Download the RDF data from QUDT
# Extract the labels (US English preferred)
# and Extract all the UCUM codes (often multiple ones per unit)
# and write out output CSV files for later use. 
# John Porter, March, 2023

rm(list=ls())
setwd("D:/Box Sync/EMLUnits")

library(rdflib)

r1<-rdf_parse("https://qudt.org/2.1/vocab/unit",format='turtle')

# get all the labels regardless of language
x1<-rdf_query(r1,'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?unit ?label
WHERE { ?unit rdfs:label ?label . }', data.frame=TRUE)

# get only the EN-US labels
x2<-rdf_query(r1,'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?unit ?label 
WHERE { ?unit rdfs:label ?label . 
        FILTER (lang(?label) = "en-us") }',data.frame=TRUE)
colnames(x2)<-c("unit","en-usLabel")

# get the ucumCode(s)
x3<-rdf_query(r1,'PREFIX qudt: <http://qudt.org/schema/qudt/>
SELECT ?unit  $ucumCode
WHERE { ?unit qudt:ucumCode ?ucumCode . 
       }',data.frame=TRUE)

# get the Dimension vector
x4<-rdf_query(r1,'PREFIX qudt: <http://qudt.org/schema/qudt/>
SELECT ?unit  ?hasDimensionVector
WHERE { ?unit qudt:hasDimensionVector ?hasDimensionVector . 
       }',data.frame=TRUE)

# get the conversion multiplier
x5<-rdf_query(r1,'PREFIX qudt: <http://qudt.org/schema/qudt/>
SELECT ?unit  ?conversionMultiplier
WHERE { ?unit qudt:conversionMultiplier ?conversionMultiplier . 
       }',data.frame=TRUE)

# get non-latex descriptions
x6<-rdf_query(r1,'PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX qudt: <http://qudt.org/schema/qudt/>
SELECT ?unit  ?description
WHERE { ?unit dcterms:description ?description . 
      FILTER( datatype(?description)!=qudt:LatexString)}',data.frame=TRUE)
colnames(x6)<-c("unit","nonLatexDescription")

# get LatexDescriptions
x7<-rdf_query(r1,'PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX qudt: <http://qudt.org/schema/qudt/>
SELECT ?unit  ?description
WHERE { ?unit dcterms:description ?description . 
      FILTER( datatype(?description)=qudt:LatexString)}',data.frame=TRUE)
colnames(x7)<-c("unit","LatexDescription")

qudtLabelDf<-merge(x1,x2,by="unit",all=T)
qudtLabelDf<-qudtLabelDf[!duplicated(qudtLabelDf),]
# use US labels when available, not if otherwise
qudtLabelDf$label<-ifelse(is.na(qudtLabelDf$`en-usLabel`),qudtLabelDf$label,qudtLabelDf$`en-usLabel`)
qudtLabelDf<-qudtLabelDf[,c("unit","label")]

# add dimensions, conversions and descriptions
qudtLabelDf<-merge(qudtLabelDf,x4,by="unit",all.x=T)
qudtLabelDf<-merge(qudtLabelDf,x5,by="unit",all.x=T)
qudtLabelDf<-merge(qudtLabelDf,x6,by="unit",all.x=T)
qudtLabelDf<-merge(qudtLabelDf,x7,by="unit",all.x=T)

ucumCodesDf<-merge(x1,x3,all=T)
ucumCodesDf<-ucumCodesDf[,c("unit","ucumCode")]
rm(x1,x2,x3,x4,x5,x6,x7,r1)

# Set unit to qudtUri for compatibility
colnames(qudtLabelDf)<-c("qudtUri","qudtLabel","qudtDimensionVector","qudtConversionMultiplier","nonLatexDescription","latexDescription")
colnames(ucumCodesDf)<-c("qudtUri","ucumCode")


# get rid of duplicates - only one line per URI
# a few units have >2 labels which generates duplicates
qudtLabelDf<-qudtLabelDf[!duplicated(qudtLabelDf$qudtUri),]
# get rid of dups but allow multiple lines per URI for ucum codes
ucumCodesDf<-ucumCodesDf[!duplicated(ucumCodesDf),]

write.csv(qudtLabelDf,"qudtLabels.csv",row.names=F)
write.csv(ucumCodesDf,"ucumCodes.csv",row.names=F)
