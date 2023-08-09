This directory contains code written by John Porter to process units obtained from Ecological Metadata Language metadata.  A data directory contains some of the inputs and outputs. 

There is a PDF flow chart showing R scripts (ovals) and data inputs and outputs (rectangles).  The workflow includes several major steps, 1) assembling the units lists from various sources (LTERUnitAnalysis1.r and IntegrateUnitAnalysis1.r, 2) providing a means to link similar units (IntegrateUnitAnalysis3.r) by creating "pseudunits", 3) Converting pseudounits to QUDT units (IntegrateUnitAnalysis4.r) and verifying that they are in QUDT, 4) linking labels and other information in QUDT and adding UCUM codes to list (LTERUnitCreateDataTables1.r) and 5) draft code for processing EML documents to add <annotation> elements (LTERUnitCreateAnnotations1.r).


![FlowChartImage](https://github.com/EDIorg/Units-WG/assets/8126686/b875ed0c-78c1-4b59-afa9-e2274aaca82b)
