## ---- echo=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(collapse = T, 
                      comment = "##    ", 
                      eval=TRUE, 
                      echo=FALSE, 
                      warning = FALSE, 
                      results = FALSE, 
                      message = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)

## ----init_query, echo=TRUE-------------------------------------------------
# Load the ReMapEnrich library
library(ReMapEnrich) 

# Load the example dataset
query <- bedToGranges(system.file("extdata",
                                  "ReMap_nrPeaks_public_chr22_SOX2.bed",
                                  package = "ReMapEnrich"))

catalog <- bedToGranges(system.file("extdata",
                                    "ReMap_nrPeaks_public_chr22.bed",
                                    package = "ReMapEnrich"))

## ----echo=TRUE, results=TRUE-----------------------------------------------
# Check what the catalog contains
print(catalog)

# Check what the query contains
print(query)


## ----echo=TRUE, results=TRUE-----------------------------------------------
enrichment.df <- enrichment(query, catalog, byChrom = TRUE)
head(enrichment.df)

## --------------------------------------------------------------------------
# Display a bar plot
enrichmentBarPlot(enrichment.df, sigDisplayQuantile = 0.5, top = 20, aRisk = 0.00001)

## ---- echo=TRUE------------------------------------------------------------
# Display a volcano plot (na.omit() is mandatory as there is NAs in the enrichment data frame).
enrichmentVolcanoPlot(na.omit(enrichment.df), sigDisplayQuantile = 0.9, aRisk = 0.00001)

## ---- echo=TRUE, fig.height=7----------------------------------------------
# Display a dot plot.
enrichmentDotPlot(enrichment.df)

## ---- echo=TRUE, fig.height=7----------------------------------------------
# Display a dot plot without SOX2.
enrichmentDotPlot(enrichment.df[enrichment.df$category != "SOX2",])

