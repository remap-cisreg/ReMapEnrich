## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", eval=TRUE)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)

## ---- echo=FALSE---------------------------------------------------------
# Load the roken library
library(roken) 

# Load the example dataset
query <- bedToGranges(system.file("extdata",
                                  "ReMap_nrPeaks_public_chr22_SOX2.bed",
                                  package = "roken"))

catalog <- bedToGranges(system.file("extdata",
                                    "ReMap_nrPeaks_public_chr22.bed",
                                    package = "roken"))

## ------------------------------------------------------------------------
# Check what the catalog contains
catalog
# Check what the query contains
query

## ------------------------------------------------------------------------
enrichment <- enrichment(query, catalog, byChrom = TRUE)
head(enrichment)

## ------------------------------------------------------------------------
# Display a bar plot
enrichmentBarPlot(enrichment, sigDisplayQuantile = 0.5, top = 20, aRisk = 0.00001)

## ---- echo=TRUE----------------------------------------------------------
# Display a volcano plot (na.omit() is mandatory as there is NAs in the enrichment data frame).
enrichmentVolcanoPlot(na.omit(enrichment), sigDisplayQuantile = 0.9, aRisk = 0.00001)

## ---- echo=TRUE, fig.height=7--------------------------------------------
# Display a dot plot.
enrichmentDotPlot(enrichment)

## ---- echo=TRUE, fig.height=7--------------------------------------------
# Display a dot plot without SOX2.
enrichmentDotPlot(enrichment[enrichment$category != "SOX2",])

