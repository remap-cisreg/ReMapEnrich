queryFile <- "inst/extdata/ReMap_nrPeaks_public_chr22_SOX2.bed"
catFile <- "inst/extdata/ReMap_nrPeaks_public_chr22.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
enrich <- GrEnrichment(query, cat, shuffles = 10, lower = FALSE)
