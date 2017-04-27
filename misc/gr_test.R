queryFile <- "inst/extdata/ReMap_nrPeaks_public_chr22_SOX2.bed"
catFile <- "inst/extdata/ReMap_nrPeaks_public_chr22.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
enrich <- Enrichment(query, cat, shuffles = 50, byChrom = TRUE, nCores = 7)
