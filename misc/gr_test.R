queryFile <- "~/Documents/semestre2/PIB/roken/big_data/ENCFF001VCU.bed"
catFile <- "~/Documents/semestre2/PIB/roken/big_data/nrPeaks_all.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
enrich <- GrEnrichment(query, cat, shuffles = 10, lower = FALSE)