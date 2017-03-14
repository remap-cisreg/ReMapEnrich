queryFile = "big_data/ENCFF001VCU.bed"
catFile = "big_data/nrPeaks_all.bed"
query = BedToGranges(queryFile)
cat = BedToGranges(catFile)
enrichment = GrEnrichment(query, cat, shuffles = 10, lower = FALSE)
