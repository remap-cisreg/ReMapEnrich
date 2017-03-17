queryFile = "big_data/ENCFF001VCU.bed"
catFile = "big_data/nrPeaks_all.bed"
enrichment = BedEnrichment(queryFile, catFile, fraction = 0, shuffles = 1)
