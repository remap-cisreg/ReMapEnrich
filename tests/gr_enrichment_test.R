cat = BedToGranges("big_data/nrPeaks_all.bed")
query = BedToGranges("big_data/ENCFF001VCU.bed")
enrich = GrEnrichment(query, cat)