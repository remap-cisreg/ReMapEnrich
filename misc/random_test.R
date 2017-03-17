query = BedToGranges("big_data/ENCFF001VCU.bed")
catalog = BedToGranges("big_data/nrPeaks_all.bed")
pVals = c()
lambdas = c()
for(i in 1:2) {
    cat(i, "\r")
    enrichment = GrEnrichment(GrShuffle(query), catalog)
    pVals = c(pVals, enrichment$adjusted.p.value)
    lambdas = c(lambdas, enrichment$random.average)
}