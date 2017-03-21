catalog = BedToGranges("big_data/wgEncodeRegTfbsClusteredV3.bed")
pVals = c()
lambdas = c()
nbIntersects = c()
effectSizes = c()
for(i in 1:10) {
    cat(i, "\r")
    enrichment = GrEnrichment(GenRegions(1000,3000), catalog)
    enrichment = enrichment[order(enrichment$category),]
    pVals = c(pVals, enrichment$adjusted.p.value)
    lambdas = c(lambdas, enrichment$random.average)
    nbIntersects = c(nbIntersects, enrichment$nb.overlaps)
    effectSizes = c(effectSizes, enrichment$effect.size)
}
effectSizes = matrix(effectSizes, ncol = 10)
pVals = matrix(pVals, ncol = 10)
nbIntersects = matrix(nbIntersects, ncol = 10)
lambdas = matrix(lambdas, ncol = 10)