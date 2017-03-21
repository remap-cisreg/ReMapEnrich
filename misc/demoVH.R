query <- BedToGranges("inst/extdata/ReMap_nrPeaks_public_chr22_SOX2.bed")
cat <- BedToGranges("inst/extdata/ReMap_nrPeaks_public_chr22.bed")

baseEnrich = GrEnrichment(query, cat)

nbIntersects = c()
lambdas = c()
for (i in 1:10) {
    enrich = GrEnrichment(GrShuffle(query), cat)
    nbIntersects = c(nbIntersects, enrich$catCount)
    lambdas = c(lambdas, enrich$theoricalMeans)
}