query <- BedToGranges("~/roken/inst/extdata/ReMap_nrPeaks_public_chr22_SOX2.bed")
cat <- BedToGranges("~/roken/inst/extdata/ReMap_nrPeaks_public_chr22.bed")

baseEnrich = GrEnrichment(query, cat)

nbIntersects = c()
lambdas = c()
for (i in 1:10) {
    enrich = GrEnrichment(GrShuffle(query), cat)
    nbIntersects = c(nbIntersects, enrich$nb.overlap)
    lambdas = c(lambdas, enrich$random.average)
}