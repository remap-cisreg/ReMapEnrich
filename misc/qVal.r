queryFile <- "inst/extdata/ReMap_nrPeaks_public_chr22_SOX2.bed"
catFile <- "inst/extdata/ReMap_nrPeaks_public_chr22.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
enrich <- GrEnrichment(query, cat, shuffles = 10, lower = FALSE)


pVals <- enrich$pVals
pVals <- sort(pVals)

cN <- sum(1/1:length(pVals))

factors <- ((pVals * length(pVals)) / 1:length(pVals)) * cN
qVals <- pVals / factors

rm(queryFile, catFile)


(0.9687378 ** 130) / 130 * cN

# q*(i)
# q(k), k>=i
# q(k) = (p(k)**N / i) * c(N)