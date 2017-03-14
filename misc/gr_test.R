queryFile = "inst/extdata/ReMap_nrPeaks_public_chr22_SOX2.bed"
catFile = "inst/extdata/ReMap_nrPeaks_public_chr22.bed"
query = BedToGranges(queryFile)
cat = BedToGranges(catFile)
c2 = rep(0,20)
for(i in 1:20){
    print(i)
    for(j in 1:5){
        cat("",j,"|")
        enrichment = GrEnrichment(query, cat, lower = FALSE)
        enrichment2 = GrEnrichment(query, cat, lower = FALSE)   
        l = enrichment$category == enrichment2$category
        c2[i] = c2[i] + match(FALSE, l)
    }
    cat("\n")
}
