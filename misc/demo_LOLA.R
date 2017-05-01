
demo_time <- function() {
library("LOLA")
dbPath = system.file("extdata", "hg19", package="LOLA")
regionDB = loadRegionDB(dbPath)
queryFile = "big_data/ENCFF001SUX.bed"
catFile = "big_data/nrPeaks_all.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
userSets <- query
userUniverse <- cat
locResults = runLOLA(userSets, userUniverse, regionDB, cores=1, suppressWarnings())  # voir nb cores  pour roken
locResults[order(maxRnk, decreasing=TRUE),]
writeCombinedEnrichment(locResults, outFolder= "lolaResults")
}
print(system.time(enrichLOLA <- demo_time()))
