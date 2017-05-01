catalog <- BedToGranges(DownloadRemapCatalog("big_data"))
query <- BedToGranges(DownloadEncodePeaks("ENCFF001SUX", "big_data"))
universe <- BedToGranges(DownloadEncodePeaks("ENCFF718QVA", "big_data"))
locResults <- runLOLA(query, universe, regionDB, cores=1)  # voir nb cores  pour roken
# lola <- locResults[order(maxRnk, decreasing=TRUE),]
# writeCombinedEnrichment(locResults, outFolder= "lolaResults")

