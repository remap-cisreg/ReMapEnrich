catalog <- BedToGranges(DownloadRemapCatalog("big_data"))
query <- BedToGranges(DownloadEncodePeaks("ENCFF001SUX", "big_data"))
enrichment <- GrEnrichment(query, catalog)
