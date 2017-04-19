catalog <- BedToGranges(DownloadRemapCatalog("big_data"))
query <- BedToGranges(DownloadEncodePeaks("ENCFF001VCU", "big_data"))
universe <- BedToGranges(DownloadEncodePeaks("ENCFF718QVA", "big_data"))
enrichment <- GrEnrichment(query, catalog, universe)