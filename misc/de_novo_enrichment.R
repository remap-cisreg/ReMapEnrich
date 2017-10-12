catalog <- bedToGranges(downloadRemapCatalog("big_data"))
query <- bedToGranges(downloadEncodePeaks("ENCFF001VCU", "big_data"))
universe <- bedToGranges(downloadEncodePeaks("ENCFF718QVA", "big_data"))
enrichment <- enrichment(query, catalog, shuffles = 6, nCores = 3)
