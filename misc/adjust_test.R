catalog <- BedToGranges("big_data/nrPeaks_all.bed")
randoms <- RandomIntersections(catalog, 500, 1000, 1000)
chi2results <- AdjustToPoisson(randoms)
