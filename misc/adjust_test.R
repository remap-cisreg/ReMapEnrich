catalog <- GenRegions(10000, 1000)
catalog@elementMetadata$id = "Vert"
randoms <- RandomIntersections(catalog, iterations = 10000, regionNb = 1000, regionSize = 1000)
chi2results <- AdjustToPoisson(randoms)
