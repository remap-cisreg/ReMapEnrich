randomCatalog <- GenRegions(10000, 1000)
randomCatalog@elementMetadata$id = "Vert"
randomsCatalogIntersections <- RandomIntersections(catalog, iterations = 10000, regionNb = 1000, regionSize = 1000)
chi2results <- AdjustToPoisson(randomsCatalogIntersections)
h <- hist(randomsCatalogIntersections, breaks = 0:(max(randomsCatalogIntersections)+1), plot = FALSE)
expOverlaps <- exp.overlaps <- dpois(x = 0:max(randomsCatalogIntersections), lambda = mean(randomsCatalogIntersections)) * sum(h$counts)
plot(h$counts, type = 'h', col = "red")
lines(expOverlaps, col = "blue")
