randomCatalog <- GenRegions(10000, 200)
randomCatalog <- reduce(randomCatalog)
randomCatalog@elementMetadata$id = "Vert"
randomsCatalogIntersections <- RandomIntersections(randomCatalog, iterations = 1000, regionNb = 1000, regionSize = 1000)
chi2results <- AdjustToPoisson(randomsCatalogIntersections)
categories <- c("Vert","Bleu","Rouge")
for (category in categories) {
    catResults <- randomsCatalogIntersections[,category]
    h <- hist(catResults, breaks = -1:(max(catResults)+1), plot = FALSE)
    expOverlaps <- exp.overlaps <- dpois(x = 0:(max(catResults)+1), lambda = mean(catResults)) * sum(h$counts)
    plot(h$counts, type = 'h', col = "red", main = category)
    lines(expOverlaps, col = "blue")
}
