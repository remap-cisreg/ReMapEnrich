catalog <- GenRegions(10000, 1000)
catalog@elementMetadata$id = "Vert"
randoms <- RandomIntersections(catalog, iterations = 10000, regionNb = 1000, regionSize = 1000)
chi2results <- AdjustToPoisson(randoms)
h <- hist(randoms, breaks = 0:(max(randoms)+1), plot = FALSE)
expOverlaps <- exp.overlaps <- dpois(x = 0:max(randoms), lambda = mean(randoms)) * sum(h$counts)
plot(h$counts, type = 'h', col = "red")
lines(expOverlaps, col = "blue")
