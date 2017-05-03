## ---- echo=FALSE, message=FALSE------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", eval=TRUE)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)

## ---- echo=FALSE, eval = FALSE-------------------------------------------
#  # Quick demonstration
#  library(roken)
#  demo.dir <- "~/roken_demo"
#  dir.create(demo.dir, showWarnings = FALSE, recursive = TRUE)
#  setwd(demo.dir)
#  catalog <- BedToGranges(DownloadRemapCatalog("roken_demo"))
#  randomRegionsVsReMap <- RandomIntersections(catalog, iterations = 500, regionNb = 1000, regionSize = 1000)

## ------------------------------------------------------------------------
data("random_regions_vs_ReMap", package = "roken")
head(randomRegionsVsReMap, n = 1)

## ------------------------------------------------------------------------
chi2results <- AdjustToPoisson(randomRegionsVsReMap, showCategories = FALSE)
head(chi2results)

## ------------------------------------------------------------------------
# An easy to do function to create histogram for any result for a category
MakeHistogram <- function(results) {
    h <- hist(results, breaks = -1:(max(results)+1), plot = FALSE)
    expOverlaps <- dpois(x = 0:(max(results)+1), lambda = mean(results)) * sum(h$counts)
    plot(h$counts, col ="red", type = 'h', xaxt = 'nt',
         xlab = "Number of overlaps", ylab = "Number of occurences",
         main = paste("Empirical distribution vs. Poisson distribution for", deparse(substitute(results)))
    )
    axis(side = 1 ,labels = c(0:max(results)), at = c(1:(max(results)+1)))
    lines(expOverlaps, col = "blue")
    legend("topright",
           paste(
            "mean = ", mean(results),
            "\nvariance = ", var(results)
        ), cex = 0.6, bty = 'n'
    )
}
# The best fit is RUNX2
RUNX2 <- randomRegionsVsReMap[,"RUNX2"]
MakeHistogram(RUNX2)
# SOX2 is an average fit
SOX2 <- randomRegionsVsReMap[,"SOX2"]
MakeHistogram(SOX2)
# The worst fit is BRF1
BRF1 <- randomRegionsVsReMap[,"BRF1"]
MakeHistogram(BRF1)

## ---- echo=TRUE----------------------------------------------------------
# Creating a random catalogue with only one category 'Vert'.
randomCatalog <- GenRegions(10000, 200)
# Reduce is a GenomicRanges function designed to merge overlapping regions.
randomCatalog <- reduce(randomCatalog)
# Creating one category for each regions.
randomCatalog@elementMetadata$id = "Vert"
randomCatalogIntersections <- RandomIntersections(randomCatalog, iterations = 500, regionNb = 1000, regionSize = 1000)
MakeHistogram(randomCatalogIntersections)

## ---- eval = FALSE-------------------------------------------------------
#  catalog <- BedToGranges(DownloadRemapCatalog("demo.dir"))
#  query <- BedToGranges(DownloadEncodePeaks("ENCFF001VCU", "demo.dir"))
#  universe <- BedToGranges(DownloadEncodePeaks("ENCFF718QVA", "demo.dir"))
#  enrichment <- Enrichment(query, catalog, universe = universe, byChrom = TRUE, shuffles = 10, included = 0.5, nCores = 7)

## ---- eval = FALSE-------------------------------------------------------
#  randomIntersections <- RandomIntersections(catalog, iterations = 10000, universe = universe, included = 0.5, shuffle = query, byChrom = TRUE)
#  chi2results <- AdjustToPoisson(randomIntersections)

