## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", eval=FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)

## ------------------------------------------------------------------------
#  # Load the roken library
#  library(roken)
#  
#  # Load the example catalogue BED file.
#  
#  catalogFile <- system.file("extdata",
#                                      "ReMap_nrPeaks_public_chr22.bed",
#                                      package = "roken")
#  catalog <- BedToGranges(catalogFile)
#  

## ---- echo=TRUE----------------------------------------------------------
#  # Create a local directory for the tutorial
#  demo.dir <- "~/roken_demo"
#  dir.create(demo.dir, showWarnings = FALSE, recursive = TRUE)
#  setwd(demo.dir)
#  # Use the function DowloadRemapCatalog
#  remapCatalog <- DownloadRemapCatalog("nrpeaks_all.bed.gz")
#  

## ------------------------------------------------------------------------
#  # Downloading the ENCFF001VCU regions.
#  ENCFF001VCU <- BedToGranges(DownloadEncodePeaks("ENCFF001VCU", demo.dir))

## ------------------------------------------------------------------------
#  # Download a universe.
#  universe <- BedToGranges(DownloadEncodePeaks("ENCFF718QVA", demo.dir))
#  # Create the enrichment with the universe.
#  enrichment <- GrEnrichment(ENCFF001VCU, remapCatalog, universe)

## ------------------------------------------------------------------------
#  # Create the enrichment with a less restrictive universe.
#  enrichment <- GrEnrichment(ENCFF001VCU, remapCatalog, universe, included = 0.1)
#  # 90% of the shuffled regions can now be outside of the universe regions.

## ------------------------------------------------------------------------
#  # Create the enrichment with a less restrictive universe.
#  enrichment <- GrEnrichment(ENCFF001VCU, remapCatalog, universe, included = 0.1, byChrom = TRUE)
#  # 90% of the shuffled regions can now be outside of the universe regions.
#  # The shuffled regions are still in the same chromosome where they came from.

## ------------------------------------------------------------------------
#  # Shuffling ENCFF001VCU
#  shuffledENCFF001VCU <- GrShuffle(ENCFF001VCU, universe = universe, byChrom = TRUE)

## ------------------------------------------------------------------------
#  # Generate 100 random regions with a size of 1000 bases pair.
#  randomRegions <- GenRegions(100, 1000)

## ------------------------------------------------------------------------
#  hg19ChromSizes <- LoadChromSizes("hg19")

## ------------------------------------------------------------------------
#  # Example with rn5
#  rn5ChromSizes <- DownloadUcscChromSizes("rn5")
#  # Creation of random regions in the rattus norvegicus genome.
#  randomRegions <- GenRegions(100, 1000, rn5ChromSizes)
#  # Shuffling of regions in the rattus norvegicus genome.
#  shuffledRegions <- GrShuffle(randomRegions, rn5ChromSizes)

