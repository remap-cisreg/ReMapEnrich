## ---- echo = FALSE, message = FALSE, warning=FALSE, paged.print=TRUE-----
knitr::opts_chunk$set(collapse = T, 
                      comment = "#--", 
                      eval=FALSE)
options(tibble.print_min = 6L, tibble.print_max = 6L)
library(dplyr)

## ---- echo=TRUE----------------------------------------------------------
#  # Create a local directory for the tutorial
#  demo.dir <- "~/ReMapEnrich_demo"
#  dir.create(demo.dir, showWarnings = FALSE, recursive = TRUE)
#  
#  # Use the function DowloadRemapCatalog
#  remapCatalog2018hg38 <- downloadRemapCatalog(demo.dir)
#  
#  # Or download other versions
#  remapCatalog2015hg19 <- downloadRemapCatalog(demo.dir, version = "2015", assembly = "hg19")

## ---- echo=TRUE----------------------------------------------------------
#  # Load the ReMap catalogue and convert it to Genomic Ranges
#  remapCatalog <- bedToGranges(remapCatalog2018hg38)

## ---- echo=TRUE----------------------------------------------------------
#  # Load the ReMapEnrich library
#  library(ReMapEnrich)
#  
#  # Load the example catalogue as a BED file.
#  catalogFile <- system.file("extdata",
#                                      "ReMap_nrPeaks_public_chr22.bed",
#                                      package = "ReMapEnrich")
#  catalog <- bedToGranges(catalogFile)

## ---- echo=TRUE----------------------------------------------------------
#  # Downloading the ENCFF001VCU regions.
#  ENCFF001VCU <- bedToGranges(downloadEncodePeaks("ENCFF001VCU", demo.dir))

## ---- message=FALSE, warning=FALSE, echo=TRUE----------------------------
#  enrichment.df <- enrichment(ENCFF001VCU, remapCatalog)
#  head(enrichment.df)

## ------------------------------------------------------------------------
#  # Download a universe.
#  universe <- bedToGranges(downloadEncodePeaks("ENCFF718QVA", demo.dir))
#  # Convert ReMap to GRanges
#  remapCatalog <- bedToGranges(remapCatalog2018hg38)
#  # Create the enrichment with the universe.
#  enrichment.df <- enrichment(ENCFF001VCU, remapCatalog, universe, nCores=2)

## ------------------------------------------------------------------------
#  # Create the enrichment with a less restrictive universe.
#  enrichment.df <- enrichment(ENCFF001VCU, remapCatalog, universe, included = 0.1, nCores=2)
#  # 90% of the shuffled regions can now be outside of the universe regions.

## ------------------------------------------------------------------------
#  # Create the enrichment with a less restrictive universe.
#  enrichment.df <- enrichment(ENCFF001VCU, remapCatalog, universe, included = 0.1, byChrom = TRUE, nCores=2)
#  # 90% of the shuffled regions can now be outside of the universe regions.
#  # The shuffled regions are still in the same chromosome where they came from.

## ------------------------------------------------------------------------
#  # Shuffling ENCFF001VCU
#  shuffledENCFF001VCU <- shuffle(ENCFF001VCU, universe = universe, byChrom = TRUE)

## ------------------------------------------------------------------------
#  # Generate 100 random regions with a size of 1000 bases pair.
#  randomRegions <- genRegions(100, 1000)

## ------------------------------------------------------------------------
#  hg38ChromSizes <- loadChromSizes("hg38")

## ------------------------------------------------------------------------
#  # Example with rn5
#  rn5ChromSizes <- downloadUcscChromSizes("rn5")
#  
#  # Creation of random regions in the rattus norvegicus genome.
#  randomRegions <- genRegions(100, 1000, rn5ChromSizes)
#  
#  # Shuffling of regions in the rattus norvegicus genome.
#  shuffledRegions <- shuffle(randomRegions, rn5ChromSizes)
#  
#  # Species relevant to current and future ReMap releases
#  hg38ChromSizes <- downloadUcscChromSizes("hg38")
#  hg19ChromSizes <- downloadUcscChromSizes("hg19")
#  mm10ChromSizes <- downloadUcscChromSizes("mm10")
#  dm6ChromSizes <- downloadUcscChromSizes("dm6")
#  
#  # Maybe one day
#  ce11ChromSizes <- downloadUcscChromSizes("ce11")
#  rn5ChromSizes <- downloadUcscChromSizes("rn5")
#  

