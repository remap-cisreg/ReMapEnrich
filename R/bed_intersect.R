#' @title Genomic regions intersections
#' @author Zacharie Menetrier
#' @description Gets the intersections of two chromosomic regions at a certain fraction.
#' The chromosomic regions are read from bed files.
#' 
#' @param bedfile1 The first bed file.
#' @param bedfile2 The second bed file.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit must exceed to be accounted.
#' @param categories=unique(BedImport(catalogFile)$name) The categories contained in the catalog.
#' This option is leaved for faster calculation when this function is runned multiple times.
#' 
#' @return A data frame (empty if no intersections have been found) containing the intersections and their lengths in base pairs.
#' 
#' @usage 
#' BedIntersect(queryFile, catalogFile, fractionQuery = 0.1,
#' fractionCatalog = 0.1, categories = unique(BedImport(catalogFile)$name))
#' 
#' @examples 
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed", package = "roken")
#' catalogFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed", package = "roken")
#' intersections <- BedIntersect(queryFile, catalogFile)
#' 
#' @export
BedIntersect <- function(queryFile, catalogFile, fractionQuery = 0.1,
                         fractionCatalog = 0.1, categories = unique(BedImport(catalogFile)$name)) {
    ##
    ## WARNING THE LAST VERSION OF BEDTOOLS MUST BE INSTALLED SO THE -F PARAMETER IS IMPLEMENTED AS THE FRACTION QUERY.
    ## FOR NOW THE FRACTION QUERY PARAMETER DOES NOTHING.
    ##
    
    # Creation of the vector that will contain the number of overlaps for each category.
    catCount <- vector()
    catCount[categories] <- 0
    tempPath <- tempfile()
    # Calls bedtools with the corresponding parameters.
    command <- paste("intersectBed -a", catalogFile, "-b", queryFile, "-f", fractionCatalog, ">", tempPath)
    system(command)
    # If the file contains nothing then an empty data frame is returned.
    size <- file.info(tempPath)$size
    if (size == 0) {
        overlaps <- data.frame()
    } else {
        overlaps <- BedImport(tempPath)   
    }
    # Destruction of the temporary file.
    unlink(tempPath)
    # Fills the vector with the corresponding number of overlaps.
    count <- lengths(split(overlaps$name, overlaps$name))
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    return(catCount)
}