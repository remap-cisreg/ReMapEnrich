#' @title Genomic regions intersections
#' @author Zacharie Menetrier
#' @description Gets the intersections of two chromosomic regions at a certain fraction.
#' The chromosomic regions are read from bed files.
#' 
#' @param bedfile1 The first bed file.
#' @param bedfile2 The second bed file.
#' @param fraction=0.1 The fraction that intersections shall exceed to be considered.
#' 
#' @return A data frame (empty if no intersections have been found) containing the intersections and their lengths in base pairs.
#' 
#' @export
BedIntersect <- function(queryFile, catalogFile, fractionQuery = 0.1, fractionCatalog = 0.1, categories) {
    ##
    ## WARNING THE LAST VERSION OF BEDTOOLS MUST BE INSTALLED SO THE -F PARAMETER IS IMPLEMENTED AS THE FRACTION QUERY
    ##
    catCount <- vector()
    catCount[categories] <- 0
    tempPath <- tempfile()
    command <- paste("intersectBed -a", catalogFile, "-b", queryFile, "-f", fractionCatalog, ">", tempPath)
    system(command)
    size <- file.info(tempPath)$size
    if (size == 0) {
        overlaps <- data.frame()
    } else {
        overlaps <- BedImport(tempPath)   
    }
    unlink(tempPath)
    count <- lengths(split(overlaps$name, overlaps$name))
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    return(catCount)
}