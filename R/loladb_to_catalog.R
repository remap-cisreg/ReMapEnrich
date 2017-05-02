#' @title Lola DB to catalogue
#' @author Zacharie Menetrier
#' @description Create a GenomicRanges object ready to be used as a catlogue
#' from Lola databases objects.
#' 
#' @param regionDB The region DB to convert.
#' 
#' @return A GenomicRanges object.
#' 
#' @usage LolaDBToCatalog(regionDB)
#' 
#' @export
LolaDBToCatalog <- function(regionDB) {
    # Creating an empty GR object.
    catalog <- GenomicRanges::GRanges()
    # Retrieving the ids from LOLA.
    ids <- regionDB$regionAnno$filename
    # Retrieving the list of GR from LOLA.
    GRList <- regionDB$regionGRL
    # Feed the empty GR object in a loop for every id.
    for (i in 1:length(ids)) {
        GR <- GRList[[i]]
        GR@elementMetadata$id <- ids[i]
        catalog <- c(catalog, GR)
    }
    return(catalog)
}
