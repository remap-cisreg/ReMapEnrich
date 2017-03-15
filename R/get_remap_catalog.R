#' Import Remap catalogue
#' 
#' Download and import in a data frame the Remap catalogue for transcriptions factors.
#' The file is donwloaded at the given file path and is a gzipped .bed file.
#' 
#' @param filePath The name of the file to be created when downloading the catalogue.
#' 
#' @return A data frame containing the Remap genomic regions.
#' 
#' @export
GetRemapCatalog <- function(filePath){
    tempZipFile <- tempfile()
    url <- "http://tagc.univ-mrs.fr/remap/download/All/nrPeaks_all.bed.gz"
    download.file(url, tempZipFile)
    remap <- BedToGranges(R.utils::gunzip(tempZipFile, filePath))
    unlink(tempZipFile)
    return(remap)
}
