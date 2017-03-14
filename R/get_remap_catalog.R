#' @export
GetRemapCatalog <- function(filePath){
    tempZipFile <- tempfile()
    url <- "http://tagc.univ-mrs.fr/remap/download/All/nrPeaks_all.bed.gz"
    download.file(url, tempZipFile)
    remap <- BedToGranges(R.utils::gunzip(tempZipFile, filePath))
    unlink(tempZipFile)
    return(remap)
}
