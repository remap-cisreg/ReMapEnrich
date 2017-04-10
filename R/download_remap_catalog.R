#' @title Download Remap catalogue
#' @author Zacharie Menetrier
#' @description Download or import in a data frame the Remap catalogue for 
#'  transcriptions factors.
#' 
#' @param targetDir The name of the directory to download the catalogue in.
#' @param fileName="nrPeaks_all.bed.gz" The name of the file to be created 
#'  after the downloaded catalogue.
#' @param force=FALSE If FALSE (default), then no file is overwrited and the
#'  user is given confirmation message.
#' @param store=TRUE If TRUE (default) then a file is downloaded and written
#'  on the disk else it is only loaded as an R object.
#' 
#' @return A data frame containing the Remap genomic regions if store = FALSE
#'  else the path to the catalog file.
#' 
#' @usage DownloadRemapCatalog(targetDir, fileName = "nrPeaks_all.bed",
#'  force = FALSE, store = TRUE)
#' 
#' @examples
#' remapFile <- DownloadRemapCatalog(targetDir = "~/roken_demo/data/ReMap")
#' remap <- BedToGranges(remapFile)
#' 
#' @export
DownloadRemapCatalog <- function(targetDir,
                                 fileName = "nrPeaks_all.bed", 
                                 force = FALSE,
                                 store = TRUE) {
    filePath <-file.path(targetDir,fileName)
    fileExists <- file.exists(filePath)
    input <- "Y"
    if (!force && !fileExists) {
        input <- readline(prompt="A 0.5 GB file will be downloaded. 
                          Do you want to continue Y/N : ")
        while (input != "Y" && input != "N") {
            input <- readline(prompt="Please type Y or N and press Enter : ")
        }
    }
    if (input == "Y") {
        if (fileExists && !force) {
            message("The file ", fileName, " already exists. You may want to use
                    'force = TRUE' to overwrite this file.")
            if (store) {
                return(filePath) 
            } else {
                return(BedToGranges(filePath))
            }
        } else {
            tempZipFile <- paste(tempfile(),".bed.gz", sep = "")
            url <- "http://tagc.univ-mrs.fr/remap/download/All/nrPeaks_all.bed.gz"
            download.file(url, tempZipFile)
            R.utils::gunzip(tempZipFile, filePath, overwrite = force)
            unlink(tempZipFile)
            message("A file has been created at ", filePath)
            if (store) {
                return(filePath)
            } else {
                remapCatalog <- BedToGranges(filePath)
                unlink(filePath)
                return(remapCatalog)
            }
        }
    }
}
