#' @title Download Remap catalogue
#' @author Zacharie Menetrier
#' @description Download and import in a data frame the Remap catalogue for transcriptions factors.
#' The file is donwloaded at the given file path and is a gzipped .bed file.
#' 
#' @param targetDir The name of the directory to download the catalogue in.
#' @param fileName="nrPeaks_all.bed.gz" The name of the file to be created after the downloaded catalogue.
#' 
#' @return A data frame containing the Remap genomic regions.
#' 
#' @export
DownloadRemapCatalog <- function(targetDir, fileName = "nrPeaks_all.bed", force = FALSE) {
    # If force is true then skip the confirmation.
    if (!force) {
        input = ""
        input <- readline(prompt="A 0.5 GB file will be downloaded. Do you want to continue Y/N : ")
        while (input != "Y" && input != "N") {
            input <- readline(prompt="Please type Y or N and press Enter : ")
        }
    } else {
        # If force is true then the input is set to YES.
        input = "Y"
    }
    if (input == "Y") {
        tempZipFile <- paste(tempfile(),".bed.gz", sep = "")
        url <- "http://tagc.univ-mrs.fr/remap/download/All/nrPeaks_all.bed.gz"
        path <-file.path(targetDir,fileName)
        # If force is FALSE and the file already exists then return an error.
        if(file.exists(path) && !force)
            stop("The file ", fileName, " already exists. You may want to use 'force =TRUE' to overwrite this file.")
        download.file(url, tempZipFile)
        R.utils::gunzip(tempZipFile, path, overwrite = force)
        unlink(tempZipFile)
        message("A file has been created at ", path)
    }
}