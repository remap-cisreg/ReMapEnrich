#' @title Download Remap catalogue
#' @author Zacharie Menetrier
#' @description Download or import in a data frame the Remap catalogue for 
#'  transcriptions factors.
#' 
#'
#' @param targetDir The name of the directory to download the catalogue in.
#' @param fileName The name of the file to be created after the downloaded
#'  catalogue.
#' @param version="2018" The year version of the catalog 2018 or 2015.
#' @param assembly="hg38" The  genomic version of assembly hg38 or hg19.
#' @param force=FALSE If FALSE (default), then no file is overwrited and the
#' user is given confirmation message.
#' @param store=TRUE If TRUE (default) then a file is downloaded and written
#' on the disk else it is only loaded as an R object.
#' 
#' @return A data frame containing the Remap genomic regions if store = FALSE
#' else the path to the catalog file.
#' 
#' @usage downloadRemapCatalog(targetDir, fileName = "ReMap2_nrPeaks.bed", 
#' version = "2018", assembly = "hg38", force = FALSE, store = TRUE)
#' 
#' @examples
#' remapFile <- downloadRemapCatalog(targetDir = "~/roken_demo/data/ReMap")
#' remap <- bedToGranges(remapFile)
#' 
#' @export
downloadRemapCatalog <- function(targetDir,
                                 fileName = "ReMap2_nrPeaks.bed", 
                                 version = "2018",
                                 assembly = "hg38",
                                 force = FALSE,
                                 store = TRUE) {
    
    input <- "Y"
    if (version == "2018") {
        if (assembly == "hg38") {
            url <- "http://tagc.univ-mrs.fr/remap/download/MACS/ReMap2_nrPeaks.bed.gz"
            fileName <- "ReMap2_nrPeaks.bed"
        } else {
            if (assembly == "hg19") {
                url <- "http://tagc.univ-mrs.fr/remap/download/MACS_lifted_hg19/ReMap2_nrPeaks_hg19.bed.gz"
                fileName <- "ReMap2_nrPeaks_hg19.bed"
            } else {
                message("Please choose version into 2015 and 2018 AND assembly into hg38 or hg19")
                stop()
            }
        }
    } else {
        if (version == "2015") {
            if (assembly == "hg38") {
                url <- "http://tagc.univ-mrs.fr/remap/download/ReMap1_lifted_hg38/remap1_hg38_nrPeaks.bed.gz"
                fileName <- "Rremap1_hg38_nrPeaks.bed"
            } else {
                if (assembly == "hg19") {
                    url <- "http://tagc.univ-mrs.fr/remap/download/remap1/All/nrPeaks_all.bed.gz"
                    fileName <- "nrPeaks_all.bed"
                } else {
                    message("Please choose version into 2015 and 2018 AND assembly into hg38 or hg19")
                    stop()
                }
            }
        }
    }
    filePath <-file.path(targetDir,fileName)
    fileExists <- file.exists(filePath)
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
                return(bedToGranges(filePath))
                }
        } else {
            tempZipFile <- paste(tempfile(),".bed.gz", sep = "")
            utils::download.file(url, tempZipFile)
            R.utils::gunzip(tempZipFile, filePath, overwrite = force)
            unlink(tempZipFile)
            message("A file has been created at ", filePath)
            if (store) {
                return(filePath)
            } else {
                remapCatalog <- bedToGranges(filePath)
                unlink(filePath)
                return(remapCatalog)
            }
        }
    }
}
