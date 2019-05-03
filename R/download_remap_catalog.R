#' @title Download Remap catalogue
#' @author Zacharie Menetrier
#' @description Download or import in a data frame the Remap catalogue for 
#'  transcriptions factors.
#' 
#'
#' @param targetDir The name of the directory to download the catalogue in.
#' @param fileName="" The name of the file after downloading.
#' If left empty, default url names will be applied.
#' @param version="2018" The year version of the catalog 2018 or 2015.
#' @param assembly="hg38" The  genomic version of assembly hg38 or hg19 for human. 
#' @param force=FALSE If FALSE (default), then no file is overwrited and the
#' user is given confirmation message.
#' @param store=TRUE If TRUE (default) then a file is downloaded and written
#' on the disk else it is only loaded as an R object.
#' 
#' @return A data frame containing the ReMap genomic regions if store = FALSE
#' else the path to the catalog file.
#' 
#' @usage downloadRemapCatalog(targetDir, fileName = "", 
#' version = "2018", assembly = "hg38", force = FALSE, store = TRUE)
#' 
#' @examples
#' remapFile <- downloadRemapCatalog(targetDir = "~/ReMapEnrich_demo/data/ReMap")
#' remap <- bedToGranges(remapFile)
#' 
#' @export
downloadRemapCatalog <- function(targetDir,
                                 fileName = "", 
                                 version = "2018",
                                 assembly = "hg38",
                                 force = FALSE,
                                 store = TRUE) {
    
    if (version != "2018" && version != "2015") {
        message("Invalid version of catalog, choose between 2015 and 2018.")
        stop()
    }
    if (assembly != "hg38" && assembly != "hg19") {
        message("Invalid assembly, choose between hg19 and hg38.")
        stop()
    }
    size <- "2"
    if (version == "2015") {
        size <- "0.5"
    }
    url <- "http://tagc.univ-mrs.fr/remap/download/"
    if (version == "2018" && assembly == "hg38") {
        url <- paste(url, "remap2018/hg38/MACS/remap2018_nr_macs2_hg38_v1_2.bed.gz", sep = "")
    }
    else if (version == "2018" && assembly == "hg19") {
        url <- paste(url, "remap2018/hg19/MACS/remap2018_nr_macs2_hg19_v1_2.bed.gz", sep = "")
    } 
    else if (version == "2015" && assembly == "hg38") {
        url <- paste(url, "remap2015/hg38/MACS/remap2015_nr_macs2_hg38_v1.bed.gz", sep = "")
    }
    else if (version == "2015" && assembly == "hg19") {
        url <- paste(url, "remap2015/hg19/MACS/remap2015_TF_archive_nr_macs2_hg19_v1.tar.gz", sep = "")
    }
    if (fileName == "") {
        splits <- strsplit(url, "/")
        fileName <- gsub(".gz", "", tail(unlist(splits), n = 1))
    }
    filePath <-file.path(targetDir,fileName)
    fileExists <- file.exists(filePath)
    input <- "Y"
    if (!force && !fileExists) {
        input <- readline(prompt=paste("A ", size, " GB file will be downloaded. 
                          Do you want to continue Y/N : "))
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
