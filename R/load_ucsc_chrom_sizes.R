#' @title Load ucsc chromosomes sizes
#' @author Zacharie Menetrier
#' @description Download the chromosome sizes from any species in the ucsc data base.
#' Need mysql installed and in the correct PATH to run.
#' 
#' @param id The ucsc id of the assembly.
#' @param file="" The path to a file where the chromosomes sizes will be saved. If empty no file is created.
#' 
#' @return A table that contains the chromosome lengths.
#' 
#' @export
LoadUcscChromSizes <- function(id, file = "") {
    path <- file
    if(file == "")
        path <- tempfile()
    command <- paste("mysql  --user=genome --host=genome-mysql.cse.ucsc.edu -A -D", id, "-e 'select chrom,size from chromInfo' >", path)
    system(command)
    if(file == "")
        return(path)
    chromSizes <- read.table(path, header = TRUE, sep = "\t",stringsAsFactors = FALSE, quote = "", row.names = 1)
    unlink(tempFile)
    return(chromSizes)
}