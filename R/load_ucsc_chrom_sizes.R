#' @title Load ucsc chromosomes sizes
#' @author Zacharie Menetrier
#' @description Download the chromosome sizes from any species in the ucsc data base.
#' Need mysql installed and in the correct PATH to run.
#' 
#' @param id The ucsc id of the assembly.
#' @param file="" The path to a file where the chromosomes sizes will be saved. If empty no file is created.
#' @param all=FALSE If TRUE then all the chromosomes sizes will be returned in the data frame, only the ones with no "_" in their names will be returned.
#' 
#' @return A vector that contains the chromosome lengths.
#' 
#' @export
LoadUcscChromSizes <- function(id, file = "", all = FALSE) {
    path <- file
    if(file == "")
        path <- tempfile()
    command <- paste("mysql  --user=genome --host=genome-mysql.cse.ucsc.edu -A -D", id, "-e 'select chrom,size from chromInfo' >", path)
    system(command)
    if(file != "")
        return(path)
    chromSizes <- read.table(path, header = TRUE, sep = "\t",stringsAsFactors = FALSE, quote = "", row.names = 1)
    unlink(path)
    if(!all)
        chromSizes <- chromSizes[-grepl("_", rownames(chromSizes))]
    return(chromSizes)
}