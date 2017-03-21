#' @title Import ucsc chromosomes sizes
#' @author Zacharie Menetrier
#' @description Download and import the chromosome sizes from any species in the ucsc data base.
#' 
#' @param id The ucsc id of the assembly.
#' @param all=FALSE If TRUE then all the chromosomes sizes will be returned in the data frame, only the ones with no "_" in their names will be returned.
#' 
#' @return A vector that contains the chromosome lengths.
#' 
#' @export
ImportUcscChromSizes <- function(id, all = FALSE) {
    connection <- dbConnect(RMySQL::MySQL(), username = "genome", host = "genome-mysql.cse.ucsc.edu", dbname = id)
    res <- dbSendQuery(connection, "select chrom,size from chromInfo")
    data <- dbFetch(res)
    if(!all){
        data <- data[!grepl("_",data$chrom),]
    }
    t = data.frame(data$size)
    rownames(t) = data$chrom
    colnames(t) = "size"
    return(t)
}