#' @title Import chromosomes sizes
#' @author Zacharie Menetrier
#' @description Import chromosome sizes file into a table.
#' 
#' @param filePath The path of the file containing the chromsomes sizes.
#' 
#' @return A table that contains the chromosome sizes.
#' 
#' @examples 
#' hg19ChromFile <- LoadChromFile("hg19")
#' hg19ChromSizes <- ImportChromSizes(hg19ChromFile)
#' 
#' @export
ImportChromSizes <- function(filePath) {
	chromSizes <- read.table(filePath, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = "", row.names = 1)
	colnames(chromSizes) = "size"
	return(chromSizes)
}