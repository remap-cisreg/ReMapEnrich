#' @title Import chromosomes sizes
#' @author Zacharie Menetrier
#' @description Import chromosome sizes file into a table.
#' 
#' @param filePath The path of the file containing the chromsomes sizes.
#' 
#' @return A table that contains the chromosome sizes.
#' 
#' @usage importChromSizes <- function(filePath)
#' 
#' @examples 
#' hg19ChromFile <- loadChromFile("hg19")
#' hg19ChromSizes <- importChromSizes(hg19ChromFile)
#' 
#' @export
importChromSizes <- function(filePath) {
	chromSizes <- utils::read.table(filePath, header = FALSE, sep = "\t",
	                         stringsAsFactors = FALSE, quote = "", row.names = 1)
	colnames(chromSizes) = "size"
	return(chromSizes)
}
