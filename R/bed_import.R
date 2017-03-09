#' Import bed files
#' 
#' Imports bed files automatically in a nice data frame.
#' 
#' @param path The path of the file to import.
#' 
#' @return A data frame with the corresponding chromosomic regions of the bed file.
#' 
#' @export
BedImport <- function(path)
{
    # Gets the data frame from the file path.
    regions <- as.data.frame(data.table::fread(path, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = ""))
    # Renaming the data frame columns to fall in the bed standard.
    columnNames <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand", "thickStart",
                         "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts")
    colnames(regions) <- columnNames[1:ncol(regions)]
    return(regions)
}