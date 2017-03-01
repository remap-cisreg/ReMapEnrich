#' Import bed files
#' 
#' Imports bed files automatically in a nice data frame.
#' 
#' @param file_path The path of the file to import.
#' 
#' @return A data frame with the corresponding chromosomic regions of the bed file.
#' 
#' @export
import_bed = function(file_path)
{
    # Gets the data frame from the file path.
    genomic_regions = as.data.frame(read.table(file_path, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = ""))
    # Renaming the data frame columns to fall in the bed standard.
    colnames(genomic_regions) = c("chr", "start", "end")
    return(genomic_regions)
}