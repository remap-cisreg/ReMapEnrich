#' Create an output file with the format chosen by user or a capture like 'txt' if none is selected
#' 
#'  @param dat dataframe to output.
#'  @param fileName Name of the output file chosen.
#'  @param format The format chosen.
#'  @param plot Boolean option T or F if user wants a plot. 
#'      
#'  @export
ExportEnrichment  <- function(dat, fileName = "output", format = "tsv")
{
    if(format == "csv")
    {
        write.csv(dat, file = fileName)
    }
    if (format == "tsv")
    {
        write.table(dat, file = fileName, quote = FALSE, sep = '\t', col.names = NA)
    }
}