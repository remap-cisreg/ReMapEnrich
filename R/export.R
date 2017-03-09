#' Create an output file with the format chosen by user or a capture like 'txt' if none is selected
#' 
#'  @param dat dataframe to output
#'  @param name.file Name of the output file chosen
#'  @param format The format chosen
#'      
#'  @export
ExportEnrichment  <- function(dat, fileName = "output", format = "default")
{
    if(format == "csv")
    {
        write.csv(dat, file = fileName)
    }
    if (format == "tsv")
    {
        write.table(dat, file = fileName, quote = FALSE, sep = '\t', col.names = NA)
    }
    if (format == "default")
    {
        # Realize a capture of the dataframe if format isn't selected
        capture.output(dat, file = fileName)
    }
}