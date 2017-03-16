#' Create an output file with the format chosen by user or a capture like 'txt' if none is selected
#' 
#'  @param dat dataframe to output.
#'  @param fileName Name of the output file chosen.
#'  @param format The format chosen.
#'  @param plot Boolean option T or F if user wants a plot. 
#'      
#'  @export
ExportEnrichment  <- function(dat, fileName = "output", format = "default", plot = FALSE)
{
    if(format == "csv")
    {
        write.csv(dat, file = fileName)
    }
    if (format == "tsv")
    {
        write.table(dat, file = fileName, quote = FALSE, sep = '\t', col.names = NA)
    }
    else
    {
        # Realize a capture of the dataframe if format isn't selected.
        capture.output(dat, file = fileName)
    }
    # Verify if user wants a plot with the output file.
    if(plot == TRUE)
   {
        # Create a default plot = barplot.
        EnrichmentBarPlot(dat, lengthData = 10, aRisk = 0.05)
   }
}