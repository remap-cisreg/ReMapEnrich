#' Create an output file with the format chosen by user or a capture like 'txt' if none is selected
#' 
#'  @param dat dataframe to output
#'  @param name.file Name of the output file chosen
#'  @param format The format chosen
#'      
#'  @export
output.file  =function(dat, name.file, format = "default")
{
    if(format == "csv")
    {
        write.csv(dat, file = name.file)
    }
    if (format == "tsv")
    {
        write.table(dat, file = name.file, quote = FALSE, sep = '\t', col.names = NA)
    }
    if (format == "default")
    {
        # Realize a capture of the dataframe if format isn't selected
        capture.output(dat, file = name.file)
    }
}