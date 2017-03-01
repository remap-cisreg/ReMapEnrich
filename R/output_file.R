#' Create an output file with the format chosen by user or a capture like 'txt' if none is chosen
#' 
#'  @param dat dataframe to output
#'  @param name_file Name of the output file chosen
#'  @param format The format chosen
#'      
#'  @export
output_file  =function(dat, name_file, format = "default")
{
    if(format == "csv")
    {
        write.csv(dat, file = name_file)
    }
    if (format == "tsv")
    {
        write.table(dat, file = name_file, quote = FALSE, sep = '\t', col.names = NA)
    }
    if (format == "default")
    {
        # Realize a capture of the dataframe if format isn't selected
        capture.output(dat, file = name_file)
    }
}