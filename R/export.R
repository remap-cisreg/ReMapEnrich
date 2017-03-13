#' Create an output file with the format chosen by user or a capture like 'txt' if none is selected
#' 
#'  @param dat dataframe to output
#'  @param name.file Name of the output file chosen
#'  @param format The format chosen
#'  @param plot Boolean option T or F if user wants a plot saved
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
    if (format == "default")
    {
        # Realize a capture of the dataframe if format isn't selected
        capture.output(dat, file = fileName)
    }
   if(plot == TRUE)
   {
       res <- dat[,5]
       names(res) <- dat[,1]
       res <- sort(res)
       png("plot.png")
       colfunc <- colorRampPalette(c("royalblue", "red"))
       output.plot <- barplot(res, horiz = TRUE, beside = TRUE,
                              xlab = "significance", ylab = "category", space = 0.5, width = 0.5,
                              col = colfunc(length(res)))
       dev.off()
   }
}