#' Create a plot from the enrichment created with the format chosen by user 
#'  @param enrich The file enrichment from which the create will be create
#'  @param format.plot The type of the plot 
#'      
#'  @export
#'  
EnrichmentPlot <- function(enrich, format.plot = 'défault')
{
    res <- enrich$significance
    names(res) <- enrich[,1]
    res2 <- sort(res)
    res2 <- res2[(length(res2)-10):length(res2)]
    color.function <- colorRampPalette(c("royalblue", "red"))
    if (format.plot == 'défault')
    {
        barplot(res2, horiz = TRUE, beside = TRUE, xlab = "significance log10(Pval)",
                space = 0.5, width = 0.5,
                cex.names = 0.8, col = colfunc(length(res2)), las = 2)
    }
    if (format.plot == 'histogramm')
    {
        
    }
    rm(color.function)
}
