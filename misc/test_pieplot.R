#'  Create a pie from the enrichment
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param lengthData The number of category for the plot.
#'  @export
EnrichmentPiePlot <- function(enrich, lengthData = 10)
{
    dataPie <- enrich$nb.overlaps
    names(dataPie) <- enrich$category
    dataPie <- dataPie[(length(dataPie)-lengthData):length(dataPie)]
    labelPie <- names(dataPie)
    percentPie <- round(dataPie/sum(enrich$nb.overlaps)*100)
    labelPie <- paste(labelPie, percentPie)
    labelPie <- paste(labelPie, "%", sep="")
    colorFunction <- colorRampPalette(c("royalblue", "red"))
    pie(dataPie, col <- colorFunction(length(dataPie)))
}

