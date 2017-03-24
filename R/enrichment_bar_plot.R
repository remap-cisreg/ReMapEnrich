#' @title Enrichment bar plot
#' @author Martin Mestdagh
#' @description Creates a barplot from the enrichment.
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param lengthData=10 The number of category for the plot.
#' @param main=c("Significance of first", lengthData, "category"). Allows to choose the title of the plot.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y axis, based on a quantile.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#' @export
EnrichmentBarPlot <- function(enrich, 
                              lengthData = 20,
                              main = c("Significance of first", lengthData, "category"),
                              sigDisplayQuantile = 0.95,
                              xlim=c(min(enrich$q.significance), max(enrich$q.significance)),
                              aRisk = 0.05,
                              col = c("#6699ff","#ff5050")) {
    
    # Creation of matrix with column adapted and with the length selected.
    qSignificanceEnrichment <- enrich$q.significance
    names(qSignificanceEnrichment) <- enrich$category
    qSignificanceEnrichment <- sort(qSignificanceEnrichment)
    qSignificanceEnrichment <- 
        qSignificanceEnrichment[(length(qSignificanceEnrichment)-lengthData):length(qSignificanceEnrichment)]

    # Create the coloring palette.
    # (Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such as brewer.pal(5,"Reds").
    colorFunction <- paste(colorRampPalette(col)(lengthData+1))

      # Create barplot with legend.
    barplot(qSignificanceEnrichment,
            main      = main,
            xlab      = "Significance",
            col       = colorFunction,
            xlim      = xlim,
            horiz     = TRUE, 
            beside    = TRUE, 
            space     = 0.5,
            width     = 0.5,
            cex.names = 1,
            las       = 2
            )
    
    # Calculate the new alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 4)
    
    # Add a line that shows the alpha risk.
    abline(v = sigAlpha, lty = 5)
    mtext(bquote(alpha == .(sigAlpha)), side = 3, at = sigAlpha, col = "red")
}


#-----------------------Suspendu---------------------------------

#'  Create a pie from the enrichment
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param lengthData The number of category for the plot.
#'  @export
PiePlot <- function(enrich, lengthData = 10,coloration = "Reds")
{
    dataPie <- enrich$nb.overlaps
    names(dataPie) <- enrich$category
    dataPie <- dataPie[(length(dataPie)-lengthData):length(dataPie)]
    labelPie <- names(dataPie)
    percentPie <- round(dataPie/sum(dataPie)*100)
    labelPie <- paste(labelPie, percentPie)
    labelPie <- paste(labelPie, "%", sep="")
    colorFunction <- colorRampPalette(c("royalblue", "red"))
    titlePie <- c("Percent of overlap of first", lengthData, "category")
    pie(dataPie,
        main   = titlePie,
        col    = coloration,
        labels = labelPie)
}