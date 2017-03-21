#'  @title Enrichment bar plot
#'  @author Martin Mestdagh
#'  @description Creates a barplot from the enrichment.
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param lengthData=10 The number of category for the plot.
#'  @param aRisk=0.05 The alpha risk, by default 0.05.
#'  @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#'  Y axis, based on a quantile.
#'  @param coloration="Accent" Allows you to use colorBrewer palettes.
#'  @export
EnrichmentPlot <- function(enrich, lengthData = 10, aRisk = 0.05) {
    #Give the title with the godd numbers of nrows
    titlePlot = c("Significance of first", lengthData, "category")
    
    # Sort the dataframe by adjusted.significance.
    enrich <- enrich[order(enrich$adjusted.significance, decreasing = TRUE),]
    
    # Allow to choose the length of the data.
    enrich <- enrich[1:lengthData,]
    barPlot <- ggplot(enrich, aes(enrich$category, enrich$adjusted.significance))
             + geom_bar()
             + coord_flip()
             + theme_classic()
             + xlab = ""
             + ylab = "Significance"
             + ggtitle = titlePlot
    return (barPlot)
    }