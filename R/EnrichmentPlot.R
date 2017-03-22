#'  @title Enrichment bar plot
#'  @author Martin Mestdagh
#'  @description Creates a barplot from the enrichment.
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param lengthData=10 The number of category for the plot.
#'  @param coloration=Reds
#'  @export
EnrichmentPlot <- function(enrich, lengthData = 10, aRisk = 0.05, coloration = "Reds") {
    #Give the title with the godd numbers of nrows.
    titlePlot = paste("Significance of first", lengthData, "category")
    
    # Sort the dataframe by adjusted.significance decreasing.
    enrich <- enrich[order(enrich$adjusted.significance, decreasing = TRUE),]
    enrich$category <- factor(enrich$category, levels = enrich$category[order(enrich$adjusted.significance)])
    
    # Allow to choose the length of the data.
    enrich <- enrich[1:lengthData,]
    
    # Palette of coloration.
    colorFunction <- paste(colorRampPalette(brewer.pal(8, coloration))(length(enrich$adjusted.significance)))
    
    
    barPlot <- ggplot(enrich, aes(enrich$category, enrich$adjusted.significance))
    barPlot <- barPlot + geom_bar(stat = "identity", fill = colorFunction)
    barPlot <- barPlot + coord_flip()
    barPlot <- barPlot + theme_classic()
    barPlot <- barPlot + ggtitle(titlePlot)
    barPlot <- barPlot + xlab("")
    barPlot <- barPlot + ylab("Significance")
    return(barPlot)
    }