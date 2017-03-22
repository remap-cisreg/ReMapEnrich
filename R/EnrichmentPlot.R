#'  @title Enrichment bar plot
#'  @author Martin Mestdagh
#'  @description Creates a barplot from the enrichment.
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param lengthData=10 The number of category for the plot.
#'  @param coloration=Reds
#'  @export
EnrichmentPlot <- function(enrich, lengthData = 10, aRisk = 0.05, coloration = c("#FEE0D2","#FC9272")) {
    #Give the title with the godd numbers of nrows.
    titlePlot = paste("Significance of first", lengthData, "category")
    
    # Sort the dataframe by q.significance decreasing.
    enrich <- enrich[order(enrich$q.significance, decreasing = TRUE),]
    enrich$category <- factor(enrich$category, levels = enrich$category[order(enrich$q.significance)])
    
    # Allow to choose the length of the data.
    enrich <- enrich[1:lengthData,]
    
    # Palette of coloration.
    colorFunction <- paste(colorRampPalette(coloration)(lengthData))
    
    # Calculate the nex alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 4)
    
    # Creation of the legend text.
    legendAlphaRisk <- paste("alpha risk", "=", sigAlpha)
    
    
    barPlot <- ggplot(enrich, aes(enrich$category, enrich$q.significance))
    barPlot <- barPlot + geom_bar(stat = "identity", fill = colorFunction)
    barPlot <- barPlot + coord_flip()
    barPlot <- barPlot + theme_classic()
    barPlot <- barPlot + ggtitle(titlePlot)
    barPlot <- barPlot + xlab("")
    barPlot <- barPlot + ylab("Significance")
    barPlot <- barPlot + geom_hline(aes(yintercept = sigAlpha, colour = legendAlphaRisk),  linetype = "longdash")
    barPlot <- barPlot + scale_color_manual("", breaks = legendAlphaRisk, values = "Red")
    barPlot <- barPlot + theme(legend.position = "bottom")
    return(barPlot)
    }