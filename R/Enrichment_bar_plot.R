#'  @title Enrichment bar plot
#'  @author Martin Mestdagh
#'  @description Creates a barplot from the enrichment with ggplot
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param lengthData=10 The number of category for the plot.
#'  @param coloration="#FEE0D2","#FC9272" Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#'  @export
EnrichmentBarPlot <- function(enrich,
                           lengthData = 10, 
                           aRisk = 0.05, 
                           coloration = c("#FEE0D2","#FC9272")) {
    #Give the title with the chosen length.
    titlePlot = paste("Significance of first", lengthData, "category")
    
    # Sort the dataframe by q.significance decreasing.
    enrich <- enrich[order(enrich$q.significance, decreasing = TRUE),]
    enrich$category <- factor(enrich$category, 
                              levels = enrich$category[order(enrich$q.significance)])
    
    # Transforms the dataframe with the selected size.
    enrich <- enrich[1:lengthData,]
    
    # Creation of the coloring palette
    # (Personnal coloration such as "#FEE0D2","#FC9272" or a RColorBrewer such as brewer.pal(5,"Reds))
    colorFunction <- paste(colorRampPalette(coloration)(lengthData))
    
    # Calculate the nex alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 4)
    
    # Creation of the legend text.
    legendAlphaRisk <- paste("alpha risk", " = ", sigAlpha)
    
    # Create the plot.
    barPlot <- ggplot(enrich, aes(enrich$category, enrich$q.significance))
    # Add Bar pour different category, and the coloring chooses.
    barPlot <- barPlot + geom_bar(stat = "identity", fill = colorFunction)
    # Reverse axis.
    barPlot <- barPlot + coord_flip()
    # Transform the background for a better visualization.
    barPlot <- barPlot + theme_classic()
    # Give the title and name axis.
    barPlot <- barPlot + ggtitle(titlePlot)
    barPlot <- barPlot + xlab("")
    barPlot <- barPlot + ylab("Significance")
    # Add the line of alpha risk, color it and make the legend of it.
    barPlot <- barPlot + geom_hline(aes(yintercept = sigAlpha, colour = legendAlphaRisk),  linetype = "longdash")
    barPlot <- barPlot + scale_color_manual("", breaks = legendAlphaRisk, values = "Red")
    barPlot <- barPlot + theme(legend.position = "bottom")
    return(barPlot)
    }