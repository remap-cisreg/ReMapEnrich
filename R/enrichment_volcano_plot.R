#'  @title Enrichment Volcano plot
#'  @author Martin Mestdagh
#'  @description Creates a Volcano plot from the enrichment with ggplot. Qval-significance=f(effectsize)
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param coloration=c("#ff5050", "#6699ff") Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#'  @param Arisk=0.05 The alpha risk?
#'  @param sigDisplayQuantile=0.95 Quantile used to define the maximal value for the
#'                                 Y axis, based on a quantile. 
#'  @export
EnrichmentVolcanoPlot <- function(enrich,
                                  aRisk = 0.05,
                                  coloration = c("#ff5050", "#6699ff"),
                                  sigDisplayQuantile = 0.95 ) {
    #Give the title with the chosen length.
    titlePlot = paste("Volcano Plot")
    
    # Sort the dataframe by q.significance decreasing.
    enrich <- enrich[order(enrich$q.significance, decreasing = TRUE),]
    enrich$category <- factor(enrich$category, 
                              levels = enrich$category[order(enrich$q.significance)])
   
    # Creation of the coloring palette
    # (Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such as brewer.pal(5,"Reds")
    colorFunction <- paste(colorRampPalette(coloration)(length(enrich$q.significance)))
    
    # Calculate the new alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 4)
    
    # Creation of the legend text.
    legendAlphaRisk <- paste("-log10(alpha risk)", " = ", sigAlpha)
    
    # Separation of point outside the quantile.
    
    y <- enrich$q.significance
    yMax <- quantile(x = y, probs = sigDisplayQuantile)
    outsiders <- y > yMax
    y[outsiders] <- yMax 

    # Create the plot.
    VolcanoPlot <- ggplot2::ggplot(enrich, aes(enrich$effect.size, y))
    #VolcanoPlot <- ggplot(enrich, aes(enrich$effect.size, y[outsiders], group = y[outsiders]))
    VolcanoPlot <- VolcanoPlot + geom_point(stat = "identity", color = colorFunction)
    #VolcanoPlot <- VolcanoPlot + geom_point(stat = "identity", color = "Black", aes(y[outsiders]))
    # Transform the background for a better visualization.
    VolcanoPlot <- VolcanoPlot + theme_minimal()
    # Give the title and name axis.
    VolcanoPlot <- VolcanoPlot + ggtitle(titlePlot)
    VolcanoPlot <- VolcanoPlot + xlab("Effect Size")
    VolcanoPlot <- VolcanoPlot + ylab("Q-Significance")
    # Add the line of alpha risk, color it and make the legend of it.
    VolcanoPlot <- VolcanoPlot + geom_hline(aes(yintercept = sigAlpha, colour = legendAlphaRisk),  linetype = "longdash")
    VolcanoPlot <- VolcanoPlot + scale_color_manual("", breaks = legendAlphaRisk, values = "Red")
    VolcanoPlot <- VolcanoPlot + theme(legend.position = "bottom")
    
    return(VolcanoPlot)
    
    
    
}