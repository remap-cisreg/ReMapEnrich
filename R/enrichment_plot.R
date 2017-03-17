#'  @title Enrichment bar plot
#'  @author Martin Mestdagh
#'  @description Creates a barplot from the enrichment.
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param lengthData=10 The number of category for the plot.
#'  @param aRisk=0.05 The alpha risk, by default 0.05.
#'  @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#'  Y axis, based on a quantile.
#'  @param col="Dark2" Allows you to use colorBrewer palettes.
#'  @export
EnrichmentBarPlot <- function(enrich, 
                              lengthData = 20,
                              aRisk = 0.05,
                              col = "Dark2") {
    
    adjustedSignificance <- enrich$adjusted.significance
    names(adjustedSignificance) <- enrich$category
    sortedAdjustedSignificance <- sort(adjustedSignificance)
    sortedAdjustedSignificance <- 
        sortedAdjustedSignificance[(length(sortedAdjustedSignificance)-lengthData):length(sortedAdjustedSignificance)]

    # Create a gradient stain.
    colorFunction <- c(colorRampPalette(brewer.pal(8, col))(length(sortedAdjustedSignificance)))
    # Give a title for the barplot with lengthData.
    titlePlot = c("Significance of first", lengthData, "category")
    # Create barplot with legend.
    barplot(sortedAdjustedSignificance,
            main      = titlePlot,
            xlab      = "Significance",
            col       = colorFunction,
            horiz     = TRUE, 
            beside    = TRUE, 
            space     = 0.5,
            width     = 0.5,
            cex.names = 0.8,
            las       = 2
            )
    
    # Convert alpha risk from p-value to significance.
    aSignificance <- ( -10 * log10(aRisk ))
    if (!is.finite(aSignificance))
        stop("The alpha risk is too small to be computed.")
    # Add a line that shows the alpha risk.
    abline(v = aSignificance, lty = 5)
    mtext(bquote(alpha == .(aSignificance)), side = 3, at = aSignificance, col = "red")
}

#'  @title Enrichment volcano plot
#'  @author Martin Mestdagh
#'  @description Creates a volcanoplot from the enrichment.
#'  
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param aRisk=0.05 The alpha risk, by default 0.05.
#'  @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#'  Y axis, based on a quantile. 
#'  @param col="Dark2" Allows you to use colorBrewer palettes.
#'
#'  @export
EnrichmentVolcanoPlot <- function(enrich,
                                  sigDisplayQuantile = 0.95,
                                  aRisk = 0.05,
                                  col = "Dark2") {
    # Create a gradient stain.
    colorFunction <- c(colorRampPalette(brewer.pal(8, col))(length(enrich$adjusted.significance)))
    effectsSize <- log(enrich$random.average/enrich$nb.overlaps,
                        base = 2)

    y <- enrich$adjusted.significance
    yMax <- quantile(x = y, probs = sigDisplayQuantile)
    outsiders <- y > yMax
    y[outsiders] <- yMax 
    
    pch <- rep(x = 19, length.out=nrow(enrich))
    pch[outsiders] <- 6
    
    # Create a volcanoplot-like.
    plot(x    = effectsSize,
         y    = y,
         ylim=c(0,yMax),
         main = "Volcano plot",
         xlab = "Effect size",
         ylab = "Significance",
         col  =  colorFunction,
         pch  = pch,
         cex  = 0.5)
    # Convert alpha risk from p-value to significance.
    aSignificance <- ( -10 * log10(aRisk))
    if (!is.finite(aSignificance))
        stop("The alpha risk is too small to be computed.")
    # Add a line that shows the alpha risk.
    abline(v = aSignificance, h = 0, col = "red", lty = 5)
    mtext(bquote(alpha == .(aSignificance)), side = 4, at = aSignificance, col = "red")
}




#-----------------------Suspendu---------------------------------

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
    percentPie <- round(dataPie/sum(dataPie)*100)
    labelPie <- paste(labelPie, percentPie)
    labelPie <- paste(labelPie, "%", sep="")
    colorFunction <- colorRampPalette(c("royalblue", "red"))
    titlePie <- c("Percent of overlap of first", lengthData, "category")
    pie(dataPie,
        main   = titlePie,
        col    = colorFunction(length(dataPie)),
        labels = labelPie)
}