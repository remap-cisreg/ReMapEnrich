#'  Enrichment bar plot
#'  
#'  Creates a barplot from the enrichment.
#'  
#'  @param enrich The enrichment data frame from which the plot will be created.
#'  @param lengthData The number of category for the plot.
#'  @param aRisk The alpha risk, by default 0.05.
#'  
#'  @export
EnrichmentBarPlot <- function(enrich, lengthData = 10 , aRisk = 0.05){
    adjustedSignificance <- enrich$adjusted.significance
    names(adjustedSignificance) <- enrich$category
    sortedAdjustedSignificance <- sort(adjustedSignificance)
    sortedAdjustedSignificance <- 
        sortedAdjustedSignificance[(length(sortedAdjustedSignificance)-lengthData):length(sortedAdjustedSignificance)]
    # Create a gradient stain.
    colorFunction <- colorRampPalette(c("royalblue", "red"))
    # Give a title for the barplot with lengthData.
    titlePlot = c("Significance of first", lengthData, "category")
    # Create barplot with legend.
    barplot(sortedAdjustedSignificance,
            horiz     = TRUE, 
            beside    = TRUE, 
            xlab      = "Significance",
            space     = 0.5,
            width     = 0.5,
            cex.names = 0.8,
            col       = colorFunction(length(sortedAdjustedSignificance)),
            las       = 2,
            main      = titlePlot)
    # Convert alpha risk from p-value to significance.
    aSignificance <- ( -10 * log10(aRisk ))
    if (!is.finite(aSignificance))
        stop("The alpha risk is too small to be computed.")
    # Add a line that shows the alpha risk.
    abline(v = aSignificance, lty = 5)
    mtext(bquote(alpha == .(aSignificance)), side = 3, at = aSignificance, col = "red")
}

#'  Enrichment volcano plot
#'  
#'  Creates a volcanoplot from the enrichment.
#'  
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param aRisk The alpha risk, by default 0.05.
#'  
#'  @export
EnrichmentVolcanoPlot <- function(enrich, aRisk = 0.05){
    # Create a gradient stain.
    colorFunction <- colorRampPalette(c("red", "royalblue"))
    matrixVolcano <- matrix(nrow = length(enrich$category), ncol = 2)
    effectsSize <- log(enrich$random.average/enrich$adjusted.significance,
                        base = 2)
    matrixVolcano[, 1] <- effectsSize
    matrixVolcano[, 2] <- enrich$adjusted.significance
    matrixVolcano[complete.cases(matrixVolcano*0), drop=FALSE]
    # Create a volcanoplot-like.
    plot(matrixVolcano,
         xlab = "Effect size",
         ylab = "Significance",
         main = "Volcano plot",
         pch  = 19,
         col  =  colorFunction(length(enrich$significance)))
    # Convert alpha risk from p-value to significance.
    aSignificance <- ( -10 * log10(aRisk))
    if (!is.finite(aSignificance))
        stop("The alpha risk is too small to be computed.")
    # Add a line that shows the alpha risk.
    abline(v = aSignificance, h = 0, col = "red", lty = 5)
    mtext(bquote(alpha == .(aSignificance)), side = 4, at = aSignificance, col = "red")
}






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