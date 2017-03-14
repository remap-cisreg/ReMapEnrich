#'  Enrichment plot
#'  
#'  Create a barplot or a volcanoplot (chosen by user) from the enrichment
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param lengthdata The number of category for the plot.
#'  @param aRisk The alpha risk, by default 0.05.
#'  @export
EnrichmentBarPlot <- function(enrich, lengthdata = 10 , aRisk = 0.05)
{
    res <- enrich$significance
    names(res) <- enrich$category
    res2 <- sort(res)
    res2 <- res2[(length(res2)-lengthdata):length(res2)]
    # Create a gradient stain.
    colorFunction <- colorRampPalette(c("royalblue", "red"))
    # Create barplot with legend.
    barplot(res2, horiz = TRUE, beside = TRUE, xlab = "Significance",
            space = 0.5, width = 0.5,
            cex.names = 0.8, col = colorFunction(length(res2)), las = 2,
            main= "Significance of  first 10 category")
    aSignificance <- (-10 * log10(aRisk ** (length(enrich$p.value))))
    if (!is.finite(aSignificance))
    {
        stop("The alpha risk is too small to be computed.")
    }
    abline(v = aSignificance, lty = 5)
    
}

EnrichmentVolcanoPlot <- function(enrich, aRisk = 0.05)
{
    # Create a gradient stain.
    colorFunction <- colorRampPalette(c("red", "royalblue"))
    matrixVolcano <- matrix(nrow = length(enrich$category), ncol = 2)
    effects_size <- log(enrich$random.average/enrich$significance, base = 2)
    matrixVolcano[,1] <- effects_size
    matrixVolcano[,2] <- enrich$significance
    # Create a volcanoplot -like.
    plot(matrixVolcano, xlab = "Effect size", ylab = "Significance",
         main = "Volcano plot", pch = 19, col =  colorFunction(length(enrich$significance)))
    # Convert alpha risk from p-value to significance.
    aSignificance <- (-10 * log10(aRisk ** (length(enrich$p.value))))
    if (!is.finite(aSignificance))
    {
        stop("The alpha risk is too small to be computed.")
    }
    # Add a line that shows the alpha risk.
    abline(aSignificance, 0, col = "red", lty = 5)
}
    