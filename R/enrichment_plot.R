#'  Enrichment plot
#'  
#'  Create a plot from the enrichment created with the format chosen by user 
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param format.plot The type of the plot.
#'  @param aRisk The alpha risk, by default 0.05.
#'  @export
EnrichmentPlot <- function(enrich, format.plot = 'défault')
#'  
EnrichmentBarPlot <- function(enrich, formatPlot = 'défault', aRisk = 0.05)
{
    res <- enrich$significance
    names(res) <- enrich[,1]
    res2 <- sort(res)
    res2 <- res2[(length(res2)-10):length(res2)]
    if (formatPlot == 'default')
    {
        colorFunction <- colorRampPalette(c("royalblue", "red"))
        barplot(res2, horiz = TRUE, beside = TRUE, xlab = "Significance",
                space = 0.5, width = 0.5,
                cex.names = 0.8, col = colorFunction(length(res2)), las = 2,
                main= "Significance of  first 10 category")
        aSignificance <- (-10 * log10(aRisk ** (length(enrich$p.value))))
        abline(v = aSignificance)
    }
    if (formatPlot == 'volcanoPlot')
    {
        colorFunction <- colorRampPalette(c("red", "royalblue"))
        matrixVolcano <- matrix(nrow = length(enrich$category), ncol = 2)
        effects_size <- log(enrich$random.average/enrich$significance, base = 2)
        matrixVolcano[,1] <- effects_size
        matrixVolcano[,2] <- enrich$significance
        plot(matrixVolcano, xlab = "Effect size", ylab = "Significance",
             main = "Volcanoplot", pch = 19, col =  colorFunction(length(enrich$significance)))
        # Convert alpha risk from p-value to significance.
        aSignificance <- (-10 * log10(aRisk ** (length(enrich$p.value))))
        # Add a line that shows the alpha risk.
        abline(aSignificance, 0, col = "red", lty = 5)
    }
    else
    {
        colorFunction <- colorRampPalette(c("royalblue", "red"))
        barplot(res2, horiz = TRUE, beside = TRUE, xlab = "Significance",
                space = 0.5, width = 0.5,
                cex.names = 0.8, col = colorFunction(length(res2)), las = 2,
                main= "Significance of  first 10 category")
    }
 }