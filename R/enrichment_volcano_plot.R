#' @title Enrichment volcano plot
#' @author Martin Mestdagh
#' @description Creates a volcanoplot from the enrichment.
#'  
#' @param enrich The file enrichment from which the plot will be create.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y axis, based on a quantile. 
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#' with personnal color or RColorBrewer palette.
#' @export
EnrichmentVolcanoPlot <- function(enrich,
                        lengthData = max(enrich$q.significance),
                        main = c("Volcanoplot of category"),
                        sigDisplayQuantile = 0.95,
                        aRisk = 0.05,
                        col = c("#6699ff","#ff5050")) {
    # Create a gradient stain.
    colorFunction <- c(colorRampPalette(col)(length(enrich$q.significance)))
    
    
    y <- enrich$q.significance
    yMax <- quantile(x = y, probs = sigDisplayQuantile)
    outsiders <- y > yMax
    y[outsiders] <- yMax 
    
    pch <- rep(x = 19, length.out=nrow(enrich))
    pch[outsiders] <- 6
    
    # Create a volcanoplot-like.
    plot(x    = enrich$effect.size,
         y    = y,
         ylim = c(0,yMax),
         main = main,
         xlab = "Effect size",
         ylab = "Significance",
         col  =  c(1:15),
         pch  = pch,
         cex  = 0.8)
    # Convert alpha risk from p-value to significance.
    aSignificance <- ( -10 * log10(aRisk))
    if (!is.finite(aSignificance))
        stop("The alpha risk is too small to be computed.")
    
    # Add a line that shows the alpha risk.
    abline(v = aSignificance, h = 0, col = "red", lty = 5)
    mtext(bquote(alpha == .(aSignificance)), side = 4, at = aSignificance, col = "red")
}



