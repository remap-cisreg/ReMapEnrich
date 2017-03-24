#' @title Enrichment volcano plot
#' @author Martin Mestdagh
#' @description Creates a volcanoplot from the enrichment(q.significance=f(effectsize))
#'  
#' @param enrich The file enrichment from which the plot will be create.
#' @param main=c("Volcanoplot of category") change the plot title.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y axis, based on a quantile. 
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#' with personnal color or RColorBrewer palette.
#' @param ylim=c(0,yMax) Create the ylim with the quantile selected.
#' @param xlab="Effect size" Allows to change label of x-axis.
#' @param ylab="Significance" Allows to change label of y-axis.
#' @param pch=pch Allows to choose shape of points outside quantile.
#' @param cex=0.8 Allows to choose the diamater of the points.
#' @export
EnrichmentVolcanoPlot <-function(enrich,
                                 main = c("Volcanoplot of category"),
                                 aRisk = 0.05,
                                 sigDisplayQuantile = 0.95,
                                 col = c("#6699ff","#ff5050"),
                                 ylim = c(0,yMax),
                                 xlab = "Effect size",
                                 ylab = "Significance",
                                 pch = pch,
                                 cex = 0.8,
                                 las = 1,
                                 ...) {
        
        # Sort the dataframe by q.significance decreasing.
        enrich <- enrich[order(enrich$q.significance, decreasing = TRUE),]
        enrich$category <- factor(enrich$category, 
                                  levels = enrich$category[order(enrich$q.significance)])
        # Create a gradient stain.
        colorFunction <- c(colorRampPalette(col)(length(enrich$category)))
        
        #Create the ymax with sigdisplayQuantile.
        y <- enrich$q.significance
        yMax <- quantile(x = y, probs = sigDisplayQuantile)
        outsiders <- y > yMax
        y[outsiders] <- yMax 
        
        # Transform point outside quantile with triangle.
        pch <- rep(x = 19, length.out=nrow(enrich))
        pch[outsiders] <- 6
        
        plot(x    = enrich$effect.size,
             y    = y,
             ylim = ylim,
             main = main,
             xlab = xlab,
             ylab = ylab,
             col  = colorFunction,
             pch  = pch,
             cex  = cex,
             las  = las,
             ...)
        
        # Calculate the new alpha risk.
        sigAlpha <- -log10(aRisk)
        sigAlpha <- round(sigAlpha, 3)
        
        # Add a line that shows the alpha risk.
        abline(h = sigAlpha, lty = 5, col = "Red")
        mtext(bquote(alpha == .(sigAlpha)), side = 4, at = sigAlpha,las = 3, col = "Red")
    }
    
    
    
