#' @title Enrichment volcano plot
#' @author Martin Mestdagh
#' @description Creates a volcano plot from the enrichment.
#'  
#' @param enrich The file enrichment from which the plot will be create.
#' @param main=c("Volcanoplot of category") change the plot title.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y axis, based on a quantile. 
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#' with personnal color or RColorBrewer palette.
#' @param sigType="q" Allows to choose between Q-significance or P-significance or E-significance.
#' @param ylim=c(0,yMax) Create the ylim with the quantile selected.
#' @param xlab="Effect size" Allows to change label of x-axis.
#' @param ylab="Significance" Allows to change label of y-axis.
#' @param pch=pch Allows to choose shape of points outside quantile.
#' @param cex=0.8 Allows to choose the diamater of the points.
#' @param las=1 Allows to change the angle of label y-axis.
#' @export
EnrichmentVolcanoPlot <-function(enrich,
                                 main = "Volcano plot",
                                 aRisk = 0.05,
                                 sigDisplayQuantile = 0.95,
                                 col = c("#ff5050", "#6699ff"),
                                 sigType = "q",
                                 ylim = c(0,yMax),
                                 xlab = "Effect size",
                                 ylab = sigTypeTitle,
                                 pch = pch,
                                 cex = 0.8,
                                 las = 1,
                                 ...) {
   
    
   sigTypeTitle <- paste(toupper(sigType), "-significance", sep = "")     
   sigType = paste(sigType, ".significance", sep = "")
    
   
   # Sort the dataframe by sigType decreasing.
   enrich <- enrich[order(enrich[,sigType], decreasing = TRUE),]
   enrich$category <- factor(enrich$category, 
                             levels = enrich$category[order(enrich[,sigType])])
   
   # Create the coloring palette.
   colorFunction <- c(colorRampPalette(col)(length(enrich$category)))
    
   #Create the ymax with sigdisplayQuantile.
   y <- enrich[,sigType]
   yMax <- quantile(x = y, probs = sigDisplayQuantile)
   outsiders <- y > yMax
   y[outsiders] <- yMax 
    
   # Transform point outside quantile with triangle.
   pch <- rep(x = 19, length.out=nrow(enrich))
   pch[outsiders] <- 17
   
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
   sigAlpha <- - log10(aRisk)
   sigAlpha <- round(sigAlpha, 3)
    
   # Add a line that shows the alpha risk.
   abline(h = sigAlpha, lty = 5, col = "Red")
   mtext(bquote(alpha == .(sigAlpha)), side = 4, at = sigAlpha,las = 3, col = "Red")
}