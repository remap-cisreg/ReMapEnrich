#' @title Enrichment dot plot
#' @author Martin Mestdagh
#' @description Creates a dotplot from the enrichment(category=f(q.significance))
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param top=10 The number of category for the plot.
#' @param main=c("Significance of first", top, "category"). Allows to choose the title of the plot.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y-axis, based on a quantile.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#' @param xlim=c(min(enrich$q.significance), max(enrich$q.significance)).
#' Change the length of x-axis.
#' @param xlab="Significance" change the label of x-axis.
#' @param horiz=TRUE Flip axis.
#' @param beside=TRUE Juxtaposing bar or not.
#' @param space=0.2 Allows to change size bar.
#' @param cex.names=1 Allows to change size of x-axis (flipped).
#' @param border=NA Allows to change the border of each bar. 
#' 
#' @examples
#' EnrichmentDotPlot(enrich = enrich)
#' @export
EnrichmentDotPlot <- function(enrich, 
                              top = 10,
                              main = paste("Significance, top", top, "categories"),
                              sigType = "q",
                              col = c("#6699ff","#ff5050"),
                              staincol = "Black") {
    
    
    
    layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
    # Allows to take the significance chosen.
    sigTypeTitle <- paste(sigType,"-significance",sep = "")
    sigType <- paste(sigType,".significance",sep = "")
    
    
    # Transform the dataframe which the length chosen by user.
    enrich <- enrich [order(enrich[,sigType], decreasing = TRUE),]
    
    # Create the coloring palette
    # (Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such as brewer.pal(5,"Reds").
    colorFunction <- paste(colorRampPalette(col)(top+1))
    
    # Creation of the dot plot.
    with(enrich[1:top,], symbols(x = mapped.peaks.ratio, 
                                        y = 1:top, 
                                        circles = nb.overlaps, 
                                        inches = 1/4, 
                                        bg = colorFunction, 
                                        fg = staincol,
                                        main = main,
                                        xlab = "Mapped peaks ratio",
                                        ylab = "Category",
                                        las = 1))
 
    
    # Legend of the plot.
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = sigTypeTitle)
    gradientLegend <- as.raster(matrix(colorFunction), ncol = 1)
    rasterImage(gradientLegend, 0,0,0.3,0.3)
}