#' @title Enrichment dot plot
#' @author Martin Mestdagh
#' @description Creates a dotplot from the enrichment(category=f(q.significance))
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param lengthData=10 The number of category for the plot.
#' @param main=c("Significance of first", lengthData, "category"). Allows to choose the title of the plot.
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
#' @export
EnrichmentDotPlot <- function(enrich) {
    
    
    
    
    
    
}