#' @title Enrichment bar plot
#' @author Martin Mestdagh
#' @description Creates a barplot from the enrichment(category=f(q.significance))
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
EnrichmentBarPlot <- function(enrich, 
                              lengthData = 10,
                              main = c("Significance of first", lengthData, "category"),
                              aRisk = 0.05,
                              sigDisplayQuantile = 0.95,
                              col = c("#6699ff","#ff5050"),
                              xlim = c(min(enrich$q.significance), max(enrich$q.significance)),
                              xlab = "Significance",
                              horiz = TRUE, 
                              beside = TRUE, 
                              space = 0.1,
                              cex.names = 0.8,
                              border = NA,
                              las = 1,
                              ...) {
    
    # Creation of matrix with column adapted and with the length selected.
    qSignificanceEnrichment <- enrich$q.significance
    names(qSignificanceEnrichment) <- enrich$category
    qSignificanceEnrichment <- sort(qSignificanceEnrichment)
    qSignificanceEnrichment <- 
        qSignificanceEnrichment[(length(qSignificanceEnrichment)-lengthData):length(qSignificanceEnrichment)]

    # Create the coloring palette
    # (Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such as brewer.pal(5,"Reds").
    colorFunction <- paste(colorRampPalette(col)(lengthData+1))

    
    #Create the ymax with sigdisplayQuantile.
    y <- enrich$q.significance
    yMax <- quantile(x = y, probs = sigDisplayQuantile)
    outsiders <- y > yMax
    y[outsiders] <- yMax
    
    barplot(qSignificanceEnrichment,
            main      = main,
            xlab      = xlab,
            col       = colorFunction,
            xlim      = xlim,
            horiz     = horiz, 
            beside    = beside, 
            space     = space,
            cex.names = cex.names,
            border    = border,
            las       = las,
            ...
            )
    
    # Calculate the new alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 3)
    
    # Add a line that shows the new alpha risk.
    abline(v = sigAlpha, lty = 5)
    mtext(bquote(alpha == .(sigAlpha)), side = 3, at = sigAlpha, col = "red")
}