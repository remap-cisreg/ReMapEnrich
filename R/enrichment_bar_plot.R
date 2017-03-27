#' @title Enrichment bar plot
#' @author Martin Mestdagh
#' @description Creates a barplot from the enrichment.
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param lengthData=10 The number of category for the plot.
#' @param main=c("Significance of first", lengthData, "category"). Allows to choose the title of the plot.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y-axis, based on a quantile.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#' @param sigType="q" Allows to choose between Q-significance or P-significance or E-significance.
#' @param xlim=c(min(enrich[sigType]), max(enrich[sigType])).
#' Change the length of x-axis.
#' @param xlab="Significance" change the label of x-axis.
#' @param horiz=TRUE Flip axis.
#' @param beside=TRUE Juxtaposing bar or not.
#' @param space=0.2 Allows to change size bar.
#' @param cex.names=1 Allows to change size of x-axis (flipped).
#' @param border=NA Allows to change the border of each bar.
#' @param las=1 Allows to change the angle of label y-axis.
#' 
#' @export
EnrichmentBarPlot <- function(enrich, 
                              lengthData = 10,
                              main = c("Significance of first", lengthData, "category"),
                              aRisk = 0.05,
                              sigDisplayQuantile = 0.95,
                              col = c("#6699ff","#ff5050"),
                              sigType = "q",
                              xlim = c(min(enrich[sigType]), max(enrich[sigType])),
                              xlab = sigType,
                              horiz = TRUE, 
                              beside = TRUE, 
                              space = 0.1,
                              cex.names = 0.8,
                              border = NA,
                              las = 1,
                              ...) {
    
    sigType = paste(sigType,".significance",sep = "")
    
    # Creation of matrix with column adapted and with the length selected.
    SignificanceEnrichment <- enrich[,sigType]
    names(SignificanceEnrichment) <- enrich$category
    SignificanceEnrichment <- sort(SignificanceEnrichment)
    SignificanceEnrichment <- 
        SignificanceEnrichment[(length(SignificanceEnrichment)-lengthData):length(SignificanceEnrichment)]

    # Create the coloring palette
    # (Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such as brewer.pal(5,"Reds").
    colorFunction <- paste(colorRampPalette(col)(lengthData+1))

    #Create the ymax with sigdisplayQuantile.
    y <- enrich[,sigType]
    yMax <- quantile(x = y, probs = sigDisplayQuantile)
    outsiders <- y > yMax
    y[outsiders] <- yMax
    
    barplot(SignificanceEnrichment,
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