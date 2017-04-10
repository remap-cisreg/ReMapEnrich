#' @title Enrichment bar plot
#' @author Martin Mestdagh
#' @description Creates a barplot from the enrichment.
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param top=20 The number of category for the plot.
#' @param main=paste("Significance, top", top, "categories") Allows to choose 
#'  the title of the plot.
#' @param aRisk=0.05 The alpha risk.
#' @param sigDisplayQuantile=0.95 Quantile used to define the maximal value for
#' the Y-axis, based on a quantile.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the plot 
#' Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such 
#'  as brewer.pal(5,"Reds").
#' @param sigType="q" Allows to choose between Q-significance, P-significance
#'  or E-significance.
#' @param xlab=sigTypeTtitle Allows to change the title of x-axis.
#' @param beside=TRUE Juxtaposing bar or not.
#' @param space=0.2 Allows to change size bar.
#' @param cex.names=1 Allows to change size of x-axis (flipped).
#' @param border=NA Allows to change the border of each bar.
#' @param las=1 Allows to change the angle of label y-axis.
#' 
#' @usage EnrichmentBarPlot(enrich, 
#' top = 10,
#' main = paste("Significance, top", top, "categories"),
#' aRisk = 0.05,
#' sigDisplayQuantile = 0.95,
#' col = c("#6699ff", "#ff5050"),
#' sigType = "q",
#' xlab = sigTypeTitle,
#' beside = TRUE, 
#' space = 0.1,
#' cex.names = 0.8,
#' border = NA,
#' las = 1,
#' ...)
#' 
#' @examples 
#' data("enrichment_example", package = "roken")
#' EnrichmentBarPlot(enrichment_example)
#' 
#' @export
EnrichmentBarPlot <- function(enrich, 
                              top = 10,
                              main = paste("Significance, top", top, "categories"),
                              aRisk = 0.05,
                              sigDisplayQuantile = 0.95,
                              col = c("#6699ff", "#ff5050"),
                              sigType = "q",
                              xlab = sigTypeTitle,
                              beside = TRUE, 
                              space = 0.1,
                              cex.names = 0.8,
                              border = NA,
                              las = 1,
                              ...) {
    
    # Creation of the two strings containing the right value 
    # to get from the enrichment.
    sigTypeTitle <- paste(toupper(sigType), "-significance", sep = "")
    sigType = paste(sigType, ".significance", sep = "")
    
    # Creation of matrix with column adapted and with the length selected.
    sig <- vector()
    sig[enrich$category] <- enrich[,sigType]
    sig <- sort(sig)
    sig <- sig[(length(sig) - top) : length(sig)]

    # Create the coloring palette
    colorFunction <- paste(colorRampPalette(col)(top + 1))

    #Create the xmax with sigDisplayQuantile.
    xMax <- quantile(x = sig, probs = sigDisplayQuantile)
    dispSig <- sig
    dispSig[sig > xMax] = xMax

    midPoints <- barplot(dispSig,
                         main      = main,
                         xlab      = xlab,
                         col       = colorFunction,
                         horiz     = TRUE, 
                         beside    = beside, 
                         space     = space,
                         cex.names = cex.names,
                         border    = border,
                         las       = las,
                         xlim = c(0, xMax),
                         ...
                         )
    labSig <- as.character(round(sig[sig >= xMax]))
    TextOutline(y = midPoints[sig >= xMax], x = rep(xMax - (xMax*0.01),
                                                    length(labSig)), 
                adj = 1, labels = labSig, cex = 1.1)
    
    # Calculate the new alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 3)
    
    # Add a line that shows the new alpha risk.
    abline(v = sigAlpha, lty = 5, col = "red")
    mtext(bquote(alpha == .(sigAlpha)), side = 3, at = sigAlpha, col = "red")
}
