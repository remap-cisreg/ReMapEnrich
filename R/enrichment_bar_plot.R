#' @title Enrichment bar plot
#' @author Martin Mestdagh
#' @description Creates a barplot from the enrichment.
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param lengthData=10 The number of category for the plot.
#' @param main=c("Significance of first", lengthData, "category"). Allows to choose the title of the plot.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y axis, based on a quantile.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#' @export
EnrichmentBarPlot <- function(enrich, 
                              lengthData = 20,
                              main = c("Significance of first", lengthData, "category"),
                              sigDisplayQuantile = 0.95,
                              xlim=c(min(enrich$q.significance), max(enrich$q.significance)),
                              aRisk = 0.05,
                              col = c("#6699ff","#ff5050")) {
    
    # Creation of matrix with column adapted and with the length selected.
    qSignificanceEnrichment <- enrich$q.significance
    names(qSignificanceEnrichment) <- enrich$category
    qSignificanceEnrichment <- sort(qSignificanceEnrichment)
    qSignificanceEnrichment <- 
        qSignificanceEnrichment[(length(qSignificanceEnrichment)-lengthData):length(qSignificanceEnrichment)]

    # Create the coloring palette
    colorFunction <- paste(colorRampPalette(col)(lengthData+1))

      # Create barplot with legend.
    barplot(qSignificanceEnrichment,
            main      = main,
            xlab      = "Significance",
            col       = colorFunction,
            xlim      = xlim,
            horiz     = TRUE, 
            beside    = TRUE, 
            space     = 0.5,
            width     = 0.5,
            cex.names = 1,
            las       = 2
            )
    
    # Calculate the new alpha risk.
    sigAlpha <- -log10(aRisk)
    sigAlpha <- round(sigAlpha, 4)
    
    # Add a line that shows the alpha risk.
    abline(v = sigAlpha, lty = 5)
    mtext(bquote(alpha == .(sigAlpha)), side = 3, at = sigAlpha, col = "red")
}

#' @title Enrichment volcano plot
#' @author Martin Mestdagh
#' @description Creates a volcanoplot from the enrichment.
#'  
#' @param enrich The file enrichment from which the plot will be create.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param sigDisplayQuantile=0.95 quantile used to define the maximal value for the
#' Y axis, based on a quantile. 
#' @param coloration="Accent" Allows you to use colorBrewer palettes.
#'
#' @export
VolcanoPlot <- function(enrich,
                                  sigDisplayQuantile = 0.95,
                                  aRisk = 0.05,
                                  coloration = "Accent") {
    # Create a gradient stain.
    colorFunction <- c(colorRampPalette(brewer.pal(8, coloration))(length(enrich$q.significance)))
    

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
         main = "Volcano plot",
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




#-----------------------Suspendu---------------------------------

#'  Create a pie from the enrichment
#'  @param enrich The file enrichment from which the plot will be create.
#'  @param lengthData The number of category for the plot.
#'  @export
PiePlot <- function(enrich, lengthData = 10,coloration = "Reds")
{
    dataPie <- enrich$nb.overlaps
    names(dataPie) <- enrich$category
    dataPie <- dataPie[(length(dataPie)-lengthData):length(dataPie)]
    labelPie <- names(dataPie)
    percentPie <- round(dataPie/sum(dataPie)*100)
    labelPie <- paste(labelPie, percentPie)
    labelPie <- paste(labelPie, "%", sep="")
    colorFunction <- colorRampPalette(c("royalblue", "red"))
    titlePie <- c("Percent of overlap of first", lengthData, "category")
    pie(dataPie,
        main   = titlePie,
        col    = coloration,
        labels = labelPie)
}