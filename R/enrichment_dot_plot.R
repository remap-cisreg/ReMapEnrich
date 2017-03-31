#' @title Enrichment dot plot
#' @author Martin Mestdagh
#' @description Creates a dotplot from the enrichment(category=f(q.significance))
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param top=10 The number of category for the plot.
#' @param main=c("Significance of first", top, "category"). Allows to choose the title of the plot.
#' @param aRisk=0.05 The alpha risk, by default 0.05.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the histogram 
#'  with personnal color or RColorBrewer palette.
#' 
#' 
#' @examples
#' EnrichmentDotPlot(enrich = enrich)
#' @export
EnrichmentDotPlot <- function(enrich, 
                              top = 10,
                              main = paste("Significance, top", top, "categories"),
                              sigType = "q",
                              col = c("#ff5050","#6699ff"),
                              staincol = NULL) {
    
    
    defPar <- par(no.readonly = TRUE)
    layout(matrix(c(1,1,2,3),2,2), widths = c(0.8,0.2))
    # Allows to take the significance chosen.
    sigTypeTitle <- paste(sigType,"-significance",sep = "")
    sigType <- paste(sigType,".significance",sep = "")
    
    
    # Transform the dataframe which the length chosen by user.
    enrich <- enrich [order(enrich[,sigType]),]
    
    # Create a matrix to take the names of category.
    enrich <- enrich[1:top,]
    sig <- enrich[,sigType]
    names(sig) <- enrich$category
    
    
    minSig <- round(min(sig))
    maxSig <- round(max(sig))
    stepSig <- 10 ** (floor(log10(maxSig)) - 1)
    maxSigLim <- maxSig - (maxSig %% stepSig) + stepSig
    minSigLim <- minSig - (minSig %% stepSig)
    
    # Create the coloring palette
    # (Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer such as brewer.pal(5,"Reds").

    colorFunction <- colorRamp(col)

    # Creation of the dot plot.
    with(enrich, symbols(x       = mapped.peaks.ratio,
                                        y       = 1:top,
                                        circles = nb.overlaps,
                                        inches  = 1/4,
                                        bg      = rgb(colorFunction(1 - ((sig - minSigLim) / (maxSigLim - minSigLim))), maxColorValue = 255),
                                        fg      = staincol,
                                        main    = main,
                                        xlab    = "Mapped peaks ratio",
                                        ylab    = "",
                                        las     = 1,
                                        yaxt    ='n'))

    # Create the axis with name's category.
    axis(2, at =1:top, labels = names(sig), las = 1, cex.axis=0.8)

    par(mar=c(0, 0, 4, 0))
    # Legend of the plot.
    colorMatrix <- colorRampPalette(col)
    plot(c(0,2),c(minSigLim,maxSigLim),type = 'n', axes = FALSE, ylab = "", xlab = sigTypeTitle)
    text(x = 0.5, y = seq(minSigLim,maxSigLim, l = 5), labels = round(seq(minSigLim,maxSigLim,l = 5)), adj = 0)
    gradientLegend <- as.raster(matrix(colorMatrix(20)), ncol = 1)
    rasterImage(gradientLegend, 0,minSigLim,0.3,maxSigLim)
    
    par(mar=c(5, 0, 0, 0))
    ## Circles
    nCircles <- 5
    #plot(c(0,1), c(0,1), type ='n',axes = FALSE, xlab = "", ylab = "" , main = "Nb Overlaps")
    minOverlaps <- min(enrich$nb.overlaps)
    maxOverlpas <- max(enrich$nb.overlaps)
    sizes <- seq(minOverlaps,maxOverlpas, l = nCircles)
    yCircles <- c(1:nCircles/nCircles)
    circles <- data.frame(sizes,yCircles)
    with (circles, symbols(
        x = rep(0,nCircles),
        y = yCircles,
        circles = sizes,
        inches  = 1/4,
        ylab = "",
        xlab = "",
        bty = 'n',
        xaxt = 'n',
        yaxt = 'n',
        fg = "black",
        bg = "black",
        xlim = c(-0.01,0.05)
        )
    )
    text(0,0.1,labels = "Number of overlaps")
    
    
    par(defPar)
}