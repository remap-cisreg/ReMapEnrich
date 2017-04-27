#' @title Enrichment dot plot
#' @author Martin Mestdagh
#' @description Creates a dotplot from the enrichment(category=f(q.significance))
#'  
#' @param enrich The enrichment data frame from which the plot will be created.
#' @param op=20 The number of category for the plot.
#' @param main=paste("Significance, top", top, "categories"). Allows to choose
#' the title of the plot.
#' @param aRisk=0.05 The alpha risk.
#' @param col=c("#6699ff","#ff5050") Palette of coloration for the plot 
#' Personnal coloration such as c("#FEE0D2","#FC9272") or a RColorBrewer 
#' such as brewer.pal(5,"Reds").
#' @param minCircleSize=0.5 Change the minimum size of circle for the plot.
#' @param inches=1/4 Modify the area of points.
#' @param xlab="Mapped peaks ratio" Allows to change the title of x-axis.
#' @param stainCol=rgb(colorFunction(((sig - minSigLim) /
#' (maxSigLim - minSigLim))), maxColorValue = 255)
#' Changes the color of perimeter of each point. 
#' 
#' @usage EnrichmentDotPlot(enrich, 
#' top = 20,
#' main = paste("Significance, top", top, "categories"),
#' sigType = "q",
#' col = c("#6699ff", "#ff5050"),
#' minCircleSize = 0.5,
#' inches = 1 / 4,
#' xlab    = "Mapped peaks ratio",
#' stainCol = rgb(colorFunction(((sig - minSigLim) / (maxSigLim - minSigLim))),
#'  maxColorValue = 255),
#' ...) 
#' 
#' @examples 
#' data("enrichment_example", package = "roken")
#' EnrichmentDotPlot(enrichment_example)
#' 
#' @export
EnrichmentDotPlot <- function(enrich, 
                              top = 20,
                              main = paste("Significance, top", top, "categories"),
                              sigType = "q",
                              col = c("#6699ff", "#ff5050"),
                              minCircleSize = 0.5,
                              inches = 1 / 4,
                              xlab    = "Mapped peaks ratio",
                              stainCol = rgb(colorFunction(((sig - minSigLim) / 
                                                            (maxSigLim - minSigLim))),
                                             maxColorValue = 255),
                              ...) {
    
    # Create the grid for the plot and his legend.
    defPar <- par(no.readonly = TRUE)
    layout(matrix(c(1,1,2,3),2,2), widths = c(0.8,0.2))
    
    # Allows to take the significance chosen.
    sigTypeTitle <- paste(toupper(sigType), "-significance", sep = "")
    sigType <- paste(sigType, ".significance", sep = "")
    
    
    # Order the dataframe by significance chosen.
    enrich <- enrich [order(enrich[,sigType], decreasing = TRUE),]
    
    # Modify the dataframe with the length chosen and order it.
    enrich <- enrich[1:top,]
    enrich <- enrich [order(enrich[,sigType], decreasing = FALSE),]
   
    # Calculate the minimum and maximum of signifiance to create the legend.
    sig <- enrich[,sigType]
    minSig <- floor(min(sig))
    maxSig <- ceiling(max(sig))
    stepSig <- 10 ** (floor(log10(maxSig)) - 1)
    maxSigLim <- maxSig - (maxSig %% stepSig) + stepSig
    minSigLim <- minSig - (minSig %% stepSig)
    
    # Calculate the minimum and maximum of overlaps to create the legend.
    overlaps <- enrich$nb.overlaps
    minOverlaps <- floor(min(overlaps))
    maxOverlaps <- ceiling(max(overlaps))
    stepOverlaps <- 10 ** (floor(log10(maxOverlaps)) - 1)
    maxOverlapsLim <- maxOverlaps - (maxOverlaps %% stepOverlaps) + 
                      stepOverlaps
    minOverlapsLim <- minOverlaps - (minOverlaps %% stepOverlaps)
    
    
    # Create the coloring palette
    # (Personnal coloration such as c("#FEE0D2", "#FC9272") 
    # or a RColorBrewer such as brewer.pal(5,"Reds").
    colorFunction <- colorRamp(col)

    # Creation of the dot plot.
    par(mar=c(5, 5, 4, 0))
    with(enrich,
         symbols(
            x       = mapped.peaks.ratio,
            y       = 1:top,
            circles = (nb.overlaps - minOverlaps) / (maxOverlapsLim -
                                                    minOverlapsLim) + 
                                                    minCircleSize,
            inches  = inches,
            bg      = rgb(colorFunction(((sig - minSigLim) / (maxSigLim - 
                                                              minSigLim))),
                          maxColorValue = 255),
            fg      = stainCol,
            main    = main,
            xlab    = xlab,
            ylab    = "",
            las     = 1,
            yaxt    ='n',
            ...
            )
         )
    cols = rgb(colorFunction( ((sig - minSigLim) / (maxSigLim - minSigLim))),
               maxColorValue = 255)
    for (i in 1:top) {
        abline(h = i, lty = 5, lwd = 0.5, col = cols[i])
    }
    
    # Create the axis with name's category.
    axis(2, at = 1:top, labels = enrich$category, las = 1, cex.axis = 0.8)

    par(mar=c(0, 0, 7, 0))
    # Legend of the plot. Gradient coloration by significance.
    colorMatrix <- colorRampPalette(rev(col))
    plot(c(0,1),c(minSigLim, maxSigLim),type = 'n', axes = FALSE, ylab = "",
         xlab = sigTypeTitle)
    text(x = 0.5, y = seq(minSigLim, maxSigLim, l = 5), 
         labels = round(seq(minSigLim, maxSigLim, l = 5)), adj = 0)
    gradientLegend <- as.raster(matrix(colorMatrix(20)), ncol = 1)
    rasterImage(gradientLegend, 0, minSigLim, 0.3, maxSigLim)
    mtext(sigTypeTitle, 3, cex = 0.8)
    
    
    par(mar=c(5, 0, 3, 0))
    # Legend of the plot. Size of plot by number of overlaps.
    nCircles <- 5
    sizes <- seq(minCircleSize, minCircleSize + 1, l = nCircles)
    yCircles <- cumsum((sizes * inches) + minCircleSize ) 
    circles <- data.frame(sizes, yCircles)
    with(circles,
         symbols(
            x = rep(0,nCircles),
            y = yCircles,
            circles = sizes,
            inches  = inches,
            ylab = "",
            xlab = "",
            bty = 'n',
            xaxt = 'n',
            yaxt = 'n',
            fg = "black",
            bg = "black",
            xlim = c( - 0.01, 0.05)
        )
    )
    text(x = 0.02, y = yCircles, labels = round(seq(minOverlapsLim, 
                                                    maxOverlapsLim, l = 5)),
         adj = 0)
    mtext("Number of overlaps", 3, cex = 0.8)

    par(defPar)
}
