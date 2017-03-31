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
EnrichmentDotPlot <- function(enrich, 
                              lengthData = 10,
                              main = c("Significance of first", lengthData, "category"),
                              sigType = "q",
                              col = c("#ff5050","#6699ff"),
                              staincol = NULL) {
    
    
    defPar <- par(no.readonly = TRUE)
    layout(matrix(c(1,1,2,3),2,2))
    # Allows to take the significance chosen.
    sigTypeTitle <- paste(sigType,"-significance",sep = "")
    sigType <- paste(sigType,".significance",sep = "")
    
    
    # Transform the dataframe which the length chosen by user.
    enrich <- enrich [order(enrich[,sigType], decreasing = TRUE),]
    
    # Create a matrix to take the names of category.
    enrich <- enrich[1:lengthData,]
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
                                        y       = 1:lengthData,
                                        circles = nb.overlaps,
                                        inches  = 1/4,
                                        bg      = rgb(colorFunction((sig - minSigLim) / maxSigLim), maxColorValue = 255),
                                        fg      = staincol,
                                        main    = main,
                                        xlab    = "Mapped peaks ratio",
                                        ylab    = "",
                                        las     = 1,
                                        yaxt    ='n',
                                        ann     =FALSE))

    # Create the axis with name's category.
    axis(2, at =1:lengthData, labels = enrich$category, las = 1, cex.axis=0.8)
    
    # Legend of the plot.
   
    plot(c(0,2),c(minSig,maxSig),type = 'n', axes = FALSE, xlab = "", ylab = "", main = sigTypeTitle)
    text(x = 0.5, y = seq(minSig,maxSig, l = 5), labels = round(seq(minSigLim,maxSigLim,l = 5)), adj = 0)
    gradientLegend <- as.raster(matrix(colorFunction(20)), ncol = 1)
    rasterImage(gradientLegend, 0,minSig,0.25,maxSig)
    #plot(c(1,2), c(0,1), type ='n',axes = FALSE, xlab = "", ylab = "" , main = "Nb Overlaps")
    #minOverlaps <- min(enrich$nb.overlaps)
    #maxOverlpas <- max(enrich$nb.overlaps)
    #legend("topleft", legend = round(seq(minOverlaps,maxOverlpas, l = 5)), pch = 20, cex = 2)
    
    par(defPar)
}