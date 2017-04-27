#' @title Text outline
#' @author Zacharie Menetrier
#' @description Draw text with an outline.
#' 
#' @param x,y numeric vectors of coordinates where the text
#' labels should be written.
#' @param labels a character vector or expression specifying the text 
#' to be written.
#' @param col The color to be used for the front text.
#' @param outline The color to be used for the outline.
#' 
#' @usage TextOutline <- function(x, y, labels, col = 'white',
#'                                outline = 'black', ... )
#' 
TextOutline <- function(x, y, labels, col = 'white', outline = 'black', ... ) {
    
    # Calculating the spherical offset.
    theta <- seq(pi / 4, 2 * pi, length.out = 8)
    # The radius at which draw the outline.
    r <- 0.1
    
    xy <- xy.coords(x, y)
    xo <- r * strwidth('A')
    yo <- r * strheight('A')
    
    # Draw the outline in a loop.
    for (i in theta) {
        text( xy$x + cos(i) * xo, xy$y + sin(i) * yo, labels, 
              col = outline, ... )
    }
    # Draw the front text.
    text(xy$x, xy$y, labels, col=col, ... ) 
}
