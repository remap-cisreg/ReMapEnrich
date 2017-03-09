#' Count overlaps
#' 
#' Gets the number of overlaps for two chromosomic regions sets at a certain frequency.
#' The chromosomic regions are read from bed files.
#' 
#' @param bedfile1 The first bed file.
#' @param bedfile2 The second bed file.
#' @param fraction The fraction that intersections shall exceed to be considered.
#' 
#' @return The number of overlaps for the two sets of chromosomic regions given a frequency.
#' 
#' @export
BedCountOverlaps <- function(bedfile1, bedfile2, fraction = 0.1)
{
    # Calling the function genome_intersect to get a data frame of the overlaps.
    intersections <- BedIntersect(bedfile1, bedfile2, fraction)
    return(nrow(intersections))
}