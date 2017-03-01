#' Overlaps number
#' 
#' Gets the number of overlaps for two chromosomic regions sets at a certain frequency.
#' The chromosomic regions are read from bed files.
#' 
#' @param bedfile1 The first bed file.
#' @param bedfile2 The second bed file.
#' @param frequency The frequency that intersections shall exceed to be considered.
#' 
#' @return The number of overlaps for the two sets of chromosomic regions given a frequency.
#' 
#' @export
overlaps_number = function(bedfile1, bedfile2, frequency = 0)
{
    # Calling the function genome_intersect to get a data frame of the overlaps.
    intersections = genome_intersect(bedfile1, bedfile2, frequency)
    return(nrow(intersections))
}

