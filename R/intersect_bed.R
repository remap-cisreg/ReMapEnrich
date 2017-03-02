#' Genomic regions intersections
#' 
#' Gets the intersections of two chromosomic regions at a certain fraction.
#' The chromosomic regions are read from bed files.
#' 
#' @param bedfile1 The first bed file.
#' @param bedfile2 The second bed file.
#' @param fraction The fraction that intersections shall exceed to be considered.
#' 
#' @return A data frame (empty if no intersections have been found) containing the intersections and their lengths in base pairs.
#' 
#' @export
intersect.bed = function(bedfile1, bedfile2, fraction = 0)
{
    temp.path = tempfile()
    command = paste("intersectBed -a", bedfile1, "-b", bedfile2, "-f", fraction, ">", temp.path)
    system(command)
    intersections = import.bed(temp.path)
    return(intersections)
}

########################################## bedr version ##########################################

intersect.bedr = function(regions1, regions2, fraction = 0)
{
    # The parameters are set in a string to be called in bedtools.
    parameters = paste("-wo -f", fraction)
    # Calling bedtools with the intersect method.
    intersections = bedr::bedr( input = list(a = regions1, b = regions2),
                                method = "intersect", 
                                params = parameters
    )
    # Testing if the data frame is containing a 7th column before renaming it.
    if (ncol(intersections) >= 7)                         
    {
        colnames(intersections)[7] = "intersect.length"
    }
    return(intersections)
}