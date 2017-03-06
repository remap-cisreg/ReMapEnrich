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
intersect.bed = function(bedfile1, bedfile2, fraction = 0.1)
{
    temp.path = tempfile()
    command = paste("intersectBed -a", bedfile1, "-b", bedfile2, "-f", fraction, ">", temp.path)
    system(command)
    size = file.info(temp.path)$size
    if(size == 0)
    {
        intersections = data.frame()
    }
    else
    {
        intersections = import.bed(temp.path)   
    }
    unlink(temp.path)
    return(intersections)
}