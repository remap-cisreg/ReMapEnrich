#' Genomic regions intersections
#' 
#' Gets the intersections of two chromosomic regions at a certain frequency.
#' The chromosomic regions are read from bed files.
#' 
#' @param bedfile1 The first bed file.
#' @param bedfile2 The second bed file.
#' @param frequency The frequency that intersections shall exceed to be considered.
#' 
#' @return A data frame (empty if no intersections have been found) containing the intersections and their lengths in base pairs.
#' 
#' @export
genome_intersect = function(bedfile1, bedfile2, frequency = 0)
{
    # The parameters are set in a string to be called in bedtools.
    parameters = paste("-wo -f", frequency)
    # Importing the two bed files.
    genomic_regions_1 = import_bed(bedfile1)
    genomic_regions_2 = import_bed(bedfile2)
    # Calling bedtools with the intersect method.
    intersections = bedr::bedr( input = list(a = genomic_regions_1, b = genomic_regions_2), 
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