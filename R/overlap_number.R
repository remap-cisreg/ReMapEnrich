#' @export
overlap_number = function(bedfile1, bedfile2, frequency =0)
{
    intersections = genome_intersect(bedfile1, bedfile2, frequency)
    return(nrow(intersections))
}