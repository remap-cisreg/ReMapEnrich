#' @export
genome_intersect = function(bedfile1, bedfile2, frequency =0)
{
    parameters = paste("-wo -f", frequency)
    dat1 = import_bed(bedfile1)
    dat2 = import_bed(bedfile2)
    intersections = bedr::bedr( input = list(a = dat1, b = dat2), 
                                method = "intersect", 
                                params = parameters
    )
    return(intersections)
}