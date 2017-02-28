#' @export
genome_intersect = function(bedfile1, bedfile2)
{
    dat1 = import_bed(bedfile1)
    dat2 = import_bed(bedfile2)
    intersections = bedr::bedr( input = list(a = dat1, b = dat2), 
                                method = "intersect", 
                                params = "-wo"
    )
    return(intersections)
}