#' @export
genomic_shuffle = function(bedfile, genome)
{
    parameters = paste("-g", genome, "-chrom")
    genomic_regions = import_bed(bedfile)
    shuffle = bedr::bedr( input = list(i = genomic_regions), 
                          method = "shuffle", 
                          params = parameters
    )
    return(shuffle)
}