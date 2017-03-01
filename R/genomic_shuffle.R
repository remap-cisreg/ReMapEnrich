#' Genomic regions shuffle
#' 
#' Shuffle genomic regions among a defined genome.
#' 
#' @param bedfile The bed file containing the genomic regions to shuffle.
#' @param genome A file containing all the chromosome lengths for the species in consideration.
#' 
#' @return A data frame containing the new shuffled chromosic regions.
#' 
#' @export
genomic_shuffle = function(genomic_regions, genome)
{
    # The parameters are set in a string to be called in bedtools.
    parameters = paste("-g", genome, "-chrom")
    # Calling bedtools with the shuffle method.
    shuffle = bedr::bedr( input = list(i = genomic_regions), 
                          method = "shuffle", 
                          params = parameters
    )
    return(shuffle)
}