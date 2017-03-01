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
genomic.shuffle = function(regions, genome)
{
    temp.path = tempfile()
    command = paste("shuffleBed -i", regions, "-g", genome, "-chrom >", temp.path)
    system(command)
    shuffle = import.bed(temp.path)
    return(shuffle)
}

genomic.shuffle.temp.file = function(regions, genome)
{
    temp.path = tempfile()
    command = paste("shuffleBed -i", regions, "-g", genome, "-chrom >", temp.path)
    system(command)
    return(temp.path)
}

########################################## bedr version ##########################################

genomic.shuffle.bedr = function(regions, genome)
{
    # The parameters are set in a string to be called in bedtools.
    parameters = paste("-g", genome, "-chrom")
    # Calling bedtools with the shuffle method.
    shuffle = bedr::bedr( input =  list(i = regions),
                          method = "shuffle", 
                          params = parameters
    )
    return(shuffle)
}