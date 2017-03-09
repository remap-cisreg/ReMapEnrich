#' @export
shuffle.gr = function(regions, genome.file)
{
    genome = as.data.frame(data.table::fread(genome.file, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = ""))
    random.gr = GRanges()
    loop.size = length(regions)
    for(i in 1:loop.size)
    {
        size = regions[i]@ranges@width
        chrom = regions[i]@seqnames@values
        chromsize = genome[genome$V1 %in% chrom, ][[2]]
        corr.chromsize = chromsize - size
        random.start = sample(corr.chromsize, 1)
        random.gr = c(random.gr, GRanges(chrom, IRanges(start = random.start, width = size)))
    }
    return(random.gr)
}