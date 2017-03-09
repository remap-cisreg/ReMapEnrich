#' @export
GrShuffle <- function(regions, chromFile = GetChromFile("hg19"))
{
    genome <- as.data.frame(data.table::fread(genome.file, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = ""))
}