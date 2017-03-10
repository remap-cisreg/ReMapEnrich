#' @export
GetChromFile <- function(genome)
{
    return(system.file("extdata", paste(genome, ".genome", sep = ""), package = "roken"))
}

#' @export
GetChromSizes <- function(genome)
{
    chromFile = GetChromFile(genome)
    chromSizes <- read.table(chromFile, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = "", row.names = 1)
    return(chromSizes)
}