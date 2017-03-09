#' @export
GetChromFile = function(genome)
{
    return(system.file("extdata", paste(genome, ".genome", sep = ""), package = "roken"))
}