#' @export
get.genome = function(genome)
{
    return(system.file("extdata", paste(genome, ".genome", sep = ""), package = "roken"))
}

#' @export
roken.load.examples = function()
{
    mini.regions <<- system.file("extdata", "mini_regions.bed", package = "roken")
    mini.catalog <<- system.file("extdata", "mini_catalog.bed", package = "roken")
    mini.genome <<- system.file("extdata", "mini_genome.genome", package = "roken")
    cat("Three environnement variables have been created, containing three paths to small files.\n")
    cat("'mini.catalog' contains the catalog.\n")
    cat("'mini.regions' contains the regions.\n")
    cat("'mini.genome' contains the genome.\n")
}