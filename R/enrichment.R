#' @export
enrichment = function(regions, catalog, genome, fraction = 0, shuffles = 100)
{
    cat("COMPUTING OVERLAPS\n")
    nb.overlaps = overlaps.number(regions, catalog, fraction)
    nb.random.overlaps = 0
    cat(paste("COMPUTING", shuffles, "SHUFFLES\n"))
    for(i in 1:shuffles)
    {
        cat(paste("#",i, "\n"))
        shuffle = genomic.shuffle.temp.file(regions, genome)
        nb.random.overlaps = nb.random.overlaps + overlaps.number(shuffle, catalog, fraction)
    }
    mean.th = nb.random.overlaps / shuffles
    significance = ppois(nb.random.overlaps - 1, lambda = mean.th, lower.tail = FALSE, log = TRUE) / log(10)
    p.value = 10 ** significance
    significance = - significance
    
    if(mean.th == 0)
    {
        p.value = NA
        significance = NA
    }
    enrich.tab = data.frame(nb.overlaps, mean.th, p.value, significance)
    return(enrich.tab)
}