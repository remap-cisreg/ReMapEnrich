#' @export
enrichment = function(regions, catalog, genome, frequency = 0, shuffles = 100)
{
    nb.overlaps = overlaps_number(genomic.regions, catalog, frequency)
    nb.random_overlaps = 0
    for(i in shuffles)
    {
        shuffle = genomic.shuffle(regions, genome)
        nb.random.overlaps = nb.random.overlaps + overlaps.number(shuffle, catalog, frequency)
    }
    mean.th = nb.random.overlaps / shuffles
    significance = ppois(nb.random.overlaps - 1, lambda = mean_th, lower.tail = FALSE, log = TRUE) / log(10)
    p.value = 10 ** significance
    significance = - significance
    
    if(mean.th == 0)
    {
        p.value = NA
        significance = NA
    }
    enrich.tab = data.frame(n.overlaps, mean.th, p.value, significance)
    return(enrich.tab)
}