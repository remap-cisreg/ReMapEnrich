#' @export
enrichment = function(regions, catalog, genome, fraction = 0, shuffles = 100)
{
    cat("COMPUTING OVERLAPS\n")
    overlaps = count.overlaps(regions, catalog, fraction)
    random.overlaps = 0
    cat(paste("COMPUTING", shuffles, "SHUFFLES\n"))
    for(i in 1:shuffles)
    {
        cat(paste("#",i, "\n"))
        shuffle = shuffle.bed.temp.file(regions, genome)
        random.overlaps = random.overlaps + count.overlaps(shuffle, catalog, fraction)
    }
    theorical.mean = random.overlaps / shuffles
    significance = ppois(random.overlaps - 1, lambda = theorical.mean, lower.tail = FALSE, log = TRUE) / log(10)
    p.value = 10 ** significance
    significance = - significance
    
    if(theorical.mean == 0)
    {
        p.value = NA
        significance = NA
    }
    enrich.tab = data.frame(overlaps, theorical.mean, p.value, significance)
    return(enrich.tab)
}