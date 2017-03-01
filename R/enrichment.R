#' @export
enrichment = function(genomic_regions, catalog, genome, frequency = 0.1, shuffle_numbers = 100)
{
    n_overlaps = overlaps_number(genomic_regions, catalog, frequency)
    n_random_overlaps = 0
    for(i in shuffle_numbers)
    {
        shuffle = genomic_shuffle(genomic_regions, genome)
        n_random_overlaps = n_random_overlaps + overlaps_number(shuffle, catalog, frequency)
    }
    mean_th = n_random_overlaps / shuffle_numbers
    significance = ppois(n_random_overlaps - 1, lambda = mean_th, lower.tail = FALSE, log = TRUE) / log(10)
    p_value = 10 ** significance
    significance = - significance
    
    if(mean_th == 0)
    {
        p_value = NA
        significance = NA
    }
    enrich_tab = data.frame(n_overlaps, mean_th, p_value, significance)
    return(enrich_tab)
}