#' @export
# Cette fonction va permettre de calculer l'overlap entre les deux fichiers selon la fréquence choisit
overlap_number = function(bedfile1, bedfile2, frequency =0)
{
    intersections = genome_intersect(bedfile1, bedfile2, frequency)
    return(nrow(intersections))
}