#' @export
category.enrichment.bed = function(regions, catalog, genome, fraction = 0, shuffles = 100)
{
    all.overlaps = intersect.bed(catalog, regions, fraction)
    cat.overlaps = split(all.overlaps, all.overlaps$name)
    cat.length = length(cat.overlaps)
    cat.list = list()
    cat.names = list()
    for(i in 1:cat.length)
    {
        cat.list[[i]] = nrow(cat.overlaps[[i]])
        cat.names[[i]] = cat.overlaps[[i]]$name
    }
    names(cat.list) = cat.names
    return(cat.names)
}