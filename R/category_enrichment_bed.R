#' @export
category.enrichment.bed = function(regions, catalog, genome, fraction = 0, shuffles = 100)
{
    
    ## Comptage des overlaps des catégories
    cat("Computing overlaps.\n")
    overlaps = intersect.bed(catalog, regions, fraction)
    categories = split(overlaps, overlaps$name)
    categories.length = length(categories)
    overlaps.count = list()
    cat(categories.length, "categories have been found.\n")
    for(i in 1:categories.length)
    {
        overlaps.count[categories[[i]]$name[1]] = nrow(categories[[i]])
    }
    
    ## Comptage des overlaps des catégories avec shuffle
    cat("Computing", shuffles,"shuffles.\n")
    shuffle.overlaps.count = list()
    for(i in 1:shuffles)
    {
        cat(i, "\r")
        shuffle = shuffle.bed.temp.file(regions, genome)
        shuffle.overlaps = intersect.bed(catalog, shuffle, fraction)
        shuffle.categories = split(shuffle.overlaps, shuffle.overlaps$name)
        shuffle.categories.length = length(shuffle.categories)
        for(j in 1:shuffle.categories.length)
        {
            name = shuffle.categories[[j]]$name[1]
            if(name %in% names(shuffle.overlaps.count))
            {
                shuffle.overlaps.count[name] = shuffle.overlaps.count[[name]] + nrow(shuffle.categories[[j]])
            }
            else
            {
                shuffle.overlaps.count[name] = nrow(shuffle.categories[[j]])
            }
        }
    }
    ## Résultat final
    cat("Computing output.")
    output = matrix(ncol = 5, nrow = categories.length)
    for(i in 1:categories.length)
    {
        name = categories[[i]]$name[1]
        count = overlaps.count[[name]]
        if(!name %in% names(shuffle.overlaps.count))
        {
            random.count = 0
        }
        else
        {
            random.count = shuffle.overlaps.count[[name]]
        }
        theorical.mean = random.count / shuffles
        if(theorical.mean == 0)
        {
            p.value = NA
            significance = NA
        }
        else
        {
            significance = ppois(count - 1, lambda = theorical.mean, lower.tail = FALSE, log = TRUE) / log(10)
            p.value = 10 ** significance
            significance = - significance
        }
        output[i,] = c(name, count, theorical.mean, p.value, significance)
    }
    colnames(output) = c("category", "nb.overlaps", "random.average", "p.value", "significance")
    output = data.frame(output, stringsAsFactors = FALSE)
    output$p.value = as.numeric(output$p.value)
    output$significance = as.numeric(output$significance) 
    return(output[order(output$significance, decreasing = TRUE),])
}