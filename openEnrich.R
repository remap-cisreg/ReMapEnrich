openEnrich = function(path)
{
    enrich = read.table(path, header = TRUE, skip = 1)
    return(enrich)
}
