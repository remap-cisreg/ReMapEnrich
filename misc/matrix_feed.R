GetLambda <- function (length, size, shuffles, catalog, category) {
    lambda = 0
    for (i in shuffles){
        regions <- GenRegions(length, size)
        lambda <- lambda + GrIntersect(regions, catalog, categories = category)[[1]]
    }
    return(lambda / shuffles)
}

##
nLength <- 5
lengthPower <- 8
nSize <- 5
sizePower <- 8
shuffles <- 1
##
catalog <- BedToGranges("inst/extdata/ReMap_nrPeaks_public_chr22.bed")
categories <- unique(catalog@elementMetadata$id)
matrices <- list()
##
i <- 0
for (cat in categories) {
    i <- i + 1
    cat("\r", i, "/", length(categories))
    flush.console()
    matrix <- matrix(nrow = nLength, ncol = nSize)
    for (row in 1:nLength) {
        for (col in 1:nSize) {
            matrix[row, col] <- GetLambda(length = row ** lengthPower,
                                         size = col ** sizePower,
                                         shuffles = shuffles,
                                         catalog = catalog[catalog@elementMetadata$id == cat],
                                         category = cat)
        }
    }
    rownames(matrix) <- c(1:nLength) ** lengthPower
    colnames(matrix) <- c(1:nSize) ** sizePower
    matrices[[cat]] <- matrix
}