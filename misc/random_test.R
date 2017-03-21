# Create a temporary directory for the tests
testDir <- "~/roken_demo/random_test"
dir.create(testDir, recursive = TRUE, showWarnings = FALSE)
setwd(testDir)

## Download and load the ReMap catalog
catalog <- LoadRemapCatalog()
## Generate random regions
query <- GenRegions(n=10000, size = 200)

## Quick test: generate random regions fot the reference
catalog <- GenRegions(n=50000, size = 500)
catalog@elementMetadata$id <- "rand"


# query <- BedToGranges("big_data/ENCFF001VCU.bed")
# catalog <- BedToGranges("big_data/nrPeaks_all.bed")

## Instantiate result table
iterations <- 1000
result <- data.frame()
# resultNames <- c("pval", "lambda", "intersections")
# result <- data.frame(matrix(ncol=length(resultNames), nrow = iterations))
# names(result) <- resultNames
for (i in 1:iterations) {
    message("Shuffling test, iteration\t", i)
    enrichment <- GrEnrichment(GrShuffle(query), catalog)
    result <- rbind(result, enrichment)
    # pVals <- c(pVals, enrichment$adjusted.p.value)
    # lambdas <- c(lambdas, enrichment$random.average)
}


for (category in unique(result$category)) {
    cat.result <- result[result$category==category,]

    # estimate the lambda as the mean of overlaps over all iterations
    mean.nb.overlaps <- mean(cat.result$nb.overlaps)
    
    # Alternative: estimae the lambda as the mean of all the values for all iterations, where each lambda was estimated by 6 random shuffles of the query
    mean.lambda <- mean(cat.result$random.average)
    
    ## Draw the empirical density of overlaps
    max.x <- max(cat.result$nb.overlaps)
    h <- hist(cat.result$nb.overlaps, breaks = 0:(max.x+1), 
              col="grey", border = "grey")
    
    ## Fit a Poisson onto the empirical density
    x.values <- 0:max.x
    exp.overlaps <- dpois(x = x.values, lambda = mean.nb.overlaps) * sum(h$counts)
    lines(x.values, exp.overlaps, col="blue")
    
    ## Goodness of fit test with a chi2 conformity test.
    ## Before this, we need to merge the tails of the theoretical distribution in order to meet 
    ## the required condition for chi2 test: all expected values must be > 5. 
    
    at.least.5 <- which(exp.overlaps >5)
    left.merge <- 1:(at.least.5[1]-1)
    right.merge <- (at.least.5[length(at.least.5)]+1):length(exp.overlaps)
    obs <- h$counts
    exp <- exp.overlaps
    obs.for.chi2 <- c(sum(obs[left.merge]), obs[at.least.5], sum(obs[right.merge]))
    exp.for.chi2 <- c(sum(exp[left.merge]), exp[at.least.5], sum(exp[right.merge]))
    
    ## Run the chi2 test
    chisq.test(obs.for.chi2, p = exp.for.chi2 / sum(exp.for.chi2))
    
    ## Manual computation of the chi2 stat, to make sure we handle it correctly
    chi2.obs <- sum((exp.for.chi2 - obs.for.chi2)^2/exp.for.chi2)
    chi2.df <- length(exp.for.chi2) -1
    chi2.pval <- pchisq(q=chi2.obs, df=chi2.df , lower.tail = FALSE)
    
    ## To do: collect a summary table with the stats for each catalog entry: 
    ## - category
    ## - nb of peaks for cateogory,
    ## - mean peak size for cat
    ## - median peak size for cat
    ## - mean.nb.overlap
    ## - min.nb.overlap
    ## - max.nb.overlaps
    ## - chi2.obs
    ## - chi2.df
    ## - chi2.pval
    
    
}
