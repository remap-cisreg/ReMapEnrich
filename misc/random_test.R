#query <- BedToGranges("big_data/ENCFF001VCU.bed")
catalog <- BedToGranges("big_data/nrPeaks_all.bed")

## Instantiate result table
iterations <- 1000
result <- data.frame()
for (i in 1:iterations) {
    cat("\r", i, "/", iterations)
    flush.console()
    overlaps <- GrIntersect(GenRegions(10000,500), catalog)
    df <- data.frame(names(overlaps), overlaps)
    result <- rbind(result, df)
}

rm(df, iterations, overlaps, i)

df.cat <- data.frame()
for (category in unique(result[,1])) {
    cat.result <- result[result[,1] == category,]
    # Estimate the lambda as the mean of overlaps over all iterations.
    mean.nb.overlaps <- mean(cat.result[,2])
    # Draw the empirical density of overlaps.
    max.x <- max(cat.result[,2])
    histogram <- hist(cat.result[,2], breaks = 0:(max.x+1), 
                      col="grey", border = "grey")
    ## Fit a Poisson onto the empirical density
    x.values <- 0:max.x
    exp.overlaps <- dpois(x = x.values, lambda = mean.nb.overlaps) * sum(histogram$counts)
    lines(x.values, exp.overlaps, col="blue")
    ## Goodness of fit test with a chi2 conformity test.
    ## Before this, we need to merge the tails of the theoretical distribution in order to meet 
    ## the required condition for chi2 test: all expected values must be > 5. 
    at.least.5 <- which(exp.overlaps >5)
    if(length(at.least.5) < 1){
        next
    }
    left.merge <- 1:(at.least.5[1]-1)
    right.merge <- (at.least.5[length(at.least.5)]+1):length(exp.overlaps)
    obs <- histogram$counts
    exp <- exp.overlaps
    obs.for.chi2 <- c(sum(obs[left.merge]), obs[at.least.5], sum(obs[right.merge]))
    exp.for.chi2 <- c(sum(exp[left.merge]), exp[at.least.5], sum(exp[right.merge]))
    ## Run the chi2 test
    chisq.test(obs.for.chi2, p = exp.for.chi2 / sum(exp.for.chi2))
    ## Manual computation of the chi2 stat, to make sure we handle it correctly
    chi2.obs <- sum((exp.for.chi2 - obs.for.chi2)^2/exp.for.chi2)
    chi2.df <- length(exp.for.chi2) -1
    chi2.pval <- pchisq(q=chi2.obs, df=chi2.df , lower.tail = FALSE)
    df <- data.frame(category, mean.nb.overlaps, chi2.obs, chi2.df, chi2.pval)
    df.cat <- rbind(df.cat, df)
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
