#query <- BedToGranges("big_data/ENCFF001VCU.bed")
catalog <- BedToGranges("big_data/nrPeaks_all.bed")
# For JvH: catalog <- BedToGranges("~/roken_demo/data/ReMap/nrPeaks_all.bed")

## Instantiate result table
iterations <- 500
catalog.names <- unique(catalog@elementMetadata$id)
result <- data.frame(matrix(nrow=iterations, ncol=length(catalog.names)))
colnames(result) <- catalog.names
dim(result)
region.nb <- 1000
region.size <- 5000
for (i in 1:iterations) {
    cat("\r", i, "/", iterations)
    flush.console()
    overlaps <- GrIntersect(GenRegions(n = region.nb, size = region.size), catalog)
    result[i, catalog.names] <- overlaps[catalog.names]
#    df <- data.frame(names(overlaps), overlaps)
#    result <- rbind(result, df)
}

rm(iterations, overlaps, i)

df.cat <- data.frame()
category <- catalog.names[1]
for (category in catalog.names) {
    cat.result <- result[,category]
    # Estimate the lambda as the mean of overlaps over all iterations.
    mean.nb.overlaps <- mean(cat.result)
    # Draw the empirical density of overlaps.
    max.x <- max(cat.result)
    h <- hist(cat.result, breaks = 0:(max.x+1), plot=FALSE)

    ## Fit a Poisson onto the empirical density
    x.values <- 0:max.x
    exp.overlaps <- dpois(x = x.values, lambda = mean.nb.overlaps) * sum(histogram$counts)
    
    plot(h$mids-0.5, h$counts, type="h", col="#888888", lwd=3, 
         xlab="overlap", ylab="occurrences")
    abline(h=5, col="red")
    abline(v=mean.nb.overlaps, col="darkgreen")
    exp.colors <- rep(x = "#008800", length.out = length(h$counts))
    exp.colors[exp.overlaps < 5] <- "#FF7777"
    lines(x.values, exp.overlaps, col=exp.colors, type="p", pch="-", lwd=3) # show the oundary for the chi2 assumption
    
    ## Compute cumulative occurrences
    exp.cum <- cumsum(exp.overlaps)
    obs.cum <- cumsum(histogram$counts)

    
    # plot(x.values, obs.cum, type="p", pch=20, ylab="Cumulative occurrences",
    #      xlab="overlaps", panel.first=grid())
    # lines(x.values, exp.cum, type="l", col="blue")
    # abline(v=mean.nb.overlaps)

    
    ## If not a single value has exp >5, group values starting from the median
    if (sum(exp.overlaps > 5) < 1) {
        ## identify the expected (Integer) value cosest to the median
        median.index <- which.min(abs(exp.cum - sum(exp.overlaps)/2))
        median.value <- x.values[median.index]
        abline(v=median.value, col="pink")
        median.exp.cum <- exp.cum[median.index]
        abline(h=median.exp.cum, col="pink")
        right.tail.start <- median.index + 1
        left.tail.start <- median.index
        grouped <- data.frame()
    } else {
        left.tail.start <- which(diff(exp.overlaps > 5) == 1) + 1
        right.tail.start <- which(diff(exp.overlaps > 5) == -1) + 1

        grouped <- data.frame(
            values = x.values[exp.overlaps > 5],
            obs = histogram$counts[exp.overlaps > 5],
            exp = exp.overlaps[exp.overlaps > 5])
    }
    
    abline(v=x.values[c(left.tail.start, right.tail.start)], col="orange")
    
    
    ## Compute chi2 statistics (manually)
    grouped$chi2.obs <- (grouped$obs - grouped$exp)^2 /grouped$exp
    chi2.obs <- sum(grouped$chi2.obs)
    chi2.df <- nrow(grouped) - 1
    chi2.p <- pchisq(q=chi2.obs-1, df=chi2.df, lower.tail = FALSE)
    
    ## Compute overlap classes on the right tail
    
    
    classmax <- which(cumsum(exp.overlaps) > 5)[1]
    exp <- sum(exp.overlaps[1:classmax])
    obs <- sum(histogram$counts[1:classmax])
    chi2 <- (exp - obs)^2/exp

    


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
