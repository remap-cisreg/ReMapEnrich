#' @title Adjust to poisson
#' @author Zacharie Menetrier
#' @description Adjust random tests to the poisson distribution
#' through goodness of fit test whith chi2.
#' 
#' @param randomIntersections This is a data frame with each row containing
#' a number of overlap for each category in the columns.
#' @param pAdjust Correction method. Can be abbreviated.
#' @param showCategories=TRUE Show the categories when they are computed.
#' Make debugging easier.
#' 
#' @return A data frame containing all the chi2 test informations.
#' 
#' @usage fitPoisson(randomIntersections, pAdjust = "fdr")
#' 
#' @examples
#' catalog <- bedToGranges(system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                           package = "ReMapEnrich"))
#' randoms <- randomIntersections(catalog, 500, 1000, 1000)
#' chi2results <- fitPoisson(randoms)
#' 
#' @export
fitPoisson <- function(randomIntersections, pAdjust = "fdr", showCategories = TRUE) {
    # Creating the chi square result data frame.
    chisq.result <- data.frame(matrix(nrow = ncol(randomIntersections), ncol = 4))
    row.names(chisq.result) <- colnames(randomIntersections)
    colnames(chisq.result) <- c("chi2.p", "chi2.df", "chi2.obs", "lambda")
    for (category in colnames(randomIntersections)) {
        if (showCategories) {
            cat(category, "\n")
        }
        # Gets the result of the current category.
        cat.result <- randomIntersections[,category]
        # Creates the histogram for getting the counts of it.
        h <- graphics::hist(cat.result, breaks = -1:(max(cat.result)+1), plot = FALSE)
        # The expected overlaps are created from the Poisson distribution.
        exp.overlaps <- stats::dpois(x = 0:(max(cat.result)+1), lambda = mean(cat.result)) * sum(h$counts)
        # The median index is calculated to begin the groupment from the middle of the distribution.
        median.index <- which.min(abs(cumsum(exp.overlaps) - sum(exp.overlaps)/2))
        # Instanciation of the grouped results vector.
        exp.to.analyze <- vector()
        obs.to.analyze <- vector()
        # The current index is set to the median. It will be used to iterate through a loop.
        current.index = median.index
        # This is a boolean that will be set to TRUE when the infinite loop needs to stop.
        exit.loop <- FALSE
        # This loop is for the right side of the expected overlaps from the median index to the end.
        while (TRUE){
            # The cumulative sum is calculated from the current index to the end of the expected overlaps.
            current.cum <- cumsum(exp.overlaps[current.index:length(exp.overlaps)])
            # The next index at which the sum from the current index to it will be greater than 5.
            next.index <- which(current.cum > 5)[1] + current.index - 1
            # If the next index has reached the end of the overlaps then it will exit the loop.
            if(next.index >= length(exp.overlaps)){
                exit.loop <- TRUE
            # If the next iteration of this loop will result in a cumulative sum under 5 then,
            # the next index is set to the end of the overlaps and the loop will be exited.
            } else if (sum(exp.overlaps[(next.index + 1) : length(exp.overlaps)]) < 5) {
                next.index <- length(exp.overlaps)
                exit.loop <- TRUE
            }
            # Adding the grouped overlaps to the expected and observed vectors.
            exp.to.analyze <- c(exp.to.analyze, sum(exp.overlaps[current.index:next.index]))
            obs.to.analyze <- c(obs.to.analyze, sum(h$counts[current.index:next.index]))
            current.index <- next.index + 1
            if (exit.loop) {
                break
            }
        }
        # The current index is set to the median - 1. It will be used to iterate decremently through a loop.
        current.index = median.index - 1
        # This is a boolean that will be set to TRUE when the infinite loop needs to stop.
        exit.loop <- FALSE
        while (TRUE){
            # The cumulative sum is calculated from the current index to the begin of the expected overlaps.
            current.cum <- cumsum(exp.overlaps[current.index:1])
            # The next index at which the sum from the current index to it will be greater than 5.
            next.index <- current.index - which(current.cum > 5)[1] + 1
            # If the next index has reached the begin of the overlaps then it will exit the loop.
            if(next.index <= 1){
                break
            # If the next iteration of this loop will result in a cumulative sum under 5 then,
            # the next index is set to the begin of the overlaps and the loop will be exited.    
            } else if (sum(exp.overlaps[(next.index - 1) : 1]) < 5) {
                next.index <- 1
                exit.loop <- TRUE
            }
            # Adding the grouped overlaps to the expected and observed vectors.
            exp.to.analyze <- c(exp.to.analyze, sum(exp.overlaps[current.index:next.index]))
            obs.to.analyze <- c(obs.to.analyze, sum(h$counts[current.index:next.index]))
            current.index <- next.index - 1
            if (exit.loop) {
                break
            }
        }
        # Computes chi square statistics.
        chi2.df <- length(obs.to.analyze) - 1
        chi2.obs <- sum((obs.to.analyze - exp.to.analyze)^2 / exp.to.analyze)
        chi2.p <- stats::pchisq(q=chi2.obs-1, df=chi2.df, lower.tail = FALSE)
        chisq.result[category, 1] <- chi2.p
        chisq.result[category, 2] <- chi2.df
        chisq.result[category, 3] <- chi2.obs
        chisq.result[category, 4] <- mean(cat.result)
    }
    return(chisq.result)
}
