#' @title Extract enrichment
#' @author Zacharie Menetrier
#' @description Extracts the information for genomic enrichment.
#' 
#' @param categories=unique(catalog@elementMetadata$id) 
#' The categories contained in the catalog.
#' This option is leaved for faster calculation when this function 
#' is runned multiple times.
#' @param tail If "lower" then, probabilities are P[X > x], 
#' if "higher", P[X <= x], if "both" then higher or lower is selected
#' depending on the number of overlaps vs the theorical mean.
#' @param categoriesOverlaps The number of overlaps for each category.
#' @param theoricalMeans The mean number of overlaps for each category.
#' @param categoriesCount A vector representing the number of time a category 
#' is found in the catalog.
#' @param pAdjust The method that will be used for correcting the p-values.
#' 
#' @return A data frame containing the enrichment informations.
ExtractEnrichment <- function (categories,
                               tail, 
                               categoriesOverlaps, 
                               theoricalMeans,
                               categoriesCount, 
                               pAdjust) {
    cat("Extracting enrichment.\n")
    # Matching the arguments for the tail selection.
    tail <- match.arg(tail, c("lower","higher","both"))
    # Matching the arguments for the p values correction.
    pAdjust <- match.arg(pAdjust, c("BH","BY","fdr","bonferroni"))
    if (pAdjust == "fdr") {
        pAdjust = "BH"
    }
    
    catNumber <-length(categories)

    # Computing the one or two tailed test.
    if (tail == "both") {
        # The p values are get with log transformation for computing extreme values.
        logLowerPVals <- stats::ppois(categoriesOverlaps, theoricalMeans, lower = TRUE, 
                          log = TRUE)
        logHigherPvals <- stats::ppois(categoriesOverlaps, theoricalMeans, lower = FALSE, 
                                log = TRUE)
        logPVals <- ifelse(logLowerPVals > logHigherPvals, logHigherPvals, logLowerPVals)
    } else {
        if (tail == "lower") {
            lowers <- FALSE
        } else if (tail == "higher") {
            lowers <- TRUE
        }
        # The p values are get with log transformation for computing extreme values.
        logPVals <- stats::ppois(categoriesOverlaps, theoricalMeans, lower = lowers, 
                          log = TRUE)
    }

    
  
    
    # Creation of the data frame with all the enrichment informations.
    enrichment <- data.frame(categories, stringsAsFactors = FALSE)
    enrichment = cbind(enrichment,
                       categoriesOverlaps[categories],
                       theoricalMeans[categories],
                       categoriesOverlaps[categories] / 
                           categoriesCount[categories])
    
    # Used to adjust the logarithmic p values.
    logN <- log(catNumber)
    # If the adjustment method is Benjamini & Hochberg or Benjamini & Yekutieli.
    if (pAdjust == "BH" || pAdjust == "BY") {
        # The data frame is sorted for the calculation of the q values.
        enrichment = enrichment[order(logPVals[categories]),]
        logPVals <- sort(logPVals)
        # Different logarithmic values are calculated for the q value.
        logI <- log(1:catNumber)
        logC <- log(sum(1/(1:catNumber)))
    } else {
        logI <- 0
        logC <- 0
    }
	# This is the logarithm of the q values.
	logQVals <- ((logPVals + logN) - logI) + logC
	if (pAdjust == "BY") {
	    logQVals <- cummax(logQVals)
	}
	# If the p values are greater than 1 due to bonferroni correction 
	# then they are adjusted to 1.
	logQVals[logQVals > 0] = 0
    
	# The datas are retrieved after sorting if it was necessary.
    theoricalMeans <- enrichment[,3]
    categoriesOverlaps <- enrichment[,2]
    
    # This is the logarithm of the e values.
    logEVals <- logPVals + log(catNumber)
    
    # The different significances are computed from the logarithmic p and q values.
    sigPVals <- - (logPVals / log(10))
    sigQVals <- - (logQVals / log(10))
    sigEVals <- - (logEVals / log(10))
    
    # The p e and q values are retrieved from the corresponding logarithmic values.
    pVals <- exp(logPVals)
    qVals <- exp(logQVals)
    eVals <- exp(logEVals)
    
    # The enrichment informations don't make any sense for theorical means at 0.
    sigPVals[theoricalMeans == 0] = NA
    pVals[theoricalMeans == 0] = NA
    sigQVals[theoricalMeans == 0] = NA
    qVals[theoricalMeans == 0] = NA
    sigEVals[theoricalMeans == 0] = NA
    eVals[theoricalMeans == 0] = NA

    # Computation of the effecct size.
    effectSizes <- log(categoriesOverlaps / theoricalMeans, base = 2)
    enrichment <- cbind(enrichment, effectSizes, sigPVals, pVals, sigQVals,
                        qVals, sigEVals, eVals)
    # Naming of the columns and reordering the data frame.
    colnames(enrichment) <- c("category", "nb.overlaps", "random.average", 
                              "mapped.peaks.ratio", "effect.size",
                              "p.significance", "p.value", "q.significance",
                              "q.value", "e.significance", "e.value")
    enrichment <- enrichment[order(enrichment$q.significance, decreasing = TRUE),]
    return(enrichment)
}
