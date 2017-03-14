#' Enrichment
#' 
#' Represents a genomic enrichment.
#' 
#' @export Enrichment
Enrichment <- setClass("Enrichment", slots = c(peaks = "data.frame",  totalPeaks = "integer", mappedPeaks = "integer"))