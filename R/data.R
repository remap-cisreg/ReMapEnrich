#' @title Enrichment example
#' @author Zacharie Menetrier
#' @description A dataframe containing an enrichment example.
#' This enrichment has been made with SOX2 from chr22 as a query and all the 
#' remap catalogue of the chr22 as a catalogue.
#' 
#' @format A data frame with 130 rows and 11 variables.
#' 
#' @source \href{http://tagc.univ-mrs.fr/remap/}
#' 
#' @examples 
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed", 
#'                          package = "roken")
#' catFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                        package = "roken")
#' query <- BedToGranges(queryFile)
#' cat <- BedToGranges(catFile)
#' enrichmentExample <- GrEnrichment(query, cat, shuffles = 200, lower = FALSE)
#' 
#' @name enrichmentExample
