#' @title Export enrichment
#' @description Create an output file for an enrichment data frame.
#' @author Martin Mestdagh
#' 
#' @param enrichment Enrichment data frame to output.
#' @param fileName="output" Name of the output file.
#' @param format="tsv" The format of the output file.
#'      
#' @usage exportEnrichment(enrichment, fileName = "output", format = "tsv")
#' 
#' @examples 
#' data("enrichment_example", "roken")
#' exportEnrichment(enrichment_example)
#'      
#' @export
exportEnrichment  <- function(enrichment, fileName = "output", format = "tsv") {
    if(format == "csv") {
        utils::write.csv(enrichment, file = fileName)
    } else {
        utils::write.table(enrichment, file = fileName, quote = FALSE, sep = '\t',
                    col.names = NA)
    }
}
