#' BED to GRanges
#'
#' This function loads a bed file and stores it as a GRanges object.
#' The tab-delimited file must be ordered as 'chr', 'start', 'end', 'id', 'score', 'strand'.
#' The minimal bed file must have the 'chr', 'start', 'end' columns.
#' Any columns after the strand column are ignored.
#' 
#' @param path Location of your file
#' @export
bed.to.granges = function(path)
{
    # Imports the bed file in a data frame.
    df = import.bed(path)
    # If the data frame has more than 6 columns then remove them.
    if(ncol(df) > 6)
    {   
        df = df[,-c(7:ncol(df))]
    }
    # If the data frame has less than 3 columns then throw an error.
    if(ncol(df)<3)
    {   
        stop("File has less than 3 columns")
    }
    # If the strand is known in the data frame then replace it by the GRanges equivalent.
    if('strand' %in% colnames(df))
    {
        df$strand = gsub(pattern= "[^+-]+", replacement = '*', x = df$strand)
    }
    # Construct the GRanges object depending on the number of columns.
    if(ncol(df) == 3)
    {
        gr = with(df, GRanges(chrom, IRanges(chromStart, chromEnd)))
    }
    else if (ncol(df)==4)
    {
        gr = with(df, GRanges(chrom, IRanges(chromStart, chromEnd), id=name))
    } 
    else if (ncol(df)==5)
    {
        gr = with(df, GRanges(chrom, IRanges(chromStart, chromEnd), id=name, score=score))
    } 
    else if (ncol(df)==6)
    {
        gr = with(df, GRanges(chrom, IRanges(chromStart, chromEnd), id=name, score=score, strand=strand))
    }
    else
    {
        stop("Error while constructing the GRanges object. No number of columns have been matched.")
    }
    return(gr)
}