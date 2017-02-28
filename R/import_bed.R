#' @export
# Cette fonction va permettre d'importer les fichiers (.bed) pour être lu par la suite
import_bed = function(path)
{
    dat = as.data.frame(read.table(path,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote=""))
    colnames(dat) = c("chr", "start", "end")
    return(dat)
}