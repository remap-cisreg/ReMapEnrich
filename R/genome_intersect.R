#' @export
# Cette fonction va permettre de calculer le nombre d'intersections entre les deux fichiers selectionnés
genome_intersect = function(bedfile1, bedfile2, frequency =0)
{
    parameters = paste("-wo -f", frequency)
    dat1 = import_bed(bedfile1)
    dat2 = import_bed(bedfile2)
    intersections = bedr::bedr( input = list(a = dat1, b = dat2), 
                                method = "intersect", 
                                params = parameters
    )
    if (ncol(intersections)>=7)                         #Ajout d'une condition du changement du nom de la colonne 
        {colnames(intersections)[7]="intersect.length"} # pour eviter un erreur avec la fonction overlap_number
    return(intersections)
}