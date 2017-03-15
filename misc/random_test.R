# Feed the matrix
catalog = BedToGranges("big_data/nrPeaks_all.bed")
categories = unique(catalog@elementMetadata$id)
#
megaMat = array(dim = c(length(categories), 50, 50))
sizeRanges = seq(1000,50000, 1000)
nbRanges = seq(1000, 50000, 1000)
for(cat in 1:length(categories)){
    for(size in 1:length(sizeRanges)){
        for(nb in 1:length(nbRanges)){
            print(nbRanges[nb])
            print(sizeRanges[size])
            megaMat[cat, size, nb] = GrIntersect(GenRegions(nbRanges[nb], sizeRanges[size]), catalog)[categories[cat]]
        }
    }
}
