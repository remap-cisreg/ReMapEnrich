intersect = intersect.bed("data/minicat.bed", "data/minific.bed")
shuffle = shuffle.bed("data/minicat.bed", "data/hg19.genome")

big.intersect = intersect.bed("data/ENCFF001VCU.bed", "data/nrPeaks_all.bed")
big.shuffle = shuffle.bed("data/ENCFF001VCU.bed", "data/hg19.genome")

enrich = enrichment("data/ENCFF001VCU.bed", "data/nrPeaks_all.bed", "data/hg19.genome", shuffles = 2, fraction = 0.1)
