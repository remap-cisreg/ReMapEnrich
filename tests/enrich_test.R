intersect = genome.intersect("data/minicat.bed", "data/minific.bed")
shuffle = genomic.shuffle("data/minicat.bed", "data/hg19.genome")

big.intersect = genome.intersect("data/ENCFF001VCU.bed", "data/nrPeaks_all.bed")
big.shuffle = genomic.shuffle("data/ENCFF001VCU.bed", "data/hg19.genome")

enrich = enrichment("data/ENCFF001VCU.bed", "data/nrPeaks_all.bed", "data/hg19.genome", shuffles = 2, fraction = 0.1)
