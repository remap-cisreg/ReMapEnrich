################################################################
## Install dependencies



required.packages <- c("R.utils", "data.table", "RMySQL")
for (pkg in required.packages) {
    if (! require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
}

required.packages.bioconductor <- c("GenomicRanges")
for (pkg in required.packages.bioconductor) {
    if (! require(pkg, character.only = TRUE)) {
        source("https://bioconductor.org/biocLite.R")
        biocLite(pkg)
    }
    library(pkg, character.only = TRUE)
}
