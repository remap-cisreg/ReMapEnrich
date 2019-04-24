# Example
library(ReMapEnrich)

demo.dir <- "~/ReMapEnrich_demo" 
remapCatalog2018hg38 <- downloadRemapCatalog(demo.dir) 

#-- ReMap catalog in Grange
time1 = Sys.time()
remapCatalog <- bedToGranges(remapCatalog2018hg38)
time2 = Sys.time()
difftime(time2 ,time1, units="auto")
# 1min47 to convert ReMap to Grange
# [1] "2019-04-24 20:58:01 CEST"
# [1] "2019-04-24 20:59:47 CEST"

#-- Save Rdata - to speed up the process of reading the file
# -- and converting it to Grange
save(remapCatalog, file = "data/remapCatalog2018hg38.RData")

#-- Laod 
time1 = Sys.time()
load("data/remapCatalog2018hg38.RData")
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 39.84031 secs

#-- Query - Get an ENCODE file as example
ENCFF001VCU <- bedToGranges(downloadEncodePeaks("ENCFF001VCU", demo.dir))

#-- Enrichment default 
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 1.140819 mins


#-- Enrichment  shuffle 6
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=6)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 1.085097 mins


#-- Enrichment  shuffle 6 nCores 3
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=6, nCores=3)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 2.516755 mins


#-- Enrichment  shuffle 6 nCores 6
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=6, nCores=6)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 2.88172 mins


#-- Enrichment  shuffle 12 nCores 1
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=12, nCores=1)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 1.854118 mins



