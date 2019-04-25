# Example
library(ReMapEnrich)

demo.dir <- "~/ReMapEnrich_demo" 
remapCatalog2018hg38 <- downloadRemapCatalog(demo.dir) 

#-- ReMap catalog in GRange
time1 = Sys.time()
remapCatalog <- bedToGranges(remapCatalog2018hg38)
time2 = Sys.time()
difftime(time2 ,time1, units="auto")
# Time difference of 39.87249 secs

#-- Save Rdata - to speed up the process of reading the file
# -- and converting it to Grange
save(remapCatalog, file = "data/remapCatalog2018hg38.RData")

#-- Laod 
time1 = Sys.time()
load("data/remapCatalog2018hg38.RData")
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 13.05326 secs

#-- Query - Get an ENCODE file as example
ENCFF001VCU <- bedToGranges(downloadEncodePeaks("ENCFF001VCU", demo.dir))

#-- Enrichment default 
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 31.0131 secs


#-- Enrichment  shuffle 6
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=6)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 36.88208 secs


#-- Enrichment  shuffle 6 nCores 3
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=6, nCores=3)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 55.98755 secs


#-- Enrichment  shuffle 6 nCores 6
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=6, nCores=6)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 1.082652 mins


#-- Enrichment  shuffle 12 nCores 1
time1 = Sys.time()
enrichment <- enrichment(ENCFF001VCU, remapCatalog, shuffle=12, nCores=1)
time2 = Sys.time()
difftime(time2, time1, units="auto")
# Time difference of 1.035931 mins



