cat = BedToGranges("big_data/nrPeaks_all.bed")
query = BedToGranges("big_data/ENCFF001VCU.bed")
enrich = GrEnrichment(query, cat)
res2=enrich[,5]
names(res2) = enrich[,1]
res2=sort(res2)
res2=res2[(length(res2)-10):length(res2)]
colfunc<-colorRampPalette(c("royalblue","red"))
output.plot=barplot(res2,horiz=TRUE,beside=TRUE,xlab="significance(log10(Pval))",space=0.5,width=0.5,
                    cex.names=0.8,col=colfunc(length(res2)),las=2)

