for (category in colnames(randoms)) {
    pdf(file = paste("~/Bureau/roken/graph/Histograms/",category,".pdf",sep = ""))
    catResults <- randoms[,category]
    h <- hist(catResults, breaks = 0:(max(catResults)+1), plot = FALSE)
    expOverlaps <- dpois(x = 0:max(catResults), lambda = mean(catResults)) * sum(h$counts)
    hits <- vector()
    for (i in 1:length(expOverlaps)) {
        hits <- c(hits, rep(i-1, expOverlaps[i]))
    }
    plot(h$counts, col ="red", type = 'h', xaxt = 'nt',
         xlab = "Nombre d'intersections", ylab = "Nombre d'occurences",
         main = paste(category, 
                      "\nmean the =",
                      mean(hits),
                      "\nmean obs =",
                      mean(catResults)
                      )
    )
    axis(side = 1 ,labels = c(0:max(catResults)), at = c(1:(max(catResults)+1)))
    lines(expOverlaps, col = "blue")
    dev.off()
}
