chi2 <- AdjustToPoisson(randoms)
for (category in colnames(randoms)) {
    pdf(file = paste("~/Bureau/roken/graph/Histograms/",category,".pdf",sep = ""))
    catChi2 = chi2[category,]
    catResults <- randoms[,category]
    h <- hist(catResults, breaks = -1:(max(catResults)+1), plot = FALSE)
    expOverlaps <- dpois(x = 0:(max(catResults)), lambda = mean(catResults)) * sum(h$counts)
    plot(h$counts, col ="red", type = 'h', xaxt = 'nt',
         xlab = "Nombre d'intersections", ylab = "Nombre d'occurences",
         main = paste(category)
    )
    axis(side = 1 ,labels = c(0:max(catResults)), at = c(1:(max(catResults)+1)))
    lines(expOverlaps, col = "blue")
    legend("topright",
           paste(
            "mean = ", mean(catResults),
            "\nvariance = ", var(catResults),
            "\nchi2 obs = ", catChi2$chi2.obs,
            "\nchi2 df = ", catChi2$chi2.df,
            "\nchi2 p = ", catChi2$chi2.p
        ), cex = 0.6, bty = 'n'
    )
    dev.off()
}
