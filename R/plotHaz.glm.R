plotHaz.glm <- function(res, printLegend = FALSE, col = c("black", "blue", "red"),
                    xlab = "", main = "", layout = c(4, 3)){
    ## 'res' is list of results from a fit.
    ## Note, number of levels of hisclass is hardcoded to 4 (four) here!!
    ## Will be fixed.
    
    ##leg <- c("elite", "lowMan", "lowWhiteC", "worker") ## a hack for the time being
    
    leg <- rownames(res[[1]])
    n.leg <- length(leg)
    
    oldpar <- par(mfrow = layout)
    on.exit(par(oldpar))
    n <- length(res)

    if (length(main) == 1){
        main = rep(main, n)
    }
    
    
    for (i in 1:n){
        x <- as.numeric(colnames(res[[i]]))
        #x <- c(x, x[length(x)] + (x[2] - x[1]) )
        
        y <- res[[i]]
        ymax <- max(y)
        plot(x, y[1, ], col = col[1], type = "l", lty = 1,
              main = names(res)[i], ylim = c(0, ymax),
             xlab = "Age", ylab = "Cum. Hazards", lwd = 2)
        for (j in 2:n.leg){
            lines(x, y[j, ], col = col[j], lty = j, lwd = 2)
        }
        legend("topleft", legend = leg, col = col, lty = 1:4, cex = 1.0)
    }
    abline(h = 0)
}

    
