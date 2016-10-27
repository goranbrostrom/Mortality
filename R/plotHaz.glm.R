plotHaz.glm <- function(res, printLegend = FALSE, col = 1:4,
                    xlab = "", main = "", layout = c(4, 3)){
    ## 'res' is list of results from a fit.
    ## Note, number of levels of hisclass is hardcoded to 4 (four) here!!
    ## Will be fixed.
    
    leg <- c("1+2", "3+4", "5", "6+7") ## a hack for the time being
    
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
        for (j in 2:4){
            lines(x, y[j, ], col = col[j], lty = j, lwd = 2)
        }
        legend("topleft", legend = leg, col = col, lty = 1:4, cex = 1.0)
    }
    abline(h = 0)
}

    
