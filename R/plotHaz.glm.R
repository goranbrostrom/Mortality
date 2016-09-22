plotHaz.glm <- function(res, printLegend = FALSE, col = 1:4,
                    xlab = "", main = "", layout = c(4, 3)){
    ## 'res' is list of results from a fit.
    ## Note, number of levels of hisclass is hardcoded to 4 (four) here!!
    ## Will be fixed.
    
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
        plot(x, y[1, ], col = scol[1], type = "l", lty = 1,
              main = names(res)[i], ylim = c(0, ymax),
             xlab = "Age", ylab = "Cum. Hazards")
        for (j in 2:4){
            lines(x, y[j, ], col = scol[j], lty = j)
        }
        legend("topleft", legend = 1:4, col = scol, lty = 1:4, cex = 0.9)
    }
    abline(h = 0)
}

    
