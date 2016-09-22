plotHaz.ph <- function(res, printLegend = FALSE, col = 1:6,
                    xlab = "", main = "", layout = c(3, 3)){
    ## 'res' is list of results from a fit.
    oldpar <- par(mfrow = layout)
    on.exit(par(oldpar))
    n <- length(res)

    if (length(main) == 1){
        main = rep(main, n)
    }
    
    for (i in 1:n){
        plot(survfit(res[[i]]), fun = "cumhaz", col = scol, 
             xlab = xlab, main = main[i])
        legend("topleft", legend = 1:6, col = scol, lty = 1:6, cex = 0.9)
    }
    abline(h = 0)
}

    
