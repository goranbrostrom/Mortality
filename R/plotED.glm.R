plotED.glm <- function(res,
                       col = 1:length(res),
                       logScale = FALSE,
                       ylab = "",
                       main = ""){
    
    ## 'res' is a list of matrices of cumulative hazards from a piecewise-contant fit
    ## We want the last column of each matrix.

    ##nper <- NCOL(res[[1]])

    nper <- length(res)
    k <- NCOL(res[[1]])
    m <- NROW(res[[1]])
    labb <- names(res)
    
    outp <- matrix(0, ncol = nper, nrow = m)

    for (i in 1:nper){
        outp[, i] <- res[[i]][, k]         
    }
    
    if (logScale){
        outp <- log(outp)
    }
    fr <- min(outp, 0)
    to <- max(outp)
    xv <- (as.numeric(substr(names(res), 1, 4)) +
        as.numeric(substr(names(res), 6, 9))) / 2

    plot(xv, outp[1, ], type = "b", lty = 2, col = col[1], pch = 1,
         ylim = c(fr, to), main = main,
         axes = FALSE, ylab = ylab, cex.axis = 0.4, xlab = "", 
         xlim = c(min(xv), max(xv) + 10))
    axis(1, at = xv, lab = labb, las = 2)
    axis(2, las = 2)
    box()
    for (i in 2:m){
        points(xv, outp[i, ], col = col[i], pch = i)
        lines(xv, outp[i, ], col = col[i], lty = i)
    }
    abline(v = xv, lty = 3, col = "magenta")
    abline(h = 0)
    legend("topright", legend = 1:m, col = col, pch = 1:m, lty = 1:m)
    invisible(outp)
}