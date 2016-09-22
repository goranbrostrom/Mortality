plotED.glm <- function(res,
                       scol = 1:4,
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
    xv <- as.numeric(substr(names(res), 1, 4)) + 5 ## NOTE: assuming ten-year periods!

    plot(xv[1:5], outp[1, 1:5], type = "b", lty = 2, col = scol[1], pch = 1,
         ylim = c(fr, to), main = main,
         axes = FALSE, ylab = ylab, cex.axis = 0.4, xlab = "", 
         xlim = c(min(xv), max(xv) + 10))
    lines(xv[7:9], outp[1, 7:9], type = "b", lty = 2, col = scol[1], pch = 1)
    axis(1, at = xv, lab = labb, las = 2)
    axis(2, las = 2)
    box()
    for (i in 2:m){
        points(xv, outp[i, ], col = scol[i], pch = i)
        if (i == 2){
            lines(xv[1:5], outp[i, 1:5], col = scol[i], lty = 2)
            lines(xv[7:9], outp[i, 7:9], col = scol[i], lty = 2)
        }else{
            if (i %in% c(6, 7)){
                lines(xv[1:5], outp[i, 1:5], col = scol[i], lty = 4)
                lines(xv[7:9], outp[i, 7:9], col = scol[i], lty = 4)
            }
        }
    }
    abline(v = xv, lty = 3, col = "magenta")
    abline(h = 0)
    legend("topright", legend = 1:4, col = scol, pch = 1:4)
    invisible(outp)
}
