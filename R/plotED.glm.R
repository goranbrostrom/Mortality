plotED.glm <- function(res,
                       col = 1:NROW(res[[1]]),
                       logScale = FALSE,
                       relative = FALSE,
                       ylab = "",
                       main = "",
                       exclude = 100){
    
    ## 'res' is a list of matrices of cumulative hazards from a piecewise-contant fit
    ## We want the last column of each matrix.

    ##nper <- NCOL(res[[1]])

    ## Assuming 'm == 4', we try:
    leg = c("1+2", "3+4", "5", "6+7")
    nper <- length(res)
    k <- NCOL(res[[1]])
    m <- NROW(res[[1]])
    labb <- names(res)

    out <- 1:nper
    out <- out[!(out %in% exclude)]
    outp <- matrix(0, ncol = nper, nrow = m)

    for (i in 1:nper){
        outp[, i] <- res[[i]][, k]         
    }
    
    if (logScale){
        outp <- log(outp)
    }
    if (relative){
        outp <- prop.table(outp, 2)
    }
    fr <- min(outp, 0)
    if (relative){
        to <- max(outp[, out])
        fr <- min(outp[, out])
        #cat("to = ", to, ", fr = ", fr, "\n")
    }else{    
        to <- max(outp)
    }
    xv <- (as.numeric(substr(names(res), 1, 4)) +
        as.numeric(substr(names(res), 6, 9))) / 2

    plot(xv[out], outp[1, -exclude], type = "b", lty = 1, col = col[1], pch = 1,
         ylim = c(fr, to), main = main,
         axes = FALSE, ylab = ylab, cex.axis = 0.4, xlab = "", 
         xlim = c(min(xv), max(xv) + 25))
    axis(1, at = xv, lab = labb, las = 2)
    axis(2, las = 2)
    box()
    ##lines(xv[7:nper], outp[1, 7:nper], col = col[1], pch = 1)
    for (i in 2:m){
        points(xv[out], outp[i, out], col = col[i], pch = i)
        lines(xv[out], outp[i, out], col = col[i], lty = i)
        ##lines(xv[7:nper], outp[i, 7:nper], col = col[i], lty = i)
    }
    abline(v = xv[out], lty = 3, col = "magenta")
    abline(h = 0)
    legend("topright", legend = leg, col = col, pch = 1:m, lty = 1:m)
    invisible(outp)
}
