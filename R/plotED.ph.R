plotED.ph <- function(res,
                   scol = 1:6,
                   logScale = FALSE,
                   ylab = "",
                   main = ""){
    ## 'res' is a list of results from a fit.
    source("R/getH.R")
    outp <- matrix(0, ncol = nper, nrow = 6)

    n <- length(res)
    for (i in 1:nper){
        sfi <- survfit(res[[i]], new.data = data.frame(civst = "unmarried", urban = FALSE))
        x <- cumsum(sfi$strata)
        
        ##outp[, i] <- 1 - sfi$surv[x] ### NOTE!!!
        outp[, i] <- -log(sfi$surv[x])
    }
##    for (j in 1:7){
  ##      outp[j, ] <- outp[j, ] /outp[5, ]
    ##}
    
    if (logScale){
        outp <- log(outp)
    }
    fr <- min(outp, 0)
    to <- max(outp)
    xv <- c(seq(1905, 1945, by = 10), seq(1965, 1995, by = 10))

    plot(xv[1:5], outp[1, 1:5], type = "b", lty = 2, col = scol[1], pch = 1,
         ylim = c(fr, to), main = main,
         axes = FALSE, ylab = ylab, cex.axis = 0.4, xlab = "", 
         xlim = c(min(xv), max(xv) + 10))
    lines(xv[7:9], outp[1, 7:9], type = "b", lty = 2, col = scol[1], pch = 1)
    axis(1, at = xv, lab = labb, las = 2)
    axis(2, las = 2)
    box()
    for (i in 2:6){
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
    legend("topright", legend = 1:6, col = scol, pch = 1:6)
    invisible(outp)
}
