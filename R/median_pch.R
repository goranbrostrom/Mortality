median_pch <- function(cuts, levels){
    library(eha) # To be removed ...
    ## Median for a piecewise constant hazards (pch)
    ## distribution.
    ## 'cuts' are strictly poisitive and finite (assuming
    ## the "final cut" being 'Infinity' and the "first" 'zero'.
    ##
    ## Uses ppch from package eha, and base 'uniroot'
    ##
    ## Find x such that P(T <= x) >= 0.5:
    x <- 10
    done <- FALSE
    while(!done & x < 100){
        done <- ppch(x, cuts, levels) > 0.5
        if (!done){
            x <- x + 10
        }
    }

    ##cat("x = ", x, "\n")
    if (x >= 90){
        ret <- NA
    }else{
        fn <- function(y) ppch(y, cuts, levels) - 0.5
        ret <- uniroot(fn, interval = c(0, x))$root
    }
    
    ret
}

    
