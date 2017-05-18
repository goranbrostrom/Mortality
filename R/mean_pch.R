mean_pch <- function(cuts, levels){
    ## Mean of a piecewise constant hazards (PCH) distribution

    library(eha)
    fn <- function(x) ppch(x, cuts, levels, lower.tail = FALSE)

    integrate(fn, lower = 0, upper = Inf)$value
}
