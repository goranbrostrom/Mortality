getH <- function(x, tid){
    ## 'x' is a vector of coefficients from a stratified (on hisclass, 7 strata) gompertz model.
    ## Components 1:14 are the baseline parameters (log(scale1), log(shape1), log(scale2), etc)
    
    ## 'tid' is the age (or duration!) at which H is evaluated.
    
    library(eha)

    x <- x[which(substr(names(x), 1, 3) == "log")] # New 8 Sep 2016!!
           
    x <- exp(x[1:14]) # Note!
    
    ret <- numeric(7) # NOTE!
    
    for (i in 1:7){
        ret[i] <- Hgompertz(tid, scale = x[2 * i - 1], shape = x[2 * i], param = "canonical")
    }
    ret
}
