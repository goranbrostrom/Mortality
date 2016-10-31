ssykTOhisclass <- function(x){
    ## 'x' is a vector of ssyk codes.
    ## as output we want a vector of hisclass codes (7 levels)
    
    ## Step 1: ssyk to 12-levels hisclass
    load("../../../Data/shh.rda")
    indx <- match(x, shh$ssyk)
    tmp <- shh$hisclass[indx]

    ## Step 2: go from 12 to 7 levels:
    
    
    res <- rep(-1, length(x))
    
    tmp[is.na(tmp)] <- -1
    
    res[tmp %in% 1:2] <- 1
    res[tmp == 3] <- 2
    res[tmp == 8] <- 3
    res[tmp %in% 4:6] <- 4
    res[tmp == 7] <- 5
##    res[tmp %in% c(9, 11)] <- 6
##    res[tmp %in% c(10, 12)] <- 7 # skip klass 7!
    res[tmp %in% 9:12] <- 6
    is.na(res) <- res == -1
    
    res
}