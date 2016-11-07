ssyk3TOhisclass <- function(x){
    ## 'x' is a vector of ssyk codes.
    ## as output we want a vector of hisclass codes (7 levels)
    
    ## Step 1: Convert shh$ssyk to "three-digit" (except '110'):
    load("../../../Data/shh.rda")
    shh$ssyk <- shh$ssyk %/% 10 # removes last digit.
    ## Remove duplicate ssyk-rows in shh:
    shh <- shh[!duplicated(shh$ssyk), ]
    
    ## Step 2: SSYK3 to 12-levels hisclass
    indx <- match(x, shh$ssyk)
    tmp <- shh$hisclass[indx]

    ## Step 2: go from 12 to 4 levels: ## NEW!!
    
    res <- rep(-1, length(x))
    
    tmp[is.na(tmp)] <- -1
    
    res[tmp %in% 1:2] <- 1
    res[tmp %in% c(3, 8)] <- 2
    res[tmp %in% 4:6] <- 3
    res[tmp %in% c(7, 9:12)] <- 4
    
    is.na(res) <- res == -1
    
    res
}