fixCovar2 <- function(id, covar, enter, exit, age, lookBack = 0){
    ## The difference to the function 'fixCovar' is the argument
    ## 'lookBack', which means that we search for the value in an
    ## interval (age - lookBack, age]. The last non-NA value (if any)
    ## is chosen, otherwise NA.

    ## The assumption here is NOT that covar is positive numeric, with
    ## the value 0 (zero) a code for missing.

    ord <- order(id, enter)
    id <- id[ord]
    covar <- covar[ord]
    enter <- enter[ord]
    exit <- exit[ord]

    isF <- is.factor(covar)
    if (isF){
        ##print(table(covar, useNA = "ifany"))
        covar <- factor(covar) # remove unused levels
        levs <- levels(covar)
        ##cat("levs = ", levs, "\n")
        covar <- as.numeric(covar)
    }

    out <- enter > age | exit < age - lookBack
    is.na(covar) <- out | is.na(covar)

    getval <- function(x){ # Get the last non-NA value
        if (length(x) <= 1 | all(is.na(x))){
            val <- x[1]
        }else{
            y <- x[!is.na(x)]
            val <- y[length(y)]
        }
        val
    }

    indx <- tapply(id, id)

    ret <- tapply(covar, id, getval)[indx]
    if (isF){
        ret <- factor(ret, levels = 1:length(levs), labels = levs)
    }
    ret
}
