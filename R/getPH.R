getPH <- function(adu, fo, nper1, nper2, labb, from = 40){
    ##cat("names(adu) = ", names(adu), "\n")
    res <- vector(mode = "list", length = nper1 + nper2)

    for (i in 1:nper1){
        res[[i]] <- coxph(Surv(enter-from, exit-from, event) ~ strata(hisclass) + civst + urban,
                           data = adu[adu$period == labb[i], ], model = TRUE) 
    }

    for (i in 1:nper2){
        i2 <- i + nper1
        res[[i2]] <- coxph(Surv(enter-from, exit-from, event) ~ strata(hisclass) + civst + urban, 
                           data = fo[fo$period == labb[i2], ], model = TRUE)
    }
    res
}
