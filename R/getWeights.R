getWeights <- function(dat){
    options(na.action = "na.exclude")
    ## "Inverse probability weights" are created.
    hlevs <- levels(dat$hisclass)
    n <- length(hlevs)
    wght <- numeric(NROW(dat))
    ##
    for (i in seq_along(hlevs)){
        fit <- glm(I(hisclass == hlevs[i]) ~ urban * civst, data = dat, family = poisson)
        gg <- dat$hisclass == hlevs[i]
        wght[gg] <- 1 / predict(fit, type = "response")[gg]
    }
    ##
    wght
}