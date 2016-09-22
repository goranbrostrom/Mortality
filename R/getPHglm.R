getPHglm <- function(dat, nper, labb, from = 40){
    ## dat$age must be a factor ...
    ## Note: return value is Cumulative hazards at given age points (straight line between).
    ##
    ## 'dat$age' is assumed to be grouped in equidistant intervals!
    res <- vector(nper, mode = "list")
    hc <- levels(dat$hisclass)
    m <- length(hc)
    k <- length(unique(dat$age))
    if (k <= 1) error("No. of age intervals must be at least 2!")
    ageName <- as.numeric(levels(dat$age))
    ageInt <- ageName[2] - ageName[1]
    levs <- matrix(0, nrow = m, ncol = k + 1)
   
    for (i in 1:nper){
   
        for (j in 1:m){
   
            fit <- glm(event ~ offset(log(exposure)) +  age +
                           civst + urban,
                       data = dat[dat$period == labb[i] &
                                  dat$hisclass == hc[j], ], family = poisson)
   
            lupp <- fit$coef[1:k]
            lupp[2:k] <- lupp[2:k] + lupp[1]
            levs[j, ] <-  cumsum(c(0, exp(lupp)) * ageInt) 
        }
        colnames(levs) <- c(ageName, ageName[k] + ageInt)
        rownames(levs) <- hc
        res[[i]] <- levs
    }
    names(res) <- labb
    res
}
