pgetmedian <- function(dat, nper, labb, from = 65, median = TRUE){
    ## For calculating remaining life:
    ## dat$age must be a factor ...
    ## Note: return value is Cumulative hazards at given age points (straight line between).
    ##
    ## 'dat$age' is assumed to be grouped in equidistant intervals!
    source("R/mpch.R")
    
    labb <- levels(dat$period)
    nper <- length(labb)
    hc <- levels(dat$hisclass)
    m <- length(hc)
    dat$age <- factor(dat$age)
    k <- length(unique(dat$age))
    if (k <= 1) error("No. of age intervals must be at least 2!")
    cts <- as.numeric(levels(dat$age))[-1]
    medi <- matrix(0, ncol = nper, nrow = m)
    for (j in 10:nper){
        cat("j = ", j, "\n")
        for (i in 1:m){
            cat("   i = ", i, "\n")
            fit <- glm(event ~ offset(log(exposure)) + age + urban + civst,
                     data = dat[dat$period == labb[j] & dat$hisclass == hc[i], ], family = poisson)
            print(fit$coefficients)
            co <- fit$coefficients[1:k]
            co[2:k] <- co[2:k] + co[1]
            cat("co = ", co, "\n")
            levs <- exp(co)
            if (median){
                medi[i, j] <- qpch(0.5, levels = levs, cuts = cts - from)
            }else{
                medi[i, j] <- mpch(levels = levs, cuts = cts - from)
            }
        }
    }
    rownames(medi) <- hc
    colnames(medi) <- labb
    medi
}
