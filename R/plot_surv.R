plot_surv <- function(pop, cols = c("darkgreen", "black", "red", "blue"), sex = "Men"){
    pp <- levels(pop$period)
    n <- length(pp)
    hc <- levels(pop$hisclass)
    m <- length(hc)
    res <- matrix(0, nrow = n, ncol = 10)
    cuts <- seq(5, 45, by = 5)
    oldpar <- par(mfrow = c(2, n / 2))
    for (i in 1:n){
        ##cat("i = ", i, "\n")
        pim <- pop[pop$period == pp[i], ]
        for (j in 1:m){
            fit <- glm(event ~ offset(log(exposure)) + age + urban * civst, 
                       data = pim[pim$hisclass == hc[j], ], family = poisson)
            
            atoms <- c(fit$coef[1], fit$coef[substr(names(fit$coef), 1, 3) == "age"])
            nl <- length(atoms)                                 
            atoms[2:nl] <- atoms[2:nl] + atoms[1]
            if (nl < 10){
                klipp <- cuts[1:(nl - 1)]
            }else{
                klipp <- cuts
            }
            
            levels <- exp(atoms)
            x <- seq(40, klipp[length(klipp)] + 45, length = 20)
            y <- ppch(x - 40, cuts = klipp, levels = levels, lower.tail = FALSE)
            if (j == 1){
                plot(x, y, type = "l", col = cols[1], ylim = c(0, 1), xlim = c(40, 90),
                     xlab = "Age", ylab = "", main = paste(sex, pp[i]), axes = FALSE)
                axis(1)
                axis(2, at = c(0.0, 0.5, 1.0), las = 1)
            }else{
                lines(x, y, col = cols[j])
            }
            abline(h = 0.5, lty = 3)
            abline(h = 0)
            legend("bottomleft", legend = hc, col = cols, lty = 1)
        }
    }
    par(oldpar)
    
}