plot_median <- function(pop){
    pp <- levels(pop$period)
    n <- length(pp)
    hc <- levels(pop$hisclass)
    m <- length(hc)
    res <- matrix(0, nrow = m, ncol = n)
    cuts <- seq(5, 45, by = 5)
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
                atoms <- c(atoms, rep(-10, 10 - nl))
            }
            
            levels <- exp(atoms)
            res[j, i] <- median_pch(cuts, levels) + 40
        }
    }
    rownames(res) <- hc
    colnames(res) <- pp
    cols <- c("darkgreen", "black", "red", "blue")
    plot(1:NCOL(res), res[1, ], type = "b", col = cols[1], axes = FALSE, 
         ylab = "Age", xlab = "Period", ylim = c(60, 91), main = "")
    axis(1, at = 1:NCOL(res), labels = pp)
    axis(2, las = 1)
    axis(4, las = 1)
    box()
    lines(1:NCOL(res), res[2, ], type = "b", col = cols[2])
    lines(1:NCOL(res), res[3, ], type = "b", col = cols[3])
    lines(1:NCOL(res), res[4, ], type = "b", col = cols[4])
    legend("bottomright", legend = c("Middle", "Elite", "Worker", "NA"), col = cols, lty = 1)
    abline(h = c(70, 75, 80, 85, 90), lty = 3)
}