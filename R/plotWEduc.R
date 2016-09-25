plotWEduc <- function(fit){
    ## fit from a glm
    
    co <- fit$coefficients
    con <- co[1]
    educ <- co[substr(names(co), 1, 4) == "educ"]
    income <- co[substr(names(co), 1, 4) == "inco"]

    educ <- exp(c(con, educ + con))
    names(educ) <- 1:7
    income <- exp(c(con, income + con))
    names(income) = c("lowest", "low", "high", "highest")
    oldpar <- par(mfrow = c(1, 2))
    barplot(100 * educ, xlab = "Education", col = "green", las = 1, 
            ylab = "Mortality per cent")
    barplot(100 * income, xlab = "Income", col = "blue", las = 2,
            ylab = "Mortality per cent")
    par(oldpar)
    invisible(list(educ = educ, income = income))
}