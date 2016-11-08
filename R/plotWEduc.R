plotWEduc <- function(fit){
    ## fit from a glm
    
    co <- fit$coefficients
    con <- co[1]
    educ <- co[substr(names(co), 1, 4) == "educ"]
    income <- co[substr(names(co), 1, 4) == "inco"]

    educ <- exp(c(0, educ))
    ##educ <- exp(c(con, educ + con))
    names(educ) <- 1:7
    income <- exp(c(0, income))
    ##income <- exp(c(con, income + con))
    names(income) = c("lowest", "low", "high", "highest")
    base <- co[1:5]
    base <- c(co[1], co[1] + co[2:5])
    mort <- 5 * sum(exp(base))
    oldpar <- par(mfrow = c(1, 2))
    barplot(100 * (1 - exp(-educ * mort)), xlab = "Education", col = "green", las = 1, 
            ylab = "Mortality per cent")
    barplot(100 * (1 - exp(-income * mort)), xlab = "Income", col = "blue", las = 2,
            ylab = "Mortality per cent")
    par(oldpar)
    invisible(list(educ = educ, income = income))
}