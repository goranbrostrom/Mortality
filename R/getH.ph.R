getH.ph <- function(x){
    ## 'x' is a fit from a stratified (on hisclass, 7 strata) coxph model.
    ## covariates civst and urban
    ## 'tid' is the age (or duration!) at which H is evaluated. Last!!!
    
    library(survival)
    y <- survfit(x, new.data = data.frame(civst = "unmarried", urban = FALSE))
    hupp <- cumsum(y$strata)
##    ret <- 1 - y$surv[hupp]
    ret <- -log(y$surv[hupp]) # Cumulative hazard at the 'end'.
    ret
}
