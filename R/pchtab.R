pchtab <- function(formula, data){
    ## Assume 'events' as name for response, 'exposure' as name for 
    ## exposure, and 'age' as name for factor time.
    fit <- glm(formula, data = data, family = poisson)    
    coeff <- coefficients(fit)
    on <- substr(names(coeff), 1, 3) %in% c("(In", "age")
    bas <- coeff[on]
    f_age <- as.numeric(substr(names(bas)[2], 4, 5)) - 5
    names(bas)[1] <- paste("age", f_age, sep = "")
    bas[2:length(bas)] <- bas[2:length(bas)] + bas[1]
    bas <- exp(bas)
    cuts <- substr(names(bas), 4, 5)
    list(levels = c(0, bas), 
         cuts = as.numeric(cuts))
    
}