extractLisa <- function(year, followUp, data){
    ## data = lisa
    
    out <- data[data$AR == year, ]
    
    library(eha)
    
    out$fobdate <- as.numeric(toTime(paste(year + 1, "01", "01", sep = "-")))
    
    out$enter <- out$fobdate - out$birthdate
    out$event <- !(is.na(out$deathdate)) & (out$deathdate <= out$fobdate + followUp)
    out$exit <- out$enter + followUp
    out$exit[out$event] <- out$deathdate[out$event] - out$birthdate[out$event]
    out
}