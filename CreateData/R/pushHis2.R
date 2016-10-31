pushHis2 <- function(dat){
    ## 'dat' is a data frame with variables 'id' and 'hisclass'
    ## A copy of 'pushHis' with LINNEID <--> id.
    
    library(skum)
    om <- unique(dat$id)
    
    hyhis <- numeric(NROW(dat))
    dat$nyhisclass <- numeric(NROW(dat))
    for (i in om){
        who <- dat$id == i
        ok <- dat$hisclass[who]
        ok <- pushForward(ok)
        dat$nyhisclass[who] <- ok
    }
    dat
}
