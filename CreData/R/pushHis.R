pushHis <- function(dat){
    ## 'dat' is a data frame with variables 'LINNEID' and 'hisclass'
    library(skum)
    om <- unique(dat$LINNEID)
    
    hyhis <- numeric(NROW(dat))
    dat$nyhisclass <- numeric(NROW(dat))
    for (i in om){
        who <- dat$LINNEID == i
        ok <- dat$hisclass[who]
        ok <- pushForward(ok)
        dat$nyhisclass[who] <- ok
    }
    dat
}