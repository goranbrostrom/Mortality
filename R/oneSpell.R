oneSpell <- function(dat){
    ## 'dat' is a survival data frame, with
    ## id, enter, exit, event (+ some covariates)
    ##
    ## Output is a dataframe with one record per id.
    indx <- tapply(dat$id, dat$id)
    dat$enter <- tapply(dat$enter, dat$id, min)[indx]
    dat$exit <- tapply(dat$exit, dat$id, max)[indx]
    dat$event <- tapply(dat$event, dat$id, max)[indx]
    dat <- dat[!duplicated(dat$id), ]
    dat <- dat[dat$exit > dat$enter, ] # Clean...no big deal!
    dat
}