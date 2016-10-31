pH <- function(dat){
    ## 'dat' is a data frame with variables 'LINNEID' and 'hisclass'
    library(tidyr)
    library(dplyr)
    ##om <- unique(dat$LINNEID)
    
    ##hyhis <- numeric(NROW(dat))
    ##dat$nyhisclass <- numeric(NROW(dat))
    x <- group_by(dat, LINNEID) %>% fill_("hisclass")
    ##for (i in om){
      ##  dat[dat$LINNEID == i, ] <- fill(dat[dat$LINNEID == i, ], hisclass)
    ##}
    x
}