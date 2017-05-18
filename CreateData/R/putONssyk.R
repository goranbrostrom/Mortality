putONssyk <- function(dat){
    ## 'dat' is the processed file 'fobdata'
    ## This must be run from "Mortality/CreDat"
    
    load("../../Data/yr_ssyk.rda")
    dat$ssyk <- NA

    ##for (year in seq(60, 75, by = 5)){
      ##  nam <- paste("yr", year, sep = "")
    ##    load(paste("data/", nam, ".rda" sep = ""))
##        nyr <- dat[dat$AR == i, ]
  ##      indx <- match(nyr$YRKE)
    ##}
    ## 1960:
    load("../../Data/yr60.rda")
    who <- dat$AR == 1960 & dat$YRKE %in% yr60$old
    nyr <- dat$YRKE[who]
    indx <- match(nyr, yr60$old)
    nyr <- yr60$new[indx]
    dat$YRKE[who] <- nyr
    
    ## 1965:
    load("../../Data/yr65.rda")
    who <- dat$AR == 1965 & dat$YRKE %in% yr65$old
    nyr <- dat$YRKE[who]
    indx <- match(nyr, yr65$old)
    nyr <- yr65$new[indx]
    dat$YRKE[who] <- nyr
    
    ## 1970:
    load("../../Data/yr70.rda")
    who <- dat$AR == 1970 & dat$YRKE %in% yr70$old
    nyr <- dat$YRKE[who]
    indx <- match(nyr, yr70$old)
    nyr <- yr70$new[indx]
    dat$YRKE[who] <- nyr
    
    ## 1975:
    load("../../Data/yr75.rda")
    who <- dat$AR == 1975 & dat$YRKE %in% yr75$old
    nyr <- dat$YRKE[who]
    indx <- match(nyr, yr75$old)
    nyr <- yr75$new[indx]
    dat$YRKE[who] <- nyr
    
    ## 1980: Vad ska vi ta här?? 'yr80.rda' finns inte!
    ## och YRKE80 gäller ej 1980!!
    ## Chansar på att 'yr75.rda' gäller:

    ## 1980 (try):
    load("../../Data/yr75.rda")
    who <- dat$AR == 1980 & dat$YRKE %in% yr75$old
    nyr <- dat$YRKE[who]
    indx <- match(nyr, yr75$old)
    nyr <- yr75$new[indx]
    dat$YRKE[who] <- nyr
    
    ##dat$YRKE[dat$AR == 1980] <- dat$YRKE80[dat$AR == 1980]

    ## 1985:
    dat$YRKE[dat$AR == 1985] <- dat$YRKE80[dat$AR == 1985]

    ## 1990:
    dat$YRKE[dat$AR == 1990] <- dat$YRKE85[dat$AR == 1990]

    is.na(dat$YRKE) <- dat$YRKE %in% c(0, 999)
    
    ## Now put on ssyk:
    
    indx <- match(dat$YRKE, yr_ssyk$yrke)
    dat$ssyk <- yr_ssyk$ssyk[indx]
    dat
}
