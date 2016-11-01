getHeart <- function(dat){
    ## 'dat' is 'vb'
##    dat$eb <- dat$exit + dat$birthdate
##    heart <- with(dat, !is.na(ULORSAK) &
##                       event == 1 &
##                       ((eb < 1951) & ULORSAK == 19 |
##                            (eb >= 1969 & eb < 1987 & ULORSAK %in% 390:458) |
##                            (eb >= 1987 & eb < 1997 & ULORSAK %in% 390:459) |
##                            (eb >= 1997 & eb < 2014 & ULORSAK %in% c("I", "J"))))
##    dat$heart <- heart
##    dat$eb <- NULL
##    dat

    ## New version (ULORSAK is a character vector)
    eb <- dat$exit + dat$birthdate
    library(stringr)
    ulor <- str_trim(dat$ULORSAK)
    fc <- (substr(ulor, 1, 1) %in% c("I", "J")) & (eb >= 1997) & (eb < 2014)
    heart <- fc
    fc <- (ulor %in% as.character(390:459)) & (eb >= 1987) & (eb < 1997)
    heart <- heart | fc
    fc <- (ulor %in% as.character(390:458)) & (eb >= 1969) & (eb < 1987)
    heart <- heart | fc
    fc <- (ulor == "19") & (eb < 1951)
    heart <- heart | fc
    dat$heart <- dat$event & heart
    dat
}
