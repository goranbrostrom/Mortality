getCancer <- function(dat){
    ## 'dat' is 'vb'
    dat$eb <- dat$exit + dat$birthdate
    cancer <- with(dat, !is.na(ULORSAK) &
                       event == 1 &
                       ((eb < 1951) & ULORSAK == 12 |
                            (eb >= 1969 & eb < 1997 & ULORSAK %in% 140:239) |
                            (eb >= 1997 & eb < 2014 & ULORSAK == "C")))
    dat$cancer <- cancer
    dat$eb <- NULL
    dat
}