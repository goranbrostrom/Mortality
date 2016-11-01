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
## New version: 'ULORSAK' is a character vector:
    library(stringr)
    eb <- dat$exit + dat$birthdate
    ##cancer <- logical(NROW(dat))
    ulor <- str_trim(dat$ULORSAK)
    ## First, fix ulor start with "C":
    fc <- (substr(ulor, 1, 1) == "C") & (eb >= 1997) & (eb < 2014)
    cancer <- fc
    ## Then the 'numeric' versions:
    ##ulor <- as.numeric(ulor)  # NA if letters in it
    