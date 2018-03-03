extractFob <- function(year, followUp, data){
    ## data = fob
    out <- fob[fob$AR == year & 
                     fob$KOMMUN %in% ourkom &
                     fob$LINNEID %in% per$LINNEID, ]
    ```
    
    They need to be of the right age (born on November 1, 1940 or earlier). We
    get the birthdates from *inddata*.
    
    ```{r mat60}
    library(eha)
    indx <- match(fob60$LINNEID, per$LINNEID)
    fob60$birthdate <- per$birthdate[indx]
    fob60$fobdate <- as.numeric(toTime("1960-11-01"))
    ##lim <- as.numeric(toTime("1940-11-01"))
    ##fob60 <- fob60[fob60$birthdate < lim, ]
    indx <- match(fob60$LINNEID, per$LINNEID) # NOTE: Necessary!
    fob60$enter <- fob60$fobdate - fob60$birthdate
    dd <- per$deathdate[indx]
    ##fob60$event <- !(is.na(dd)) & (dd <= lim + 25) # NOTE: 40 --> 25!
    fob60$event <- !(is.na(dd)) & (dd <= fob60$fobdate +5)
    fob60$exit <- fob60$enter + 5
    fob60$exit[fob60$event] <- dd[fob60$event] - fob60$birthdate[fob60$event]
    head(fob60)
}