getlisaold <- function(){
    library(eha)
    library(skum)
    load("../../Data/lisadata.rda")
    civst <- lisadata$CIV
    civst[is.na(civst)] <- 0
    civst[civst == 4] <- 3
    civst[civst == 5] <- 2
    civst[civst == 6] <- 3
    is.na(civst) <- civst == 0
    lisadata$civst <- civst
    rm(civst)
    ## Fix KON:
    load("../../Data/sex.rda")
    indx <- match(lisadata$LINNEID, sex$LINNEID)
    lisadata$sex <- sex$KON[indx]
    levels(lisadata$sex) <- c("male", "female")
    lisaold <- lisadata
    lisaold$id <- lisaold$LINNEID
    source("R/addPer.R")
    cat("addPer:\n")
    lisaold <- addPer(lisaold, name = "period", cuts = seq(1986, 2014, by = 2))
    lisaold <- lisaold[with(lisaold, !is.na(civst)), ]
    lisaold$civst <- factor(lisaold$civst, 
                         labels = c("unmarried", "married", "prev.married"))
    lisaold$urban <- lisaold$FORSAMLING %in% c(248001, 248002, 284004, 
                                               284005, 248011, 248201, 248204)
    source("R/ageGroup.R")
    cat("addPer:\n")
    lisaold <- ageGroup(lisaold, seq(40, 90, by = 5))
    lisaold <- age.window(lisaold, c(40, 90))
    lisaold$exposure <- with(lisaold, exit - enter)
    lisaold$educ <- as.factor(lisaold$SUN2000NIVA_OLD)
    lisaold$income <- cut(lisaold$DISPINKPERSF, c(-600, 730, 958, 1270, 500000),
                          labels = c("lowest", "low", "high", "highest"))
    save(lisaold, file = "lisaold.rda")
    loTab <- aggregate(lisaold[, c("event", "exposure")], 
                       lisaold[, c("sex", "age", "period", "urban", 
                                   "civst", "income", "educ")],
                       FUN = sum)
    save(loTab, file = "../Mortality/data/loTab.rda")
    invisible(loTab)
}
