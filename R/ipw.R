ipw <- function(dat){
    ## Creates 'inverse probability weights'
    ## Confounders: 'urban' and 'civst', 2 x 3 levels.
    ## Independently for: 'period', 'sex', and 'age' (5-year intervals.)
    
    ## We add together "married" and "prev.married":
    
    levels(dat$civst) <- c("married", "unmarried", "married")
    dat$civst <- factor(dat$civst)
    
    ## We aggregate over 'cohort':
    dat <- aggregate(dat[c("event", "exposure")], by = dat[c("sex", "age", "period", "civst", "hisclass", "urban")], FUN = sum)
    
    dat <- dat[order(dat$sex, dat$period, dat$age), ]
    rownames(dat) <- 1:nrow(dat)
    dat$age <- factor(dat$age)
    dat$wght <- NA
    periods <- levels(dat$period)
    n.per <- length(periods)
    ages <- levels(dat$age)
    n.ages <- length(ages)
    
    for (per in 1:n.per){
        for (age in 1:n.ages){
            for (sx in c("male", "female")){
                ##cat("sex = ", sx, "\n")
                select <- dat$period == periods[per] & dat$age == ages[age] & dat$sex == sx
                ct <- dat[select, c("hisclass", "urban", "civst", "exposure", "event")]
                ct$id <- paste(ct$civst, ct$urban, sep = "_")
                all <- aggregate(ct[c("event", "exposure")], by = ct["hisclass"], FUN = sum)
                indx <- match(ct$hisclass, all$hisclass)
                totexpo <- all$exposure[indx]
                wght <- totexpo / ct$exposure
                ct$event <- wght * ct$event
                ct$exposure <- wght * ct$exposure
                out <- aggregate(ct[c("event", "exposure")], by = ct["hisclass"], FUN = sum)
                out$sex <- sx
                out$period <- periods[per]
                out$age <- ages[age]
                out$rate <- out$event / out$exposure
                ##return(out)
                if (per == 1 & age == 1 & sx == "male"){
                    res <- out
                }else{
                    res <- rbind(res, out)
                }
                ##
            }
        }
    }
    res
}