addPer <- function(dat, name = "period", cuts = seq(1801, 1951, by = 25)){
    n <- length(cuts)
    start <- cal.window(dat, c(cuts[1], cuts[2]))
    start[[name]] <- 1

    if (n > 2){
        for (i in (3:n)){
            tmp <- cal.window(dat, c(cuts[i - 1], cuts[i]))
            tmp[[name]] <- i - 1
            start <- rbind(start, tmp)
        }
    }
    levs <- character(n - 1)

    for (i in 1:(n - 1)){
        levs[i] <- paste(cuts[i], "-", cuts[i + 1] - 1, sep = "")
    }
    start[[name]] <- factor(start[[name]], labels = levs)
    start <- start[order(start$id, start$enter), ]
    ##start <- rc(start)
    start
}

