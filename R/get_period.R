get_period <- function(period = "1981-1990", hisc = "hisclass40", 
                       sex = "all", civst = "all", tabl){

    tabl <- tabl[tabl$age != "85", ]
    tabl <- tabl[tabl$period == period, ]
    if (sex != "all"){
        tabl <- tabl[tabl$sex == sex, ]
    }
    if (civst != "all"){
        tabl <- tabl[tabl$civst == civst, ]
    }
    tabl$hisc <- tabl[[hisc]]
    tabl <- aggregate(tabl[, c("event", "exposure")], 
                      by = tabl[, c("age", "hisc")], FUN = sum)
    tabl$haz <- tabl$event / tabl$exposure
    tabl$age <- as.numeric(as.character(tabl$age))
    tabl
}

plot_ash <- function(period = "1981-1990", hisc = "hisclass40", 
                     sex = "all", civst = "all", tabl = vbTab){
    x <- get_period(period, hisc, sex, civst, tabl)
    levs <- levels(x$hisc)
    ##x$haz <- log(x$haz)
    with(x[x$hisc == levs[1], ], plot(age, haz, main = period, type = "b", col = "black", 
                                      xlab = "Age", ylim = c(0.0004, 0.15), ylab = "Mortality", log = "y"))
    with(x[x$hisc == levs[2], ], lines(age, haz, type = "b", col = "darkgreen"))
    with(x[x$hisc == levs[3], ], lines(age, haz, type = "b", col = "red"))
    abline(h = 0)
    legend("topleft", legend = levs, col = c("black", "darkgreen", "red"), lty = 1)
}

s_show <- function(hisc = "hisclass40", sex = "all", civst = "all", tabl = vbTab, secs = 2){
    peri <- levels(tabl$period)
    for (per in peri){
        cat("per = ", per, "\n")
        plot_ash(period = per, hisc = hisc, sex = sex, civst = civst, tabl = tabl)
        Sys.sleep(secs)
    }
}