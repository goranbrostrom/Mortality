pushForward <- function(x, fill = TRUE){
    n <- length(x)
    if (n > 1){
        for (i in 2:n){
            if (is.na(x[i])) x[i] <- x[i-1]
        }
        if (fill & is.na(x[1])){
            if (!is.na(x[n])){
                m <- which(!is.na(x))[1]
                x[1:(m-1)] <- x[m]
            }
        }
    }
    x
}