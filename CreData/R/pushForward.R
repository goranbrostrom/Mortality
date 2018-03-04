pushForward <- function(x){
    n <- length(x)
    if (n > 1){
        for (i in 2:n){
            if (is.na(x[i])) x[i] <- x[i-1]
        }
    }
    x
}