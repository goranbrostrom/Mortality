mpch <- function(cuts, levels){
  n <- length(levels)
  if (length(cuts) != (n-1)) stop("length(cuts) = ", length(cuts), " != ", n, "\n")
  res <- integrate(ppch, 0, Inf, cuts = cuts, levels = levels, lower.tail = FALSE)
  res$value
}