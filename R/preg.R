preg <- function(formula = "", data, debug = FALSE){
  ## formula = "event ~ offset(log(exposure) + hisclass + age...)"
  ## Argument: formula is a text string equal to ... above
  f1 <- "event ~ offset(log(exposure)) + hisclass + age"
  form <- as.formula(paste(f1, formula))
  fit <- glm(form, data = data, family = poisson)
  if (debug) print(summary(fit)$coefficients)
  y <- exp(c(0, fit$coefficients[2:4]))
  y <- exp(c(fit$coefficients[2], 0, fit$coefficients[3:4]))
  names(y) <- c("elite", "middle", "worker", "NA")
  y
}