getRR <- function(x = 0, mix = 1, MOD, scale_factor = 1) {
  #' @title Get Relative Risk (RR) with Intervals
  #' @description Calculate relative risk (RR), confidence intervals, and prediction intervals based on the specified model.
  #' @param x Numeric vector of exposure values.
  #' @param mix Integer indicating the model to use (1 = lin / int+, 2 = lin / int-, etc.).
  #' @param MOD A list containing data and results from a meta-analysis.
  #' @param scale_factor Numeric, scaling factor for spline predictions (default = 1).
  #' @return Data frame with RR estimates and intervals.
  
  require(mixmeta)
  require(splines)
  
  # Validate model selection
  if (!mix %in% 1:length(MOD)) {
    stop("Only integers in the range 1 to ", length(MOD), " are allowed.")
  }
  
  # Predict exposure effects
  X <- predict.nsplin(MOD[[mix]]$NS, newx = x, scale_factor = scale_factor)
  currentModel <- MOD[[mix]]$m[[1]]
  predicted <- X %*% currentModel$coefficients
  
  # Confidence interval for RR
  Variance_CI <- vcov(currentModel)
  SE_CI <- sqrt(diag(X %*% Variance_CI %*% t(X)))
  RR_CI <- pmax(exp(cbind(predicted, predicted - 1.96 * SE_CI,
                          predicted + 1.96 * SE_CI)), 1)
  
  # Prediction interval for RR
  Variance_PI <- vcov(currentModel) + currentModel$Psi
  SE_PI <- sqrt(diag(X %*% Variance_PI %*% t(X)))
  RR_PI <- pmax(exp(cbind(predicted - 1.96 * SE_PI,
                          predicted + 1.96 * SE_PI)), 1)
  
  # Combine results
  results <- cbind(
    x,                  # Exposure
    RR_CI[, 1],         # Point estimate RR
    RR_CI[, 2],         # Lower 95% CI
    RR_CI[, 3],         # Upper 95% CI
    RR_PI[, 1],         # Lower 95% PI
    RR_PI[, 2]          # Upper 95% PI
  )
  
  colnames(results) <- c("Exposure", "RR","RR_CI_lower", "RR_CI_upper", "RR_PI_lower", "RR_PI_upper")
  
  return(as.data.frame(results))
}
