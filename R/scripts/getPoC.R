getPoC <- function(x = 0, mix = 1, MOD, scale_factor = 1) {
  #' @title Get Probability of Causation (PoC)
  #' @description Calculate probability of causation (PoC) and risk ratio (RR) based on the specified model to derive the exposure-response relationship.
  #' @param x Numeric vector of exposure values.
  #' @param mix Integer indicating the model to use (1 = lin / int+, 2 = lin / int-, 3 = spline / int+, 4 = spline / int-).
  #' @param MOD A list containing data and results from a meta-analysis of the exposure-response relationship.
  #' @param scale_factor Numeric, optional scaling factor to adjust for underestimation in the spline model (default = 1, no scaling).
  #' @return Data frame with calculated values.
  #' @examples
  #' # Example usage:
  #' # Assuming MOD is a predefined list of models
  #' getPoC(mix = 1, x = 0:10, MOD = MOD, scale_factor = 1.75)
  
  require(mixmeta)
  require(splines)
  
  # Validate parameters
  if (!mix %in% 1:length(MOD)) {
    stop("Only integers in the range 1 to ", length(MOD), " are allowed.")
  }
  
  # Predict values based on the selected model, using the specified scale factor
  X <- predict.nsplin(MOD[[mix]]$NS, newx = x, scale_factor = scale_factor)
  currentModel <- MOD[[mix]]$m[[1]]
  predicted <- X %*% currentModel$coefficients
  
  # Calculate standard errors (SE) and relative risks (RR)
  Variance <- vcov(currentModel)
  SE <- sqrt(diag(X %*% Variance %*% t(X)))
  RR <- pmax(exp(cbind(predicted, predicted - 1.96 * SE,
                       predicted + 1.96 * SE)), 1)
  
  results <- cbind(x, RR[, 1], (RR - 1) / RR)
  
  # Adjust variance for prediction intervals
  Variance <- vcov(currentModel) + currentModel$Psi
  SE <- sqrt(diag(X %*% Variance %*% t(X)))
  RR <- pmax(exp(cbind(predicted - 1.96 * SE, predicted + 1.96 * SE)), 1)
  
  results <- cbind(results, (RR - 1) / RR)
  
  colnames(results) <- c("Exposure", "RR", "PoC", 
                         "2.5%", "97.5%", "p2.5%", "p97.5%")
  
  return(as.data.frame(results))
}