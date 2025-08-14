getPoC <- function(x = 0, mix = 1, MOD, scale_factor = 1) {
  #' @title Get Probability of Causation (PoC)
  #' @description Calculate and probability of causation (PoC) based on the specified model to derive the exposure-response relationship.
  #' @param x Numeric vector of exposure values.
  #' @param mix Integer indicating the model to use (1 = lin / int+, 2 = lin / int-, 3 = spline / int+, 4 = spline / int-).
  #' @param MOD A list containing data and results from a meta-analysis of the exposure-response relationship.
  #' @param scale_factor Numeric, optional scaling factor to adjust for underestimation in the spline model (default = 1, no scaling).
  #' @return Data frame with RR and PoC calculated values including all confidence and prediction intervals.
  #' @examples
  #' # Example usage:
  #' # Assuming MOD is a predefined list of models
  #' getPoC(mix = 1, x = 0:10, MOD = MOD, scale_factor = 1.75)
  
  # Get RR results first 
  rr_results <- getRR(x = x, mix = mix, MOD = MOD, scale_factor = scale_factor)
  
  # Calculate PoC for all RR columns: PoC = (RR - 1) / RR
  rr_cols <- c("RR", "RR_CI_lower", "RR_CI_upper", "RR_PI_lower", "RR_PI_upper")
  poc_cols <- c("PoC", "PoC_CI_lower", "PoC_CI_upper", "PoC_PI_lower", "PoC_PI_upper")
  
  # Calculate PoC values
  for (i in 1:length(rr_cols)) {
    rr_results[[poc_cols[i]]] <- (rr_results[[rr_cols[i]]] - 1) / rr_results[[rr_cols[i]]]
  }
  
  return(rr_results)
}