calculate_i_squared <- function(model) {
  # For binomial GLMM with logit link, within-study variance is π²/3
  sigma_squared <- pi^2/3
  
  # Extract variance components
  vc <- VarCorr(model)$study_name
  
  # Intercept variance (tau²)
  tau_squared_intercept <- as.numeric(vc[1,1])
  i_squared_intercept <- (tau_squared_intercept / (tau_squared_intercept + sigma_squared)) * 100
  
  # Slope variance (if random slopes exist)
  if(nrow(vc) > 1) {
    tau_squared_slope <- as.numeric(vc[2,2])
    i_squared_slope <- (tau_squared_slope / (tau_squared_slope + sigma_squared)) * 100
  } else {
    tau_squared_slope <- NA
    i_squared_slope <- NA
  }
  
  return(list(
    intercept_i_squared = i_squared_intercept,
    slope_i_squared = i_squared_slope,
    tau_squared_intercept = tau_squared_intercept,
    tau_squared_slope = tau_squared_slope,
    sigma_squared = sigma_squared
  ))
}