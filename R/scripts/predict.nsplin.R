predict.nsplin <- function(obj, newx, scale_factor = 1) {
  #' @title Predict Natural Splines (with optional adjustment factor)
  #' @description Generate predictions using natural spline transformations based on the provided model object, with optional adjustment for a scalar error factor.
  #' @param obj An object containing the spline model attributes.
  #' @param newx Numeric vector of new data points to generate predictions for.
  #' @param scale_factor Numeric, optional factor to scale the predictions (default = 1, no scaling).
  #' @return A matrix of predicted values using natural splines.
  #' @examples
  #' # Assuming `model` is a predefined spline model object
  #' predict.nsplin(model, c(1, 2, 3, 4, 5))
  predict.ns <- function(obj, newx, intercept) {
    a <- c(list(x = newx), attributes(obj)[c("knots", "Boundary.knots")], c(intercept = intercept))
    do.call("ns", a)
  }
  
  if (attr(obj, "type") == 0) {
    NS <- matrix(newx / 100, ncol = 1)
    if (attr(obj, "intercept")) {
      NS <- cbind(1, NS)
    }
    attr(NS, "intercept") <- attr(obj, "intercept")
  } else if (attr(obj, "type") == 1) {
    NS <- predict.ns(obj, newx = newx, intercept = attr(obj, "intercept"))
  } else if (attr(obj, "type") == 2) {
    NS <- predict.ns(obj, newx = newx, intercept = FALSE)
    NS <- NS - attr(obj, "ref")[rep(1, nrow(NS)), , drop = FALSE]
    if (attr(obj, "intercept")) {
      NS <- cbind(1, NS)
    }
    attr(NS, "intercept") <- attr(obj, "intercept")
    attr(NS, "ref") <- attr(obj, "ref")
  }
  
  # Apply the scaling factor if provided (default is 1, which means no scaling)
  NS <- NS * scale_factor
  
  colnames(NS) <- paste0("NS_", 1:ncol(NS))
  attr(NS, "class") <- c("nsplin", "basis", "matrix")
  attr(NS, "type") <- attr(obj, "type")
  
  return(NS)
}
