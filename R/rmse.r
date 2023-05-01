#' @title rmse
#'
#' @description Calculates the root mean square error (RMSE) from observed and
#' predicted values.
#'
#' @inheritParams e
#'
#' @return The root mean square err or (RMSE) calculated from the observed and
#' the predicted values.
#'
#' @details rmse = sqrt(mean((observed - predicted)^2))
#'
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' rmse(observed=o, predicted=p)
#'
#' @export
rmse<-function(observed, predicted) return(sqrt(mean((observed - predicted)^2)))
