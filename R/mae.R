#' @title mae
#'
#' @description Calculates the mean absolute error (MAE) from observed and
#' predicted values.
#'
#' @inheritParams e
#'
#' @return The mean absolute error (MAE) calculated from the observed and the
#' predicted values.
#'
#' @details mae = mean(abs(observed - predicted))
#'
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' mae(observed=o, predicted=p)
#'
#' @export
mae<-function(observed, predicted) return(mean(abs(observed-predicted)))












