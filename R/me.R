#' @title me
#'
#' @description Calculates the mean error (ME) from observed and predicted values.
#'
#' @inheritParams e
#'
#' @return The mean error (ME) calculated from the observed and the predicted
#' values.
#'
#' @details ME = bias = mean(observed - predicted)
#'
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' me(observed=o, predicted=p)
#'
#' @export
me<-function(observed, predicted) return(mean(observed-predicted))
