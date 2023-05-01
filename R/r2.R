#' @title r2
#'
#' @description Calculates the coefficient of determination (r2) for a linear
#' regression model between predicted values and observed values.
#'
#' @inheritParams e
#'
#' @return Coefficient of determination (r2) for a linear regression model
#' between predicted values and observed values.
#'
#' @importFrom stats lm
#'
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' r2(observed=o, predicted=p)
#'
#' @export
r2<-function(observed, predicted) {
  return(summary(lm(predicted ~ observed, data.frame(predicted,observed)))$r.squared)
  }
