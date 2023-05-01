#' @title fback
#'
#' @description Prints feedback and adds it to a vector.
#'
#' @param t Character string. Required.
#'
#' @return A Character vector.
#'
#' @keywords internal
#'
fback<-function(t){
  if(!exists('feedback'))feedback<-'Messages'
  print(t)
  return (c(feedback))
}
