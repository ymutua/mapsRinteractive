#' @title even
#'
#' @description Checks whether an integer is odd.
#'
#' @param x Integer.
#'
#' @return Logical value (TRUE or FALSE). TRUE means that the value is odd.
#'
#' @examples
#' odd(3)
#'
#' @export
odd<-function(x) {return(x %% 2 != 0)}

