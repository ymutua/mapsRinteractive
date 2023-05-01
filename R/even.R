#' @title even
#'
#' @description Checks whether an integer is even.
#'
#' @param x Integer.
#'
#' @return Logical value (TRUE or FALSE). TRUE means that the value is even.
#'
#' @examples
#' even(3)
#'
#' @export
even<-function(x) {return(x %% 2 == 0)}

