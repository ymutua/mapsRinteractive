#' @title kth
#'
#' @description Identification of the kth highest/lowest value(s).
#'
#' @param x Numeric vector.
#'
#' @param k Positive integer. The order of the value to find. Default = 2,
#' which means that the next highest/lowest values is identified.
#'
#' @param highest Logical. TRUE means that the kth highest value(s) is/are
#' identified. FALSE means that the kth lowest value(s) is/are identified.
#' Default = TRUE.
#'
#' @param index Logical. TRUE means that the index/indices of the kth highest/lowest
#' value(s) is/are returned. FALSE means that the kth highest/lowest value
#' itself is returned. If ties exist and argument multiple = TRUE, the returned value is a vector, else
#' it is a value. Default = FALSE.
#'
#' @param unique Logical. TRUE means that duplicates are removed before the
#' identification of the kth highest/lowest value(s). Default=FALSE
#'
#' @param multiple Logical. TRUE means that, If ties exist a vector of
#' all values in x that are equal to the kth highest/lowest values is returned.
#' FALSE means that one random value from the vector of index values is
#' returned. Default=FALSE
#'
#' @return If index = FALSE: the kth highest/lowest value is returned.
#'
#'         If index = TRUE: the index of the kth highest/lowest value (s) is/are
#'         returned.
#'
#' @details
#' NA values are removed.
#'
#' @examples
#' kth(x=1:20, k=3, highest=FALSE)
#'
#' @export
kth<-function(x=NULL, k=2, highest=TRUE, index=FALSE, unique=FALSE, multiple=FALSE) {
  #check data
  if(is.null(x)) {stop(call. =FALSE, 'x is missing')}
  if(!is.numeric(x)) {stop(call. =FALSE, 'x is not numeric')}
  if(k>length(unique(x)) & unique) {stop(call. =FALSE, 'k is larger than the number of unique values in x')}
  if(k>length(x) & !unique) {stop(call. =FALSE, 'k is larger than the number of values in x')}

  #identify the kth highest/lowest value(s)
  sorted<-sort(x[!is.na(x)], decreasing=highest)
  if (unique) sorted<-unique(sorted)
  val<-sorted[k]
  ind<-1:length(x)
  ind<-ind[x==val]
  if(multiple==FALSE & length(ind)>1){
    s<-sample.int(n=length(ind), size=1)
    ind<-ind[s]
  }
  if(index==TRUE){return(ind)} else {return (val)}
}

