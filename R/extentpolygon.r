#' @title extentpolygon
#'
#' @description Create a SpatVector of polygons from the extent of a spatial object.
#'
#' @param x A spatial object.
#'
#' @details If x is projected, the SpatVector will also be projected
#'
#' @return SpatVector of polygons.
#'
#' @import 'terra'
#'
#' @export
extentpolygon<-function(x){
    e<-ext(x)
    x1<-as.numeric(e[1])
    x2<-as.numeric(e[2])
    y1<-as.numeric(e[3])
    y2<-as.numeric(e[4])
    coords<-rbind(c(x1,y1), c(x1, y2), c(x2,y2), c(x2, y1), c(x1,y1))
    names(coords)<-c('x', 'y')
    p<-vect(coords, "polygons")
    crs(p)<-crs(x)
    return(p)
  }
