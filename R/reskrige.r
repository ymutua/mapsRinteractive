#' @title reskrige
#'
#' @description Regression kriging using a standardized variogram.
#'
#' @inheritParams mri
#'
#' @inheritParams ordkrige
#'
#' @return A list with 1) a raster layer with predicted values and 2) if
#' cross.validate=T, a SpatVector of points with predictions from a
#' leave-one-out cross-validation. For details, see mri function.
#'
#' @details This is the ordinary kriging function called by the mri function.
#' It uses a standardized semivariogram model and requires a raster template for which
#' predictions are made. For details, see documentation of the mri function.
#'
#' @export
reskrige<-function(x=NULL, y=NULL, z=NULL, field = NULL, edge = 0, filter = 1, resolution = NULL,
                   md = 'Sph', rg = NULL, ng = 0.1, check.data=TRUE, cross.validate=TRUE){

  #check input data
  if(check.data){
    a<-check(x=x, y=y, z=z, field=field, edge = edge, filter=filter, resolution=resolution)
    x<-a[[1]]; y<-a[[2]]; z<-a[[3]]; feedback<-a[[4]]
  }

  ##compute residuals
  z$res<-z$map-z$obs

  #convert to data.frame
  z.df<-as.data.frame(z, geom="XY")

  #compute range (argument rg) if not specified by user
  if(is.null(rg)) rg<-0.5*sqrt(expanse(y))

  ##parameterize standardized semivariogram model
  sill<-var(as.numeric(z$res), na.rm=T)
  vgmod<- vgm(psill = (1-ng)*sill, model= md, range = rg,nugget= ng*sill)

  #cross-validate
  if(cross.validate){
    for (i in 1:nrow(z)){
      gsmod<-gstat(formula=res~1, locations=~x+y, data=z.df[-i,], model=vgmod)
      z[i,'reskrig_cv']<-predict(gsmod, z.df[i,],debug.level=0)$var1.pred
    }
    z$reskrig_cv<-z$map-z$reskrig_cv
  }

  #residual kriging to raster
  crs(x)<-crs(z) #to fix a bug
  gsmod<-gstat(formula=res~1, locations=~x+y, data=z.df, model=vgmod)
  reskrigpred<-interpolate(x, gsmod,  fun=predict, debug.level=0)$var1.pred
  reskrig<-x-reskrigpred
  reskrig<-mask(reskrig, x)

  #return objects
  return(list(reskrig, z))
}

