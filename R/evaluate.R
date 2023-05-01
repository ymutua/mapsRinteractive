#' @title evaluate
#'
#' @description Computes evaluation measures from observed and predicted data.
#'
#' @param df Data.frame. Required. A data.frame with observed and predicted data.
#'
#' @param observed Character value. Required. The  name of the column in df
#' with predicted data.The data must be of class numeric.
#'
#' @param predicted Character value or vector. Required. The  names of the
#' column(s) in df with predicted data. The data  must be of class numeric.
#'
#' @return A data.frame with evaluation statistics. For details, see mri function.
#'
#' @examples
#' df<-data.frame(obs=1:9, pred=c(2, 9, 10, 8, 3, 4, 6, 12, 1))
#' e<-evaluate(df, 'obs', 'pred')
#' print(e)
#'
#' @export
evaluate<-function(df, observed, predicted){
  #create data.frame
  evaluation<-matrix(nrow=length(predicted), ncol=6, data=NA)
  evaluation<-as.data.frame(evaluation)
  names(evaluation)<-c('data', 'mae', 'rmse', 'e', 'r2', 'me')
  evaluation$data<-predicted

  #compute evalauation measures
  for (j in predicted){
    o<-df[,observed]
    p<-df[,j]
    evaluation[evaluation$data==j,'mae']<-signif(mae(o, p), 2)
    evaluation[evaluation$data==j,'rmse']<-signif(rmse(o, p), 2)
    evaluation[evaluation$data==j,'e']<-round(e(o, p), 2)
    evaluation[evaluation$data==j,'r2']<-round(r2(o, p), 2)
    evaluation[evaluation$data==j,'me']<-signif(me(o, p), 2)
  }

  #return objects
  return(evaluation=evaluation)
}
