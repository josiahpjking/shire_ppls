#' time course plots for eye tracking
#' @param AOIs vector of string names of AOIs (for eyelink trackers, the RIGHT_1_P, RIGHT_2_P,... columns)
#' @param predictor variable name (string) of experimental condition.
#' @param bin BIN variable (for eyelink, this is the CURRENT_BIN column). Defaults to "CURRENT_BIN".
#' @param data dataset
#' @export
#' @examples
#' plottingdata <- make_tcplot_data(df=e6data, AOIs=c("easy_fix","diff_fix","vid_fix"),predictor="gesture")

make_tcplot_data<-function(df, AOIs, predictor, bin="CURRENT_BIN", bin_interval=20){
  plotdat<-as.data.frame(list())
  for (i in AOIs){
    x<-aggregate(df[,i],by=list(df[,bin], df[,predictor]),FUN=mean)
    x$se<-aggregate(df[,i],by=list(df[,bin], df[,predictor]),FUN=plotrix::std.error)[,3]
    x$low<-x$x-x$se
    x$up<-x$x+x$se
    x$object<-i
    plotdat<-rbind(plotdat,x)
  }
  names(plotdat)<-c("bin",predictor,"mean_prop","se","low","up","AOI")
  plotdat$time<-plotdat$bin*bin_interval
  plotdat$AOI<-factor(plotdat$AOI)
  return(plotdat)
}

