#' time course plots for eye tracking
#' @param df a dataframe in long format from the make_tcplot_data function
#' @param xmin,xmax min and max time boundaries. (ROI)
#' @param lcol = line colours variable, string.
#' @param ltype = line type variable, string.
#' @export
#' @examples
#' tcplot(plotting_data, -1000, 4000)
#' tcplot(plotting_data, -1000, 4000)+facet_wrap(~condition)
tcplot<-function(df,xmin=0,xmax=2000,col=AOI,lty=NULL,x=time,y=mean_prop,ymin=low,ymax=up){
  x=enquo(x)
  y=enquo(y)
  ymin=enquo(ymin)
  ymax=enquo(ymax)
  col=enquo(col)
  lty=enquo(lty)
  require(ggplot2)
  tplot = ggplot(data = df, aes(x = !!x, y = !!y, colour=!!col, fill=!!col, lty=!!lty))+
    xlim(xmin, xmax) + ylim(0, 1) + 
    xlab("Time") +
    geom_line(lwd = 1.5) + 
    scale_linetype_manual(values=1:6)+
    geom_ribbon(data = df, aes(x = !!x, ymin = !!ymin, ymax = !!ymax), colour = NA, alpha = 0.2, lwd = 1.5)+
    NULL

  return(tplot)
}
