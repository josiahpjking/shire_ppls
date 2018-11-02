#' time course plots for eye tracking
#' @param df a dataframe in long format from the make_tcplot_data function
#' @param xmin,xmax min and max time boundaries. (ROI)
#' @param lcol = line colours variable, string.
#' @export
#' @examples
#' tcplot_nolines(plotting_data, -1000, 4000)
#' tcplot_nolines(plotting_data, -1000, 4000)+facet_wrap(~condition)
tcplot_nolines<-function(df,xmin=0,xmax=2000,lcol="AOI"){
  require(ggplot2)
  tplot = ggplot(data = df, aes_string(x = "time", y = "mean_prop", colour=lcol, fill=lcol))+
    xlim(xmin, xmax) + ylim(0, 1) + 
    xlab("Time (ms)") + ylab("Proportion of fixations to AOIs.") + 
    geom_line(lwd = 1.5) + 
    geom_ribbon(data = df, aes_string(x = "time", ymin = "low", ymax = "up"), colour = NA, alpha = 0.2, lwd = 1.5)
  return(tplot)
}
