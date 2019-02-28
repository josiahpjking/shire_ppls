#' time course plots for eye tracking computes avg proportion and by subject standard errors.
#' @param df dataframe
#' @param AOIs column names of AOIs (for eyelink trackers, the RIGHT_1_P, RIGHT_2_P,... columns)
#' @param subj participant column
#' @param ... variable names of experimental conditions
#' @param bin BIN variable (for eyelink, this is the CURRENT_BIN column). Defaults to "CURRENT_BIN".
#' @param data dataset
#' @export
#' @examples
#' plottingdata <- make_tcplotdata(df,AOIs=c(refprop,disprop),subj=Participant,fluency,gesture)
#' tcplot(plottingdata,0,2000,col="AOI",linetype="fluency")+facet_wrap(~gesture)
make_tcplotdata<-function(df,AOIs,subj,...,bin=CURRENT_BIN, bin_interval=20){
  subj=enquo(subj)
  bin=enquo(bin)
  AOIs=enquo(AOIs)
  df %>% gather(key="AOI",value="prop",!!(AOIs)) %>%
  group_by(AOI,!!subj,...,!!bin) %>%
    summarise(
      meanaoi=mean(prop),
    ) %>% group_by(...,!!bin,AOI) %>%
    summarise(
      mean_prop=mean(meanaoi),
      se=sd(meanaoi)/sqrt(n()),
      low=mean_prop-se,
      up=mean_prop+se
    ) %>% mutate(
      time=!!bin*bin_interval
    )
}

