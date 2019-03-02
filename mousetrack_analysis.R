#source("mousetrack_processing.R")

######
#PLOT
########
tdat_binned %>% filter(include_ppt=="valid",include_trial=="valid",duplicate!="duplicate") %>%
  mutate(CURRENT_BIN = time/20) %>% 
  make_tcplotdata(.,AOIs=c(refprop,disprop),subj=Participant,Condition,duplicate) %>%
  mutate(
    Object = fct_recode(AOI,"Distractor"="disprop","Referent"="refprop")
  ) %>% 
  tcplot(lty=Condition)+facet_wrap(~duplicate)+
  ylab("proportion cumulative movement towards objects")

#elog bias plot
tdat_binned %>% 
  filter(include_ppt=="valid",include_trial=="valid",duplicate=="n-dup") %>% 
  mutate(
    CURRENT_BIN=time/20,
    Relog = log(refprop + .5/ (1 - refprop + .5)),
    Delog = log(disprop + .5/ (1 - disprop + .5)),
    elog_bias = Relog - Delog
  ) %>% make_tcplotdata(.,elog_bias,Participant,Condition) %>% 
  tcplot(lty=Condition)+ylim(-.5,1.5)+xlim(0,800)+
  stat_smooth(method=lm,col="black",fill="grey30")+
  NULL


#require(gganimate)
#tdat %>% filter(Participant==2, Trial=="kangaroo", !is.na(X), !is.na(Y)) %>%
#  ggplot(.,aes(x=X,y=Y)) + geom_point() + xlim(-512,512)+ylim(-300,300) +
#  transition_time(Elapsed) +
#  ease_aes('linear') -> p

tdat %>% group_by(Participant, Trial) %>%
  summarise(
    clicked=first(clicked),
    condition=first(condition)
  ) %>% group_by(condition) %>%
  summarise(
    refclicks=sum(clicked=="ref")/n()
  )

require(lme4)
object_clicks %<>% filter(clicked!="none",rt>=200)
object_clicks$fluency<-relevel(object_clicks$fluency,ref="Fluent")
contrasts(object_clicks$fluency)<-c(-.5,.5)
contrasts(object_clicks$clicked)

OC_model <- glmer(clicked~fluency+(1+fluency|Participant)+(1+fluency|Trial),object_clicks, family="binomial")




########
#MODEL
########
model.data <- tdat_binned %>% mutate(
  time_s = time/1000,
  sub = Participant,
  ref = Trial,
  fluency = Condition,
  Cref = refprop,
  Cdis = disprop
) %>% filter(time_s<=0.8)

aggdat = with(model.data, aggregate(Cref~time_s*fluency*sub*ref, FUN=sum, na.rm=T))
aggdat$Cdis <- with(model.data, aggregate(Cdis~time_s*fluency*sub*ref, FUN=sum, na.rm=T))[,5]
aggdat$N <- 1
aggdat <- aggdat %>% mutate(
  Relog = log(Cref + .5/ (N - Cref + .5)),
  Delog = log(Cdis + .5/ (N - Cdis + .5)),
  elog_bias = Relog - Delog,
  C_difference = abs(Cref - Cdis),
  wts =  1/(C_difference + .5) + 1/(N - C_difference + .5),
  fluency = factor(fluency)
)
aggdat$fluency<-relevel(aggdat$fluency,ref="Fluent")
#contrasts(aggdat$fluency)<-c(-.5,.5)
require(lme4)
model_mouse<-lmer(elog_bias~fluency*time_s+(1+fluency+time_s|sub)+(1+fluency+time_s|ref),aggdat)
summary(model_mouse)

aggdat %>% mutate(
  CURRENT_BIN = time_s/0.02,
  fitted=fitted(model_mouse)
) %>% make_tcplot_data(.,c("elog_bias","fitted"),"fluency") -> plotdat

ggplot(plotdat,aes(x=time,col=fluency,fill=fluency))+
  geom_point(data=plotdat[!grepl("fitted",plotdat$AOI),],aes(y=mean_prop))+
  geom_ribbon(data=plotdat[!grepl("fitted",plotdat$AOI),],aes(ymin=low,ymax=up),col=NA,alpha=0.2)+
  geom_line(data=plotdat[grepl("fitted",plotdat$AOI),],aes(y=mean_prop))+
  scale_colour_manual(values=c("#2171B5","#31A354"))+
  scale_fill_manual(values=c("#2171B5","#31A354"))+
  ylab("Empirical logit transformed movement bias toward\nreferent over distractor")+
  xlab("Time (ms) relative to referent onset")+
  theme_bw()+
  #facet_wrap(~fluency)+
  theme(text = element_text(size=16),legend.position = "bottom")+
  ggtitle("Mouse movements")+
  NULL 
