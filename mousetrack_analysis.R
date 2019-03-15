require(lme4)
#source("mousetrack_processing.R")

ppt_info %>% filter(include_ppt=="valid") %>%
  mutate(
    bilingual2 = fct_recode(factor(bilingual), 
                            "Monolingual"="No",
                            "Monolingual"="No - Only English",
                            "Bilingual"="Yes - English and some other language",
                            "Non-native"="Yes - Some other language and not English",
                            "maybe"="Yes")
  ) %>% select(duplicate2, bilingual2) %>% table

######
#PLOT
########
tdat_binned %>% filter(include_ppt=="valid",include_trial=="valid",grepl("No",bilingual)) %>%
  mutate(CURRENT_BIN = time/20,
         referent=refprop,
         distractor=disprop) %>% 
  make_tcplotdata(.,AOIs=c(referent,distractor),subj=Participant,Condition,duplicate2) %>%
  mutate(
    Object = fct_recode(AOI,"Distractor"="disprop","Referent"="refprop")
  ) %>% 
  tcplot(lty=Condition)+facet_wrap(~duplicate2)+
  ylab("proportion cumulative movement towards objects")

#elog bias plot
tdat_binned %>% 
  filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>% 
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

tdat %>% filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>%
  group_by(Participant, Trial) %>%
  summarise(
    clicked=first(clicked),
    condition=first(condition),
    rt=first(rt)
  ) %>% group_by(condition) %>%
  summarise(
    refclicks=sum(clicked=="ref")/n(),
    mean_rt=mean(rt,na.rm=T)
  )


left_join(object_clicks,ppt_info) %>% left_join(.,ppt_trial_info) %>%
  filter(clicked!="none",
         rt>=200,
         fluency!="Filler",
         include_ppt=="valid",
         duplicate2=="n-dup"
         ) %>% ungroup -> object_clicks
droplevels(object_clicks) -> object_clicks
object_clicks$fluency<-relevel(object_clicks$fluency,ref="Fluent")
contrasts(object_clicks$fluency)<-c(-.5,.5)
contrasts(object_clicks$clicked)
object_clicks %>% select(fluency,clicked) %>% table %>% print %>% prop.table(.,margin=1)

OC_model <- glmer(clicked~fluency+(1+fluency|Participant)+(1+fluency|Trial),object_clicks, family="binomial")
summary(OC_model)


########
#MODEL
########
model.data <- tdat_binned %>%
  filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>%
    mutate(
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
  fluency = factor(fluency),
  time_z = scale(time_s)[,1]
)
aggdat$fluency<-relevel(aggdat$fluency,ref="Fluent")
contrasts(aggdat$fluency)[,1]<-c(-.5,.5)

model_mouse<-lmer(elog_bias~fluency*time_s+(1+fluency*time_s|sub)+(1+time_s|ref),aggdat)
summary(model_mouse)

aggdat %>% mutate(
  CURRENT_BIN = time_s/0.02,
  fitted=fitted(model_mouse)
) %>% make_tcplotdata(.,c(elog_bias,fitted),sub,fluency) -> plotdat

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
