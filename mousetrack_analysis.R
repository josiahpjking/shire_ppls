require(lme4)
source("mousetrack_processing.R")
MTURKppt_info<-ppt_info %>%
  mutate(
  Participant=paste0("Mturk_",Participant)
  )
MTURKppt_trial_info<-ppt_trial_info %>% ungroup %>%
  mutate(
    Participant=paste0("Mturk_",Participant)
  )
MTURKtdat_binned<-tdat_binned %>% ungroup %>%
  mutate(
    Participant=paste0("Mturk_",Participant)
  )
MTURKclicks<-object_clicks %>% ungroup %>%
  mutate(
    Participant=paste0("Mturk_",Participant)
  )
source("mousetrack_processing_LAB.R")
ppt_info <- ppt_info %>% mutate(
  Participant=paste0("LAB_",Participant)
)
ppt_trial_info <- ppt_trial_info %>% ungroup %>% mutate(
  Participant=paste0("LAB_",Participant)
)
tdat_binned <- tdat_binned %>% ungroup %>% mutate(
  Participant=paste0("LAB_",Participant)
)
object_clicks<-object_clicks %>% ungroup %>%
  mutate(
    Participant=paste0("LAB_",Participant)
  )


## JOIN
ppt_info <- bind_rows(MTURKppt_info, ppt_info)
ppt_trial_info <- bind_rows(MTURKppt_trial_info, ppt_trial_info)
mtrack_data <- bind_rows(MTURKtdat_binned,tdat_binned)
object_clicks<-bind_rows(MTURKclicks, object_clicks)


ppt_info %>% filter(include_ppt=="valid") %>% 
  select(duplicate2, bilingual2) %>% table

#how long did people take?
ppt_info %>% filter(include_ppt=="valid", duplicate2=="n-dup") %>%
  pull(time_taken) %>% as.numeric() %>% summary

#what hours do mturkers work? :)
ppt_info$hourstart = lubridate::hour(ppt_info$first_time1)
ggplot(ppt_info,aes(x=hourstart))+geom_histogram(bins=24) -> timeofday
timeofday
source("~/Desktop/git_repositories/jkr/R/BSmake_tcplotdata.R")
######
#PLOT
########
mtrack_data <- mutate(mtrack_data,expt=ifelse(grepl("Mturk",Participant),"mturk","lab"))

mtrack_data %>% 
  filter(include_ppt=="valid",
         include_trial=="valid",
         grepl("No",bilingual),
         duplicate2=="n-dup") %>%
  mutate(CURRENT_BIN = time/20,
         referent=refprop,
         distractor=disprop) %>% 
  BSmake_tcplotdata(.,AOIs=c(referent,distractor),subj=Participant,Condition,expt,n=1000) %>%
  mutate(
    Object = fct_recode(AOI,"Distractor"="disprop","Referent"="refprop")
  ) %>% 
  tcplot(lty=Condition)+facet_wrap(~expt)+
  ylab("proportion cumulative movement towards objects")

#elog bias plot
tdat_binned %>% 
  filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>% 
  mutate(
    CURRENT_BIN=time/20,
    Relog = log(refprop + .5/ (1 - refprop + .5)),
    Delog = log(disprop + .5/ (1 - disprop + .5)),
    elog_bias = Relog - Delog
  ) %>% BSmake_tcplotdata(.,elog_bias,Participant,Condition,expt,n=1000) %>% 
  tcplot(lty=Condition)+facet_wrap(~expt)+
  ylim(-.5,1.5)+xlim(0,800)+
  stat_smooth(method=lm,col="black",fill="grey30")+
  NULL

#require(gganimate)
#tdat %>% filter(Participant==2, Trial=="kangaroo", !is.na(X), !is.na(Y)) %>%
#  ggplot(.,aes(x=X,y=Y)) + geom_point() + xlim(-512,512)+ylim(-300,300) +
#  transition_time(Elapsed) +
#  ease_aes('linear') -> p

mtrack_data %>% filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>%
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


object_clicks %>%
  filter(clicked!="none",
         rt>=200,
         include_ppt=="valid",
         duplicate2=="n-dup"
         ) %>% ungroup -> object_clicks
droplevels(object_clicks) -> object_clicks
object_clicks$fluency<-relevel(object_clicks$fluency,ref="Fluent")
contrasts(object_clicks$fluency)<-c(-.5,.5)
contrasts(object_clicks$clicked)
object_clicks %>% select(clicked) %>% table %>% print %>% prop.table()
object_clicks %>% select(fluency,clicked) %>% table %>% print %>% prop.table(.,margin=1)

OC_model <- glmer(clicked~fluency+(1+fluency|Participant)+(1+fluency|Trial),object_clicks, family="binomial")
summary(OC_model)


########
#MODEL
########
model.data <- mtrack_data %>%
  filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>%
    mutate(
      time_s = time/1000,
      sub = Participant,
      ref = Trial,
      fluency = Condition,
      Cref = refprop,
      Cdis = disprop,
      N=1,
      Relog = log(Cref + .5/ (N - Cref + .5)),
      Delog = log(Cdis + .5/ (N - Cdis + .5)),
      elog_bias = Relog - Delog,
      C_difference = abs(Cref - Cdis),
      wts =  1/(C_difference + .5) + 1/(N - C_difference + .5),
      fluency = factor(fluency),
      time_z = scale(time_s)[,1]
    ) %>% filter(time_s<=0.8) %>% ungroup -> aggdat

aggdat$fluency<-relevel(aggdat$fluency,ref="Fluent")
contrasts(aggdat$fluency)[,1]<-c(-.5,.5)

model_mouse<-lmer(elog_bias~fluency*time_z*expt+(1+fluency*time_z|sub)+(1+time_z|ref),aggdat)
summary(model_mouse)

aggdat %>% mutate(
  CURRENT_BIN = time_s/0.02,
  fitted=fitted(model_mouse)
) %>% make_tcplotdata(.,c(elog_bias,fitted),sub,fluency,expt) -> plotdat

ggplot(plotdat,aes(x=time,col=fluency,fill=fluency))+
  geom_point(data=plotdat[!grepl("fitted",plotdat$AOI),],aes(y=mean_prop))+
  geom_errorbar(data=plotdat[!grepl("fitted",plotdat$AOI),],aes(ymin=low,ymax=up),alpha=0.7)+
  geom_line(data=plotdat[grepl("fitted",plotdat$AOI),],aes(y=mean_prop))+
  scale_colour_manual(values=c("#2171B5","#31A354"))+
  scale_fill_manual(values=c("#2171B5","#31A354"))+
  ylab("Empirical logit transformed movement bias toward\nreferent over distractor")+
  xlab("Time (ms) relative to referent onset")+
  theme_bw()+
  facet_wrap(~expt)+
  theme(text = element_text(size=16),legend.position = "bottom")+
  ggtitle("Mouse movements")+
  NULL 




#####
#GAMMs
#####
require(mgcv)
require(itsadug)

mtrack_data %>% 
  filter(include_ppt=="valid",include_trial=="valid",duplicate2=="n-dup",grepl("No",bilingual)) %>% 
  mutate(
    CURRENT_BIN=time/20,
    Time=time,
    Relog = log(refprop + .5/ (1 - refprop + .5)),
    Delog = log(disprop + .5/ (1 - disprop + .5)),
    elog_bias = Relog - Delog,
    moving = factor(ifelse(refprop>.5,"ref",
                      ifelse(disprop>.5,"dis","neither"))),
    movingO = as.numeric(moving),
    sub = factor(Participant),
    ref = factor(Trial),
    Condition=factor(Condition)
  ) %>% filter(Time <= 2000) %>% ungroup-> dat
dat$ConditionO<-as.ordered(dat$Condition)
contrasts(dat$ConditionO)<-"contr.treatment"
dat$expt<-as.ordered(dat$expt)
contrasts(dat$expt)<-"contr.treatment"

moc1A = bam(movingO~Condition+s(Time)+s(Time,by=Condition),
              #s(Time,ref,by=Condition,bs="fs",m=1)+
              #s(Time,sub,by=Condition,bs="fs",m=1),
            data=dat,family=ocat(R=3))

moc1B = bam(movingO~s(Time)+s(Time,by=ConditionO),
            #s(Time,ref,by=Condition,bs="fs",m=1)+
            #s(Time,sub,by=Condition,bs="fs",m=1),
            data=dat,family=ocat(R=3))

plot(moc1A,select=2)
plot_diff(moc1A,"Time",comp=list(Condition=c("Fluent","Disfluent")),rm.ranef=TRUE)
plot_smooth(moc1A,view="Time", plot_all="Condition", rug=FALSE,rm.ranef=TRUE)

plot(moc1B,select=2)
plot_diff(moc1B,"Time",comp=list(ConditionO=c("Fluent","Disfluent")),rm.ranef=TRUE)
plot_smooth(moc1B,view="Time", plot_all="ConditionO", rug=FALSE,rm.ranef=TRUE)

dat %>% mutate(CURRENT_BIN = time/20) %>%
  make_tcplotdata(.,movingO,sub,Condition) %>% tcplot()+ylim(0,3)+
  facet_wrap(~Condition)

summary(moc1B)
gam.check(moc1B)


acf_resid(m2)
rho_val=acf_resid(m2)[2]
dat<-start_event(as.data.frame(dat),"Time",event=c("sub","ref"))
fmod<-update(m2,AR.start = dat$start.event,rho=rho_val)

plot_smooth(fmod,view="Time", plot_all="Condition", rug=FALSE,rm.ranef=TRUE)
plot_diff(fmod,"Time",comp=list(Condition=c("Fluent","Disfluent")),rm.ranef=TRUE)

summary(fmod)






