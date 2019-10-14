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
ppt_info <- bind_rows(MTURKppt_info, ppt_info) %>% 
  separate(Participant, c("cohort","Participant"),"_") %>%
  filter(
    !(total_trials<=55 & cohort=="LAB"), #these 2 are Vilde,
    !(cohort=="Mturk" & as.numeric(Participant)<=87) #these are the initial testing phase
  )

ppt_trial_info <- bind_rows(MTURKppt_trial_info, ppt_trial_info)  %>% 
  separate(Participant, c("cohort","Participant"),"_") %>%
  left_join(.,ppt_info %>% select(total_trials,Participant,cohort,bilingual2,duplicate2,include_ppt)) %>%
  filter(
    !(total_trials<=55 & cohort=="LAB"), #these 2 are Vilde,
    !(cohort=="Mturk" & as.numeric(Participant)<=87) #these are the initial testing phase
  )

mtrack_data <- bind_rows(MTURKtdat_binned,tdat_binned) %>% 
  separate(Participant, c("cohort","Participant"),"_") %>%
  filter(
    !(total_trials<=55 & cohort=="LAB"), #these 2 are Vilde,
    !(cohort=="Mturk" & as.numeric(Participant)<=87) #these are the initial testing phase
  )

object_clicks<-bind_rows(MTURKclicks, object_clicks) %>% 
  separate(Participant, c("cohort","Participant"),"_") %>%
  filter(
    !(total_trials<=55 & cohort=="LAB"), #these 2 are Vilde,
    !(cohort=="Mturk" & as.numeric(Participant)<=87) #these are the initial testing phase
  )

rm(list=setdiff(ls(), c("ppt_info","ppt_trial_info","mtrack_data","object_clicks")))


##########

ppt_info %>% mutate(
  criterion1_total_trials55 = ifelse(total_trials>=55,1,0),
  criterion2_crittrials15 = ifelse(crit_nonempty_trials>=10,1,0),
  criterion3_attention_check2 = ifelse(att_check>=.5,1,0),
  criterion4_pclick10 = ifelse(p_clickprenoun_nonempty<=10,1,0),
  criterion5_Lsideclicks10 = ifelse(Lside_clicks>=.1 & Lside_clicks<=.9,1,0),
  criterion6_avgclicktime200 = ifelse(avg_clicktime>=200,1,0)
) -> ppt_info_tab

criteria_vars=names(ppt_info_tab)[grepl("criterion",names(ppt_info_tab))]

ppt_info_tab %>% filter(cohort=="Mturk",duplicate2=="n-dup") %>%
  tableone::CreateCatTable(data=.,vars=c("bilingual2")) %>%
  print(showAllLevels=T)


ppt_info_tab %>% filter(bilingual2=="Monolingual", duplicate2=="n-dup") %>%
  tableone::CreateCatTable(data=.,strata="cohort",vars=c(criteria_vars))

ppt_info_tab %>% filter(bilingual2=="Monolingual", duplicate2=="n-dup") %>%
  tableone::CreateCatTable(data=.,strata="cohort",vars=c("include_ppt"))

######
mtrack_data %<>% filter(bilingual2=="Monolingual",duplicate2=="n-dup",include_ppt=="valid")
ppt_info %<>% filter(bilingual2=="Monolingual",duplicate2=="n-dup",include_ppt=="valid")
object_clicks %<>% filter(bilingual2=="Monolingual",duplicate2=="n-dup",include_ppt=="valid")
ppt_trial_info  %<>% filter(bilingual2=="Monolingual",duplicate2=="n-dup",include_ppt=="valid")
###
ppt_trial_info %>% mutate(
  rtval=ifelse(clicktime>=200 & !is.na(clicktime),1,0),
  clickval=ifelse(clicked!="none",1,0),
  datapoints=ifelse(datapoints!=0,1,0),
  audio=ifelse(audio_played==1,1,0)
) %>% group_by(cohort,rtval,clickval,datapoints,audio) %>% count
#71 trials invalid
#9 lab, because no clicks (and therefore no RT)
#62 mturk:
# 13 no datapoints at all (errors with recording)
# 5 no audio (so no RT val) and no timelocking
# 35 with no clicks (so no RT)
# 6 with no RT or RT<200
# 3 with no click recorded, but an RT was... weird..

#how long did people take?
ppt_info %>% 
  group_by(cohort) %>%
  summarise(
    med_time=median(time_taken),
    min_time=min(time_taken),
    max_time=max(time_taken)
  )

#what hours do mturkers work? :)
ppt_info$hourstart = lubridate::hour(ppt_info$first_time1)
ggplot(ppt_info,aes(x=hourstart))+geom_histogram(bins=24) + facet_wrap(~cohort)-> timeofday
timeofday
source("https://raw.githubusercontent.com/josiahpjking/jkr/master/R/make_tcplotdata.R")
source("https://raw.githubusercontent.com/josiahpjking/jkr/master/R/BSmake_tcplotdata.R") #for resampled 95% CIs
source("https://raw.githubusercontent.com/josiahpjking/jkr/master/R/tcplot.R")
######
#PLOT
########

mtrack_data %>% 
  filter(time<=5000) %>%
  mutate(CURRENT_BIN = time/20,
         referent=refprop,
         distractor=disprop) %>% 
  make_tcplotdata(.,AOIs=c(referent,distractor),subj=Participant,Condition,cohort) %>%
  mutate(
    Object = fct_recode(AOI,"Distractor"="disprop","Referent"="refprop")
  ) %>% 
  tcplot(lty=Condition)+facet_wrap(~cohort)+xlim(0,2000)+
  ylab("proportion cumulative movement towards objects")

#elog bias plot
mtrack_data %>% 
  filter(include_trial=="valid") %>% 
  mutate(
    CURRENT_BIN=time/20,
    Relog = log(refprop + .5/ (1 - refprop + .5)),
    Delog = log(disprop + .5/ (1 - disprop + .5)),
    elog_bias = Relog - Delog,
    move=factor(ifelse(refprop>disprop,"ref",ifelse(disprop>refprop,"dist","neither"))),
    moveO=as.numeric(move)
  ) %>% make_tcplotdata(.,AOIs=c(moveO,elog_bias),Participant,Condition,cohort) %>% 
  tcplot(lty=Condition)+facet_wrap(cohort~AOI,scales="free_y")+
  xlim(0,800)+
  #stat_smooth(method=lm,col="black",fill="grey30")+
  NULL

#require(gganimate)
#tdat %>% filter(Participant==2, Trial=="kangaroo", !is.na(X), !is.na(Y)) %>%
#  ggplot(.,aes(x=X,y=Y)) + geom_point() + xlim(-512,512)+ylim(-300,300) +
#  transition_time(Elapsed) +
#  ease_aes('linear') -> p

object_clicks %>% 
  filter(include_trial=="valid") %>%
  group_by(Participant, Trial) %>%
  summarise(
    clicked=first(clicked),
    condition=first(condition),
    rt=first(clicktime)
  ) %>% group_by(condition) %>%
  summarise(
    refclicks=sum(clicked=="ref")/n(),
    mean_rt=mean(rt,na.rm=T)
  )



object_clicks %>%
  filter(clicked!="none",
         rt>=200) %>% 
  ungroup -> object_clicks

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
      move=factor(ifelse(refprop>disprop,"ref",ifelse(disprop>refprop,"dist","neither"))),
      moveO=as.numeric(move),
      fluency = factor(fluency),
      time_z = scale(time_s)[,1]
    ) %>% filter(time_s<=2.0) %>% ungroup -> aggdat

aggdat$fluency<-relevel(aggdat$fluency,ref="Fluent")
contrasts(aggdat$fluency)[,1]<-c(-.5,.5)

model_mouse<-lmer(elog_bias~fluency*time_z*cohort+(1+fluency*time_z|sub)+(1+time_z|ref),aggdat)
summary(model_mouse)

aggdat %>% mutate(
  CURRENT_BIN = time_s/0.02,
  fitted=fitted(model_mouse)
) %>% make_tcplotdata(.,c(elog_bias,fitted),sub,fluency,cohort) -> plotdat

ggplot(plotdat,aes(x=time,col=fluency,fill=fluency))+
  geom_point(data=plotdat[!grepl("fitted",plotdat$AOI),],aes(y=mean_prop))+
  geom_errorbar(data=plotdat[!grepl("fitted",plotdat$AOI),],aes(ymin=low,ymax=up),alpha=0.7)+
  geom_line(data=plotdat[grepl("fitted",plotdat$AOI),],aes(y=mean_prop))+
  scale_colour_manual(values=c("#2171B5","#31A354"))+
  scale_fill_manual(values=c("#2171B5","#31A354"))+
  ylab("Empirical logit transformed movement bias toward\nreferent over distractor")+
  xlab("Time (ms) relative to referent onset")+
  theme_bw()+
  facet_wrap(~cohort)+
  theme(text = element_text(size=16),legend.position = "bottom")+
  ggtitle("Mouse movements")+
  NULL 




#####
#GAMMs
#####
require(mgcv)
require(itsadug)

aggdat$ConditionO<-as.ordered(aggdat$Condition)
contrasts(aggdat$ConditionO)<-"contr.treatment"
aggdat$cohort<-as.ordered(aggdat$cohort)
contrasts(aggdat$cohort)<-"contr.treatment"

moc1A = bam(moveO~ConditionO+s(time)+s(time,by=ConditionO)+s(time,by=cohort),
              #s(Time,ref,by=Condition,bs="fs",m=1)+
              #s(Time,sub,by=Condition,bs="fs",m=1),
            data=aggdat,family=ocat(R=3))

plot(moc1A,select=2)
plot_diff(moc1A,"time",comp=list(ConditionO=c("Fluent","Disfluent")),rm.ranef=TRUE)
plot_smooth(moc1A,view="time", plot_all=c("cohort"),cond=list(ConditionO=c("Fluent")), rug=FALSE,rm.ranef=TRUE)

summary(moc1A)
gam.check(moc1A)

acf_resid(m2)
rho_val=acf_resid(m2)[2]
dat<-start_event(as.data.frame(dat),"Time",event=c("sub","ref"))
fmod<-update(m2,AR.start = dat$start.event,rho=rho_val)

plot_smooth(fmod,view="Time", plot_all="Condition", rug=FALSE,rm.ranef=TRUE)
plot_diff(fmod,"Time",comp=list(Condition=c("Fluent","Disfluent")),rm.ranef=TRUE)

summary(fmod)






