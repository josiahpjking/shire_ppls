require(tidyverse)
require(magrittr)
require(conflicted)
conflict_prefer("filter","dplyr")
source("functions/make_tcplot_data.R")
source("functions/tcplot.R")
source("functions/tcplot_nolines.R")
mround<-function (x, base) {base * round(x/base)}
#1024 * 700
# group names = X_n where X is condition (fluency * ref pos) and n is sample size/4


#read in the mousetracking data
#rawdata <- bind_rows(read_csv("~/Downloads/1052-v3-trials.csv"),read_csv("~/Downloads/1052-v2-trials.csv")) %>% 
#  bind_rows(.,read_csv("~/Downloads/1052-v1-trials.csv"))
rawdata <- read_csv("~/Downloads/1052-v5-trials (1).csv") %>%
  bind_rows(., read_csv("1052-v4-trials.csv")) %>%
  bind_rows(., read_csv("1052-v3-trials.csv")) %>%
  bind_rows(., read_csv("1052-v2-trials.csv")) %>%
  bind_rows(., read_csv("1052-v1-trials.csv"))


rawdata %>% group_by(Participant, Trial) %>% 
  summarise(
    trialstart = first(`Trial Start`),
    trialstart1 = as.POSIXct(as.integer(as.numeric(as.character(trialstart)) / 1000.0), origin='1970-01-01', tz="GMT"),
    trialelapse = max(Elapsed),
    condition = first(Condition),
    n=n(),
    datapoints = sum(`Event Type`=="move")) -> trials
rawdata %>% group_by(Participant) %>%
  summarise(
    totaltrials = n_distinct(Trial),
    browser=first(Browser),
    version=first(Version),
    os=first(OS),
    list=first(List)
    ) %>% mutate(
      browser = ifelse(Participant%in%c(51,53,55),NA,browser),
      version = ifelse(Participant%in%c(51,53,55),NA,version),
      os = ifelse(Participant%in%c(51,53,55),NA,os)
    ) -> ppts

trials %>% group_by(Participant) %>%
  summarise(
    empty_trials = sum(n==1)
  ) %>% left_join(ppts,.) %>% select(Participant, list, browser,version,os,totaltrials,empty_trials) %>%
  mutate(prop_empty = (empty_trials/totaltrials)) -> ppts

print(ppts)

ppts %>% group_by(browser,version,os) %>% 
  summarise(
    nr_ppts = n(),
    nr_full = sum(totaltrials==64),
    avg_prop_emptytrials=mean(prop_empty)
    )

trials %>% filter(datapoints>=1) %>% group_by(Participant) %>%
  summarise(
    first=min(trialstart),
    first1=as.POSIXct(as.integer(as.numeric(as.character(first)) / 1000.0), origin='1970-01-01', tz="GMT"),
    last=max(trialstart),
    last1=as.POSIXct(as.integer(as.numeric(as.character(last)) / 1000.0), origin='1970-01-01', tz="GMT")
  ) %>% left_join(ppts, .) %>% mutate(fail=ifelse(prop_empty==0,0,1)) %>%
ggplot(., aes(y=factor(Participant),col=factor(fail)))+#facet_wrap(~browser*version*os,scales="free_x")+
  geom_point(aes(x=first1,group=Participant))+
  geom_point(aes(x=last1,group=Participant))+
  ggtitle("start and end points for each participant (valid trials only).\nnotice last participant failed but clearly was not overlapping with others\nso can rule out it being issues due to ppts recording data simultaneously\n(which was the previous problem as far as i could tell)")



#####
#fix after browser, os, version etc..
####
rawdata %>% mutate(
   `image-left` = ifelse(is.na(`image-left`),Browser,`image-left`),
   `image-right` = ifelse(is.na(`image-right`),Version,`image-right`),
   audio = ifelse(is.na(audio),OS,audio)
) -> rawdata



#attention checks
rawdata %>% filter(Condition=="attention") %>% 
  mutate(
    refpos = ifelse(grepl("atta",`image-right`),"R","L")
  ) %>%
  group_by(Participant, Trial) %>% 
  summarise(
    clickedLR = last(Layer[`Event Type`=="click"]),
    clicked_pos = factor(ifelse(!(grepl("left|right",clickedLR)),"none",
                            ifelse(grepl("left",clickedLR),"L",
                                   ifelse(grepl("right",clickedLR),"R")))),
    clicked = ifelse(clicked_pos==first(refpos), 1, 0)
  ) %>% group_by(Participant) %>%
  summarise(
    att_check = sum(clicked)/n()
  ) %>% select(Participant, att_check) %>% print() -> ppt_att_check

#any ppts clicking more on one side than the other?
rawdata %>% group_by(Participant, Trial) %>% 
  summarise(
    clickedLR = last(Layer[`Event Type`=="click"]),
    clicked_pos = factor(ifelse(!(grepl("left|right",clickedLR)),"none",
                                ifelse(grepl("left",clickedLR),"L",
                                       ifelse(grepl("right",clickedLR),"R"))))
  ) %>% group_by(Participant) %>%
  summarise(
    Lside_clicks = sum(clicked_pos=="L")/n()
  ) %>% select(Participant, Lside_clicks) %>% print()


#tidy data, then..
tdat <- rawdata %>% 
  mutate(
    X=X-512,
    Y=Y-350,
    item=Trial,
    refpos=substring(Condition,nchar(Condition)),
    Condition=substring(Condition,4,nchar(Condition)-1)
  ) %>% print()
#remove any who clicked on the non animal in any attention check trials
left_join(tdat, ppt_att_check) -> tdat 

####FILTER TO CRITICAL TRIALS
tdat %<>% filter(Condition!="Filler",Condition!="entio")

########
#EARLY EARLY CLICKS
########
#check that audio has begun playing in all trials (e.g. remove any who click immediately)
tdat %>% 
  group_by(Participant, Trial) %>%
  summarise(
    valid_trial = +("playbackstart"%in%`Event Type`)
  ) -> invalid_trials

#are there any participants who did it repeatedly?
invalid_trials %>% group_by(Participant) %>%
  summarise(
    valid_ppt=sum(valid_trial==1)/n()
  ) -> invalid_ppts
print(invalid_ppts)
#remove invalid trials
left_join(tdat, invalid_trials) %>% left_join(., invalid_ppts) -> tdat


########
#AUDIO DURATIONS
########
#join the times for each trial that audio starts
tdat %>% 
  group_by(Participant, Trial) %>%
  summarise(
    audio1_start = first(Elapsed[`Event Type`=="playbackstart"])
  ) %>%
  left_join(tdat, .) %>% print() -> tdat

#get the timings for each audio file.
audio<-read_csv("audio_durations.csv",col_names=c("file","duration")) %>%
  #these were just from soxi -D for all files, so I only want the initial fragments (and not the practice)
  filter(grepl("1.wav",file),!grepl("p1.wav",file)) %>%
  mutate(
    audio1_duration=duration*1000, 
    Condition=ifelse(grepl("f1.wav",file),"Fluent",
                     ifelse(grepl("d1.wav",file),"Disfluent","Filler")),
    item=ifelse(Condition=="Filler",substring(file,3,nchar(file)-6),substring(file,3,nchar(file)-7))
  ) %>% select(audio1_duration,item,Condition) %>% print()

#join audio durations and make time variable centered on referent onset.
left_join(tdat, audio) %>%
  mutate(
    audio1_end = audio1_start+audio1_duration,
    time = Elapsed-audio1_end
  ) -> tdat

########
#EARLY CLICKS
########
#click time
tdat %>% group_by(Participant, Trial) %>%
  summarise(
    clicktime = last(time[`Event Type`=="click"])
  ) %>% left_join(tdat,.) -> tdat

#are there any participants who did it repeatedly before noun onset?
tdat %>% group_by(Participant, Trial) %>%
  summarise(
    clicktime=first(clicktime)
  ) %>% group_by(Participant) %>% 
  summarise(
    earlyclicks=sum(clicktime<=0,na.rm=T)/n()
    ) %>% left_join(tdat,.) -> tdat


####FILTER TO CRITICAL TRIALS
tdat %<>% filter(Condition!="Filler",Condition!="entio")

########
#MOVEMENTS BEYOND OUTER EDGE
########
#what is the outer edge of the image?
#center of image is 0.15*1024
#so outside edge is 0.15*1024-(150/2)
#relative to screen center, this is 512-(0.15*1024-(150/2))
outer_edge = 512-(0.15*1024-(150/2))
tdat %<>% mutate(
  outside = factor(ifelse(abs(X)>outer_edge,1,0))
) 
#how many samples?
tdat %>% filter(`Event Category`=="mouse", time >= 0) %>% select(outside) %>% table() %>% prop.table()
#get rid of them.?????
#tdat %<>% filter(outside==0, `Event Category`=="mouse")   # this removes all outside movements which occur before ref onset too...
########
#CHECK OBJ CLICKED
########
tdat %>%  
  group_by(Participant, Trial) %>%
  summarise(
    fluency = factor(first(Condition)),
    refpos = first(refpos),
    rt = last(time[`Event Type`=="click"]),
    clickedLR = last(Layer[`Event Type`=="click"]),
    clicked = factor(ifelse(!(grepl("left|right",clickedLR)),"none",
                       ifelse(grepl("left",clickedLR) & refpos=="L","ref",
                              ifelse(grepl("right",clickedLR) & refpos=="R","ref","dis"))))
  ) -> object_clicks
#object_clicks %>% ungroup() %>% select(clicked,fluency) %>% table()
left_join(tdat, object_clicks) -> tdat


########
#BINNING THE DATA
########
#okay, so we need to bin the data. I don't know if there's a nice equivalent of padr::pad() for numeric vector. 
#there must be, but I can't find it.

#round the timestamps to the nearest 20, and then average the X,Y positions
tdat %>% filter(`Event Category`=="mouse") %>% select(Participant,Trial,Condition,X,Y,time) %>% 
  mutate(
   time=mround(time,20)
  ) %>% group_by(Participant, Trial,time) %>%
  summarise(
    X=mean(X),
    Y=mean(Y)
  ) -> xy_data

#now create an empty data set for each trial for each ppt, with rows from min to max time in each trial by 20ms
tdat %>% filter(!is.na(time)) %>%
  mutate(
    time=mround(time,20)
  ) %>% group_by(Participant, Trial) %>%
  summarise(
    time=list(seq(min(time),max(time),20))
  ) %>% unnest() %>%    #i love the unnest function!
  left_join(., xy_data) %>%     #join with data.  
  fill(.,c("X","Y")) %>%      #fill out X and Y values with most recent value.
  filter(time>=0) %>%         #remove all movements prior to referent onset. 
  print() -> tdat_binned

########
#CALCULATE DISTANCE TRAVELLED & CUMULATIVE DISTANCE & TOWARDS REF/DIS
########
#make distances 
tdat_binned %>%
  group_by(Participant,Trial) %>%
  summarise(
    time = list(time[-c(1)]),
    x = list(X[-c(1)]),
    distance_travelled = list(diff(X,lag=1)),
    cumulative_distance = list(cumsum(abs(diff(X,lag=1))))
  ) %>% unnest() %>%
  left_join(tdat_binned,.) %>% print() -> tdat_binned



#sort out whether distance travelled is toward ref or dis
tdat %>% select(Participant, Trial, Condition, refpos) %>% unique() %>%
  left_join(tdat_binned, .) %>%
  mutate(
    Ldist = ifelse(distance_travelled<=0,abs(distance_travelled),0),
    Rdist = ifelse(distance_travelled>=0,distance_travelled,0),
    ref_dist = ifelse(refpos=="L",Ldist,Rdist),
    dis_dist = ifelse(refpos=="L",Rdist,Ldist)
  ) -> tdat_binned 

tdat_binned %>% 
  filter(!is.na(x)) %>%
  group_by(Participant, Trial) %>%
    summarise(
      time = list(time),
      ref_cdist = list(cumsum(ref_dist)),
      dis_cdist = list(cumsum(dis_dist))
    ) %>% unnest() %>%
  left_join(tdat_binned, .) %>% print() -> tdat_binned

#replace the NAs created at time==0 with 0.
tdat_binned %<>% mutate_at(vars(contains("dist")),funs(ifelse(time==0,0,.)))

tdat_binned %<>%
  mutate(
    refprop = ifelse(cumulative_distance==0,0,ref_cdist/cumulative_distance),
    disprop = ifelse(cumulative_distance==0,0,dis_cdist/cumulative_distance)
  )



tdat %>%
  group_by(Participant, Trial) %>% summarise(
  att_check = first(att_check),
  valid_trial= first(valid_trial),
  earlyclicks = first(earlyclicks),
  valid_ppt = first(valid_ppt),
  clicked=first(clicked),
  Browser = first(Browser),
  Version=first(Version),
  OS = first(OS),
  any_valid = ifelse(valid_trial==0,0,
                   ifelse(clicktime<=0,0,1))
) -> ppt_trial_infos

######
#Remove idiots
#filter(att_check==1) -> tdat
#remove early trials
#filter(valid_trial==1)
#remove early ppts
#filter(valid_ppt==1)
#####
#filter(rt>=200, clicked!="none")

ppt_trial_infos %>% group_by(Participant) %>% 
  summarise(
    crit_trials = n_distinct(Trial),
    att_check = first(att_check),
    p_click_postnoun=sum(any_valid)/n(),
    Browser = first(Browser),
    Version=first(Version),
    OS = first(OS)
  ) %>% mutate(
    include=ifelse(att_check>=.75 & p_click_postnoun>=.8,1,0)
  ) %>% select(Participant,include) -> ppt_infos

left_join(ppt_infos,ppt_trial_infos) %>% 
  filter(include==1, any_valid==1) %>% 
  group_by(Participant) %>%
  summarise(
    crittrials=n()
  )


######
#PLOT
########
left_join(tdat_binned,ppt_infos) %>% left_join(.,ppt_trial_infos) %>%
  filter(include==1, any_valid==1) %>% 
  mutate(
    CURRENT_BIN = time/20,
    group=factor("1")
  ) %>% as.data.frame() %>%
  make_tcplot_data(.,AOIs=c("refprop","disprop"),predictor = "Condition") %>%
  mutate(
    Object = fct_recode(AOI,"Distractor"="disprop","Referent"="refprop")
  ) %>%
  tcplot_nolines(.,0,2000,lcol="Object")+facet_wrap(~Condition)+
  ylab("proportion cumulative movement towards objects")

#require(gganimate)
#tdat %>% filter(Participant==2, Trial=="kangaroo", !is.na(X), !is.na(Y)) %>%
#  ggplot(.,aes(x=X,y=Y)) + geom_point() + xlim(-512,512)+ylim(-300,300) +
#  transition_time(Elapsed) +
#  ease_aes('linear') -> p

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


model_mouse<-lmer(elog_bias~fluency*time_s+(1+fluency+time_s|sub)+(1+fluency+time_s|ref),aggdat)
summary(model_mouse)

aggdat %>% mutate(
  CURRENT_BIN = time_s/0.02,
  fitted=fitted(model_mouse)
) %>% make_tcplot_data(.,c("elog_bias","fitted"),"fluency") %>%
  tcplot_nolines(.) + ylim(-5,5) +facet_wrap(~fluency) -> modplotmouse
