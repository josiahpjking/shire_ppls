require(tidyverse)
require(magrittr)
require(conflicted)
conflict_prefer("filter","dplyr")
source("functions/make_tcplotdata.R")
source("functions/tcplot.R")
source("functions/tcplot_nolines.R")
mround<-function (x, base) {base * round(x/base)}
#1024 * 700
# group names = X_n where X is condition (fluency * ref pos) and n is sample size/4


#read in the mousetracking data
rawdata <- bind_rows(lapply(list.files(path="data/",pattern="1052-v",full.names=T), read_csv))
qdata <- bind_rows(lapply(list.files(path="qu_data/",pattern="1052-v",full.names=T), read_csv))

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
    att_checkN = n(),
    att_check = sum(clicked)/n()
  ) %>% print() -> ppt_att_check

rawdata %>% filter(!grepl("attention",Condition)) %>%
  left_join(., ppt_att_check) -> rawdata

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


####
#data proc
####
#tidy data, then..
tdat <- rawdata %>% 
  mutate(
    X=X-512,
    Y=Y-350,
    item=Trial,
    refpos=substring(Condition,nchar(Condition)),
    Condition=gsub('[[:digit:]]+|_', '', substring(Condition,1,nchar(Condition)-1))
  ) %>% print()

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


####FILTERS

#EARLY EARLY CLICKS
#check that audio has begun playing in all trials (e.g. remove any who click immediately)
tdat %>%
  group_by(Participant, Trial) %>%
  summarise(
    trialstart = first(`Trial Start`),
    audio_played = +("playbackstart"%in%`Event Type`),
    datapoints = sum(`Event Type`=="move"),
    clicktime = last(time[`Event Type`=="click"]),
    att_check = first(att_check),
    clicked=first(clicked),
    condition=first(Condition),
    Browser = first(Browser),
    Version=first(Version),
    OS = first(OS)
) %>% print -> ppt_trial_info

#are there any participants who did it repeatedly?
ppt_trial_info %>% group_by(Participant) %>%
  summarise(
    total_trials = n_distinct(Trial),
    empty_trials=sum(datapoints==0),
    crit_trials = n_distinct(Trial[condition!="Filler"]),
    crit_nonempty_trials = n_distinct(Trial[condition!="Filler" & datapoints!=0]),
    att_check = first(att_check),
    n_clickprenoun_nonempty = sum(!is.na(clicked) & datapoints!=0 & (clicktime<=0|audio_played==0),na.rm=T),
    p_clickprenoun_nonempty = sum(!is.na(clicked) & datapoints!=0 & (clicktime<=0|audio_played==0),na.rm=T)/total_trials*100,
    avg_clicktime = mean(clicktime,na.rm=T),
    first_time1=as.POSIXct(as.integer(as.numeric(as.character(min(trialstart[trialstart>946684800
]))) / 1000.0), origin='1970-01-01', tz="GMT"),
    last_time=as.POSIXct(as.integer(as.numeric(as.character(max(trialstart))) / 1000.0), origin='1970-01-01', tz="GMT"),
    time_taken=difftime(last_time,first_time1,units="mins"),
    Browser = first(Browser),
    Version=first(Version),
    OS = first(OS)
  ) %>% print -> ppt_info

qdata %>% filter(grepl("MTurk",Question)) %>% rename(
  mturk_id = `Answers...`
) %>% group_by(Participant) %>%
  summarise(mturk_id=first(mturk_id)) %>% right_join(.,ppt_info) %>% 
  mutate(
    duplicate = duplicated(mturk_id)
  ) %>% print -> ppt_info
qdata %>% filter(grepl("language",Question)) %>% rename(
  bilingual = `Answers...`
) %>% group_by(Participant) %>%
  summarise(bilingual=first(bilingual)) %>% right_join(.,ppt_info) %>% print -> ppt_info


workers<-read_tsv("mturkers.csv") %>% filter(!is.na(`WORKER ID`)) %>% mutate(mturk_id=substring(`WORKER ID`,3))
workers  %>%
  group_by(mturk_id) %>%
   summarise(
     nr_attempts = n(),
     batches=gsub("mtrack_loy ","",toString(BATCH)),
     comments=toString(COMMENTS)
   ) %>% filter(nr_attempts>1) -> dup_workers
# workers %>% mutate(
#   duplicate=duplicated(`WORKER ID`),
#   mturk_id=substring(`WORKER ID`,3)
# ) %>% filter(duplicate==TRUE,!is.na(mturk_id)) -> dup_workers
workers %>% filter(BATCH=="mtrack_loy 15") %>% pull(mturk_id) %>% duplicated() %>% any


#########
#INCLUSION CONDITIONS
ppt_info %<>% mutate(
  include_ppt = ifelse(total_trials>=55 & 
                         crit_nonempty_trials>=10 & 
                         att_check>=.5 &
                         p_clickprenoun_nonempty<=5 &
                         avg_clicktime>=200,"valid","invalid"),
  duplicate = ifelse(mturk_id %in% dup_workers$mturk_id, "duplicate",
                     ifelse(!is.na(mturk_id),"n-dup","unknown"))
  )

early_dups <- dup_workers %>% filter(grepl("GREENBERG|TWAIN",comments)) %>% pull(mturk_id)

ppt_info %>% group_by(mturk_id) %>% 
  summarise(
    nppt_nums = n(),
    pptnums = list(Participant),
    pptnums_str = toString(Participant),
    ppt_trials = toString(total_trials),
    ppt_valid = toString(include_ppt),
    ppt_first = min(which(total_trials>=30)),
    valid_nums = Participant[ppt_first]
  ) %>% filter(!is.na(mturk_id),nppt_nums>1) %>%
  mutate(
    valid_nums = ifelse(mturk_id %in% early_dups,NA,valid_nums)
  ) -> first_of_dups

ppt_info %>% mutate(
  duplicate2 = ifelse(duplicate=="duplicate" & Participant %in% first_of_dups$valid_nums,"n-dup",duplicate),
  dup_incl = paste0(include_ppt," : ",duplicate2),
  submitted = ifelse(is.na(mturk_id),NA,
                     ifelse(mturk_id %in% workers$mturk_id,"submitted","not submitted"))
) -> ppt_info

#take the first ppt num, unless we've got something like ppts 92,93. need conditional..


ppt_trial_info %<>% mutate(
  include_trial = ifelse(datapoints!=0 & 
                           clicked!="none" & 
                           clicktime>=200 & 
                           audio_played==1, "valid","invalid")
)

#ggplot(ppt_info, aes(y=factor(Participant),col=factor(att_check)))+
#  geom_point(aes(x=last_time,group=Participant))+facet_wrap(~include_ppt)

ppt_info %>% select(include_ppt) %>% table


require(plotly)
require(RColorBrewer)
ppt_info %>% filter(total_trials>=50) %>% mutate(
  text = paste(
    Participant,
    mturk_id,paste0("<b>include:</b>",include_ppt),
    paste0("<b>submitted:</b>",submitted),
    paste0("<b>duplicate:</b>",duplicate2),
    paste0("<b>p_earlyclick:</b>",p_clickprenoun_nonempty,"%"),
    paste0("<b>av clicktime:</b>",avg_clicktime),
    paste0("<b>total trials:</b>",total_trials),
    paste0("<b>non empty crit trials:</b>",crit_nonempty_trials),
    paste0("<b>att check:</b>",att_check),
    paste0("<b>time taken:</b>",time_taken),
    sep="<br>")
) %>% plot_ly(.,
        x=~last_time,
        y=~Participant,
        type="scatter",
        mode="markers",
        color=~factor(dup_incl),
        colors=brewer.pal(7,"Set1"),
        marker = list(size = 10),
        text=~text,
        hoverinfo="text"
        )


tdat_binned %<>% filter(Condition!="Filler") %>%
  left_join(.,ppt_info) %>% left_join(.,ppt_trial_info) #%>%
  #filter(include_ppt=="valid", include_trial=="valid")

tdat %<>% filter(Condition!="Filler") %>%
  left_join(.,ppt_info) %>% left_join(.,ppt_trial_info) #%>%
  #filter(include_ppt=="valid", include_trial=="valid")


