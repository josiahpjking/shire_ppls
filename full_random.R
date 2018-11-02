require(tidyverse)
all_images = list.files("images")
dist_images = all_images[grepl("^d",all_images)]
targ_images = all_images[grepl("^t",all_images)]
fill_targ_images = all_images[grepl("^fr",all_images)]

critical_items = gsub("t_|.png","",targ_images)
filler_items = gsub("fr_|.png","",fill_targ_images)
conditions = c("FluentL","FluentR","DisfluentL","DisfluentR")
fconditions = c("FillerL","FillerR")

expand.grid(critical_items,conditions) %>% 
  rename(Trial = Var1,
         Condition = Var2) %>% arrange(Trial,Condition) %>%
  mutate(
    group = c(rep(1:4,5),rep(c(2:4,1),5),rep(c(3,4,1,2),5),rep(c(4,1:3),5))
  ) -> grouplists

grouplists %>% select(group,Condition) %>% table()

expand.grid(filler_items,fconditions,1:2) %>% 
  rename(Trial = Var1,
         Condition = Var2) %>% arrange(Trial,Condition) %>% select(-Var3) %>%
  mutate(
    group = c(rep(1:4,10),rep(c(2:4,1),10),rep(c(3,4,1,2),10),rep(c(4,1:3),10))
  ) -> fgrouplists

fgrouplists %>% select(group, Condition) %>% table()

everything <- bind_rows(grouplists, fgrouplists) 

everything %>% filter(group==1) %>% select(-group) -> group1
everything %>% filter(group==2) %>% select(-group) -> group2
everything %>% filter(group==3) %>% select(-group) -> group3
everything %>% filter(group==4) %>% select(-group) -> group4


nParticipants = 32 
nConditions = 4


ppt_conditions=data.frame(list())
for(j in 1:(nParticipants/nConditions)){
  everything %>% mutate(Condition=paste0(group,"_",j,Condition)) %>%
    select(-group) %>% 
    bind_rows(ppt_conditions, .) -> ppt_conditions
}

layers = c("image-left","image-right","audio")
ppt_conditions_layers=data.frame(list())
for(j in layers){
  ppt_conditions %>% mutate(Layer=j) %>%
    bind_rows(ppt_conditions_layers, .) -> ppt_conditions_layers
}
ppt_conditions_layers %>% arrange(Trial) %>%
  mutate(
    ppt = gsub("Fluent|Disfluent|Filler|R|L","",Condition),
    Value = ifelse(grepl("FluentL|DisfluentL",Condition) & Layer=="image-left",paste0("t_",Trial,".png"),
                   ifelse(grepl("FluentR|DisfluentR",Condition) & Layer=="image-right",paste0("t_",Trial,".png"),
                          ifelse(grepl("FillerL",Condition) & Layer=="image-left",paste0("fr_",Trial,".png"),
                                 ifelse(grepl("FillerR",Condition) & Layer=="image-right",paste0("fr_",Trial,".png"),
                                        ifelse(grepl("Filler",Condition) & Layer=="audio",paste0("f_",Trial,"_.wav"),
                                               ifelse(grepl("Fluent",Condition) & Layer=="audio",paste0("e_",Trial,"_f.wav"),
                                                      ifelse(grepl("Disfluent",Condition) & Layer=="audio",paste0("e_",Trial,"_d.wav"),NA)))))))
  ) -> ppt_conditions_layers

ppt_conditions_layers %>% select(ppt,Trial) %>% distinct() %>% arrange(ppt) -> ppt_trials

for(j in levels(factor(ppt_trials$ppt))){
  ppt_trials$distractors[ppt_trials$ppt==j]<-base::sample(dist_images)
}

head(ppt_trials)

left_join(ppt_conditions_layers,ppt_trials) %>% mutate(
  Value = ifelse(is.na(Value),distractors,Value)
) %>% select(-distractors,ppt) -> ppt_conditions_layers





printTrials=function(fulldata){
  for(j in unique(fulldata$Trial)){
    trialsdata <- fulldata %>% filter(Trial==j)
    cat(paste0('\t\t{\n\t\t\t"name": "',j,'",\n\t\t\t"conditions": [\n\t\t\t\t'))
    for(m in unique(trialsdata$Condition)){
      cat(paste0('
\t\t\t\t{
\t\t\t\t\t"name": "',m,'",
\t\t\t\t\t"variables": [
\t\t\t\t\t\t{
\t\t\t\t\t\t\t"layer": "image-left",
\t\t\t\t\t\t\t"value": "',trialsdata$Value[trialsdata$Condition==m & trialsdata$Layer=="image-left"],'"
\t\t\t\t\t\t},
\t\t\t\t\t\t{
\t\t\t\t\t\t\t"layer": "image-right",
\t\t\t\t\t\t\t"value": "',trialsdata$Value[trialsdata$Condition==m & trialsdata$Layer=="image-right"],'"
\t\t\t\t\t\t},
\t\t\t\t\t\t{
\t\t\t\t\t\t\t"layer": "audio",
\t\t\t\t\t\t\t"value": "',trialsdata$Value[trialsdata$Condition==m & trialsdata$Layer=="audio"],'"
\t\t\t\t\t\t}
\t\t\t\t\t]
\t\t\t\t},'))
    }
    cat(paste0('
\t\t\t]
\t\t},\n'))
  }
}

printTrials(ppt_conditions_layers)

sink("output_trials.txt")
cat(paste0('\t"trials": [\n'))
printTrials(ppt_conditions_layers)
cat(paste0('\n\t],'))
sink()



printList=function(trials,listname){
  prefix=paste0('		{\n\t\t\t"name": "',listname,'",\n\t\t\t"trials": [\n')
  lists = trials[sample(1:nrow(trials)),] %>%
    mutate(
      cc = paste0('				"',Trial,':',listname,Condition,'",\n')
    ) %>% pull(cc)
  suffix = '\t\t\t]\n\t\t},\n'
  cat(prefix)
  cat(lists)
  cat(suffix)
}

printList(group1,"1_1")






sink("output_lists.txt")
cat('\t"lists": [\n')
for (j in 1:(nParticipants/nConditions)){
  printList(group1,paste0(1,"_",j))
  printList(group2,paste0(2,"_",j))
  printList(group3,paste0(3,"_",j))
  printList(group4,paste0(4,"_",j))
}
cat('\t]')
sink()




##### in both files, find and replace ,\n\t\t\t] with \n\t\t\t]
