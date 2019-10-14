# the aim is to end up with a list of layers (what image/audio is in which place) in each trial 
# and a list of trials for each participant
# randomisation requires hard-coding randomisation, so that you may have to define layers for a set of Nppt * Ntrial trials. 

# situation 1 same trials, random trial order
# Let's say you want 24 participants, and you have 20 trials. you need a list of 20 trials, 
# and a list of 24 participant lists, each participant list has a random ordered set of the 20 trials.

# situation 2 counterbalance layer (e.g., position), same trial order
# for all participants, you want 10 trials with a target image on the Left, and 10 with a target image on the Right.
# what is more, you want these to be counterbalanced (e.g., participants 1:12 see trials 1:10 on the left, and participants 13:24 see trials 1:10 on the right (and vice versa for trials 11:20)).
# you will need a list of 40 trials (each of the 20 trials, once with target layer as Left, once as Right)
# 2 participant lists, one with trials 1:10 Left, 11:20 Right, and the other with trials 1:10 Right, 11:20 Left.


#situation 3 counterbalance layers, random trials
# the same, but with 24 participant lists.

#situation 4 counterbalance and randomised layers, random trials
#you want to counterbalance some layers (Left/Right), randomise others, and randomise trials.


critical_targets

filler_targets

critical_conditions

filler_conditions

randomise L R

counterbalance critical group lists 

counterbalance/randomize filler group lists

bind



require(tidyverse)
all_images = list.files("~/Desktop/git_repositories/shire_ppls/construction/images/")
dist_images = all_images[grepl("^d",all_images)]
targ_images = all_images[grepl("^t",all_images)]
fill_targ_images = all_images[grepl("^fr",all_images)]

crit_items = gsub("t_|.png","",targ_images)
filler_items = gsub("fr_|.png","",fill_targ_images)
crit_conditions = c("FluentL","FluentR","DisfluentL","DisfluentR")
filler_conditions = c("FillerL","FillerR")

#what do you want to counterbalance? put these in the conditions 
#(e.g., if you have experimental condition Fluent, Disfluent, and you want to also counterbalance L/R, you'll need 4).

make_ptlist<-function(crit_items,crit_conditions,filler_items,filler_conditions){
  ncc=length(crit_conditions)
  nfc=length(filler_conditions)
  nci=length(crit_items)
  nfi=length(filler_items)
  
  expand.grid(crit_items,crit_conditions) %>% 
    rename(Trial = Var1,
           Condition = Var2) %>% arrange(Trial,Condition) %>%
    mutate(
      group = map(1:ncc,~if(.==1){1:ncc}else{c(.x:ncc,1:(.x-1))}) %>% map(.,~rep(.,nci/ncc)) %>% unlist
    ) -> grouplists
  
  expand.grid(filler_items,filler_conditions,1:2) %>% 
    rename(Trial = Var1,
           Condition = Var2) %>% arrange(Trial,Condition) %>% select(-Var3) %>%
    mutate(
      group = map(1:ncc,~if(.==1){1:ncc}else{c(.x:ncc,1:(.x-1))}) %>% map(.,~rep(.,nfi/ncc)) %>% unlist
    ) -> fgrouplists
  
  bind_rows(grouplists, fgrouplists) %>% arrange(group)
}

make_ptlist(crit_items,crit_conditions,filler_items,filler_conditions) ->d

#240
#60 trials, 4 lists




