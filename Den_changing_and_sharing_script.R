##############################################################################################
# --- CODE OF THE MODEL TO ASSESS THE ROLE OF DENS IN THE SPREAD, INVASION AND PERSISTENCE---#
# --- OF SARCOPTIC MANGE IN THE SAN JOAQUIN FOX POPULATION LOCATED IN BAKERSFIELD, CA -------#
# --- SPECIFICALLY THIS SCRIPT DETERMINES THE CHANGE AND SHARE OF DENS PER FOX FAMILY -------#
##############################################################################################

# Author:Diego Montecino-Latorre
# dmontecino@ucdavis.edu
# Details of the model are published in the journal "Epidemics" January 2019


# For the parameters of the processes associated with den changing and den sharing refer to the script "Setup_model_script.R" located 
# in https://github.com/dmontecino/SJKF_Model/blob/master/Setup_model_script.R

# this script is run in the line 264 of the natural history script

if(sharing=='Yes'){ # if the model is concerned on den change and sharing then
  
  
  
  ##### DENS CHANGING PROCESS #####
  
  # Determining when (which weeks) fox families change the occupied dens during the current year for all families
  
  if(d==1){ # in the first week of the year
    
    #randomly defined when the foxes of each family will change the curent dens they are occupying
    fams.change.den=mapply(c, lapply(c(1:59), function(x){rbinom(n=18, size=1, prob = prob.den.change.breed.t.rearing.t.week.1.18)}), #january to april
                           lapply(c(1:59), function(x){1}), #first week of other period
                           lapply(c(1:59), function(x){rbinom(n=28, size=1, prob = prob.den.change.other)}), # May to November
                           lapply(c(1:59), function(x){1}), #first week of breeding and rearing period
                           lapply(c(1:59), function(x){rbinom(n=4, size=1, prob = prob.den.change.breed.t.48.52)}), # last 4 weeks of the year
                           SIMPLIFY = F)
    

    #establish for each family the weeks the dominant adults share den during the breeding -rearing seasons (as determined by Koopman, 1998)    
    dom.adults.share.breed.rear=lapply(c(1:59), function(x){rbinom(n=length(c(breed.t, rearing.t)), size = 1, prob=.9)})
    
    #establish for each family the weeks the dominant adults share den during the rest of the year
    dom.adults.share.other=lapply(c(1:59), function(x){rbinom(n=length(other.t), size = 1, prob=.4)})
    
    # a final list with the weeks the dominant adults share den (per family) across the year
    dom.adults.share=mapply(c,lapply(dom.adults.share.breed.rear, function(x){x[6:23]}), dom.adults.share.other, lapply(dom.adults.share.breed.rear, function(x){x[1:5]}), SIMPLIFY = F) # this is the per family if they share or not.
    
    #establish for each family the weeks the dominants share den with subdominant during the breeding season (as determined by Koopman, 1998)    
    dom.adults.share.subdominants.breed=lapply(c(1:59), function(x){rbinom(n=length(breed.t), size = 1, prob=.45)})
    
    #establish for each family the weeks the dominants share den with subdominants during the rest of the year
    dom.adults.share.subdominants.other=lapply(c(1:59), function(x){rbinom(n=length(other.t), size = 1, prob=.4)})
    
    # a final list with the weeks the dominants share den with  the subdominant (per family) across the year
    dom.adults.share.sa=mapply(c,lapply(dom.adults.share.subdominants.breed, function(x){x[6:10]}), 
                               lapply(c(1:59), function(x){rep(0,length(rearing.t))}),
                               dom.adults.share.subdominants.other, 
                               lapply(dom.adults.share.subdominants.breed, function(x){x[1:5]}), SIMPLIFY = F) # this is the per family if they share or not.
    
    #establish for each family the weeks the subdominants share during the rearing season (as determined by Koopman, 1998)    
    subdom.share.subdom.rear=lapply(c(1:59), function(x){rbinom(n=length(rearing.t), size = 1, prob=.85)})
    
    #establish for each family the weeks the subdominants share during the rest of the year
    subdom.share.subdom.other=lapply(c(1:59), function(x){rbinom(n=length(other.t), size = 1, prob=.55)})
    
    # a final list with the weeks the subdominants share dens (per family)
    subdom.share.sa=mapply(c, lapply(c(1:59), function(x){rep(0,5)}),
                           subdom.share.subdom.rear,
                           subdom.share.subdom.other,
                           lapply(c(1:59), function(x){rep(0,5)}), SIMPLIFY = F) # this is the per family if they share or not.
  
    } # social groups deciding to share den when damily changes den
  
  # ---------------------------------------------- #
  
  
  
  ##### DEN SHARING PROCESS #####
  
  # Determining how fox families share dens in the current week
  # Assign the dens that the foxes are using during the current week 
  # Dens are set independently of the actual alive status of the domninant foxes so a dead adult female (or male) can
  # be assigned a den (but it does not get involved in disease transmission)
  # The dens are selected randomly but because dens are already located based on habitat type preference, then they follow 
  # Frost 2005.
  
  # which families change in the current week
  index.fams.fox.change.den<-which(sapply(fams.change.den, function(x){x[d]})==1) 
  fams.fox.change.den=paste('Fam_', index.fams.fox.change.den, sep='')
  
  
  if(length(index.fams.fox.change.den)>0){ # if there are families changing den during the current week
    
    for(g in index.fams.fox.change.den){ # then apply for those families only
      

      
      # if during the current week: dominants adults share den, dominant adults share den with subdominants, and subdominants share den with 
      # other subdominants, then all foxes move to the same den. 
      
      if(dom.adults.share[[g]][d]==1 & dom.adults.share.sa[[g]][d]==1 & subdom.share.sa[[g]][d]==1){ # if all choose to share den
        new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1) # the new den to be used by the dominant pair is not the one(s) they were using previously.
        fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Disp%in%c(0,1),]$Den_id=new.den.parents} 
      
     
      
      #if the dominant pair shares den during the current week, they also share dens with the subdominant, but subdominants do not share them between themselves,
      # then one subdominant (if anyone alive) stays with the dominant pair and the remaining ones take a den by themselves
    
       if(dom.adults.share[[g]][d]==1 & dom.adults.share.sa[[g]][d]==1 & subdom.share.sa[[g]][d]==0){ 
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){#if at least one of the dominants is alive, then choose a den for them
            new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1)}else{# new den for the dominant pair, excludes the previously one they occupied
            new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1)} # sample 1 to be used in the future by foxes taking the adult positions
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3,]$Den_id=new.den.parents # set the new den to the dominant pair (if dead or alive)
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # id of the subdominants in the current family
        if(length(sa.current.fam)>0){ # if there are subdominants
          sa.share.adults=sample(sa.current.fam,1) #select the subdominant to stay share den with the subdominants
          fox[which(fox$Fox_id==sa.share.adults),]$Den_id=new.den.parents #move the selected subdominant to the same den than the dominant pair
          sa.current.fam=sa.current.fam[!(sa.current.fam%in%sa.share.adults)]# select the remaining subdominants
          if(length(sa.current.fam)>0){ # if there are remaining subdominants
            for(i in 1:length(sa.current.fam)){ # set them in a new den, different form the one they were using and the one that is set to be used by the dominant pair and the other subdominants already located
              if(i==1){ # for the first one
                new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id, new.den.parents))],1, replace = F) #select den for these dubdominants
                fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id=new.den.sa}else{ # for the remaining ones
                  new.den.sa.temp=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id, new.den.parents, new.den.sa))], 1)
                  fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id=new.den.sa.temp
                  new.den.sa=c(new.den.sa, new.den.sa.temp)}}}}}
      
      
      
      #if the dominant pair shares den during the current week, but they do not share dens with the subdominants, but subdominants share den between themselves,
      # then the dominant pair shares a den and  the subdominant share in groups a single den different from the one used by the dominant pair
      
      if(dom.adults.share[[g]][d]==1 & dom.adults.share.sa[[g]][d]==0 & subdom.share.sa[[g]][d]==1){ 
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){#if at least one of the dominants is alive, then choose a den for them
          new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1)}else{ 
          new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1)} # sample 1 to be used in the future by foxes taking the adult positions
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3,]$Den_id=new.den.parents # move the dominants to the chosen den
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # id the subdominants in the current family
        if(length(sa.current.fam)>0){ # if there are subdominants then
          if(length(sa.current.fam)%in%c(1,2)){ # if there is just one or 2 subdominants
            new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[as.character(all_dens_avail_by_hr[[g]]@data$Den_id)!=new.den.parents], 1) # choose the den to be used by the subdominants, that disregards the one chose for the dominant pair
            fox[fox$Fox_id%in%sa.current.fam,]$Den_id=new.den.sa} # locate the subdominants in the current den
          if(length(sa.current.fam)>2){ # if there are more than 2 subdominats build groups of them to share a single den
            sa.groups=apply(restrictedparts(length(sa.current.fam),length(sa.current.fam))[,-ncol( restrictedparts(length(sa.current.fam),length(sa.current.fam)))], MARGIN = 2, FUN = list) # build subdominant groups
            sa.groups=lapply(lapply(sa.groups, function(x){unlist(x)}), function(x){x[x!=0 & !(1%in%x)]})
            sa.groups=sa.groups[lapply(sa.groups,length)>0]
            sa.groups=as.numeric(sample(as.character(unlist(sample(sa.groups,1))))) # they way subdominants will share a single den. Each group in a single den.
            for(i in 1:(length(sa.groups))){ #for each of those groups, locate them in a single den
              if(i==1){
                sa.current.group=sample(sa.current.fam, sa.groups[i])  
                den.for.sa.group=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[as.character(all_dens_avail_by_hr[[g]]@data$Den_id)!=new.den.parents], 1) # the first group of subdominants does not use the same den as the dominant pair
                fox[fox$Fox_id%in%sa.current.group,]$Den_id=den.for.sa.group}else{ # the remaining groups of subdominants do not use the den of the dominat pair neither the dens used by other groups of subdominants
                  sa.current.group.temp=sample(sa.current.fam[!(sa.current.fam%in%sa.current.group)], sa.groups[i])  
                  den.for.sa.group.temp=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(new.den.parents, den.for.sa.group))], 1)
                  fox[fox$Fox_id%in%sa.current.group.temp,]$Den_id=den.for.sa.group.temp # move the subdominants in the current group to the corresponding den
                  sa.current.group=c(sa.current.group, sa.current.group.temp)
                  den.for.sa.group=c(den.for.sa.group, den.for.sa.group.temp)}}}}}
      
      
      
      #if the dominant pair shares den during the current week, but they do not share dens with the subdominants, and the subdominants also do not share den between themselves,
      # then the dominant pair shares a den and the subdominant are all in a single den different from the one used by the other members of the family
      
      if(dom.adults.share[[g]][d]==1 & dom.adults.share.sa[[g]][d]==0 & subdom.share.sa[[g]][d]==0){ 
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){#if at least one of the dominants is alive, then choose a den for them
          new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1)}else{ # a den for the dominant pair that is different of the den they were previously using
          new.den.parents=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1)} # sample 1 to be used in the future by foxes taking the adult positions
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3,]$Den_id=new.den.parents # move the dominant pair to the new den (if dead or alive)  
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # identify the subdominants of the current family
        if(length(sa.current.fam)>0){ # if there are  subdominants 
          for(i in 1:length(sa.current.fam)){ #then
            if(i==1){ # choose a den for the first subdominant different from the one it was using the previous week and the one chosen for the dominant pair
              new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id, new.den.parents))], 1, replace = F)
              fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id=new.den.sa}else{ # move it to this chosen den
                new.den.sa.temp=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id, new.den.parents, new.den.sa))], 1) # select a den for the following subdominants. This den cannot be the one used by the subdominant in the previous week neither the ones chosen by the dominat pair and other subdominants.
                fox[which(fox$Fox_id%in%sa.current.fam[i]),]$Den_id=new.den.sa.temp # move the subdominants to the selected den.
                new.den.sa=c(new.den.sa, new.den.sa.temp)}}}} 
      
      
      
      #if the dominant pair does not share den during the current week, but they do share dens with the subdominants, and the subdominants also share a den,
      # then the male and female dominant occupy a different den, each of them shared the den with a single subdominant (at least 2 alive) and the remaining subdominants use a unique den.
     
      if(dom.adults.share[[g]][d]==0 & dom.adults.share.sa[[g]][d]==1 & subdom.share.sa[[g]][d]==1){ 
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){#if at least one of the dominants is alive, then choose a den for them
          new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1) # new den fo the dominant male which it cannot be the one it was using the previous week
          new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id, new.den.male))], 1)}else{
          new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1) # new den fo the dominant female which it cannot be the one it was using the previous week
          new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%new.den.male)], 1)}
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==1,]$Den_id=new.den.male # move the male dominant to the corrsponding den
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==0,]$Den_id=new.den.female # move the female dominant to the corrsponding den
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # identify the subdominants in the current family
        if(length(sa.current.fam)>0){ # if there are subdominants in the current family
          if(length(sa.current.fam)==1){ # but it is just one of them
            den.with.mother=rbinom(n = 1, 1, 0.5) # choose randomly if this unique subdominant stays with the dominant female or male
            if(den.with.mother==1){ # if it stays with the dominant female
              fox[fox$Fox_id%in%sa.current.fam,]$Den_id=new.den.female}else{ # move it to the den occupied by the dominant female
                fox[fox$Fox_id%in%sa.current.fam,]$Den_id=new.den.male}} # otherwise, move it to the den occupied by the dominant male
          if(length(sa.current.fam)==2){ # if there are 2 subdominants, each one stay with one of the adult dominants
            fox[fox$Fox_id%in%sa.current.fam,]$Den_id=sample(c(new.den.female, new.den.male))} # which subdominant dens with the dominant male or female is random
          if(length(sa.current.fam)==3){ # if there are 3 subdominants, then create a pair and a single one.  onw eith am and one with afgroup randomly the sa and assigned group to the adult male female or if there are more than 2 groups -> one group in another den
            num.sa.current.fam.adult.fem=sample(c(1,2),1) # choose the number of subdominats that stay with the dominant female
            sa.current.fam.af=sample(sa.current.fam, num.sa.current.fam.adult.fem)#select the subdominants that stay with dominant female
            sa.current.fam.am=sa.current.fam[!(sa.current.fam%in%sa.current.fam.af)]#select the subdominants that stay with dominant male
            fox[fox$Fox_id%in%sa.current.fam.am,]$Den_id=new.den.male # locate subdominats with the dominat male
            fox[fox$Fox_id%in%sa.current.fam.af,]$Den_id=new.den.female} # locate subdominats with the dominant female
          if(length(sa.current.fam)>3){ # if there are more than 3 subdominants create groups of them
            sa.groups=apply(restrictedparts(length(sa.current.fam),length(sa.current.fam))[,-ncol( restrictedparts(length(sa.current.fam),length(sa.current.fam)))], MARGIN = 2, FUN = list)
            sa.groups=lapply(lapply(sa.groups, function(x){unlist(x)}), function(x){x[x!=0 & !(1%in%x)]})
            sa.groups=sa.groups[lapply(sa.groups,length)>0 & lapply(sa.groups,length)>1] 
            sa.groups=sample(unlist(sample(sa.groups,1))) # they way sundominants will be gathered
            sa.current.fam.am=sample(sa.current.fam, sa.groups[1]) # choose the subdominants to stay with with the dominant male
            sa.current.fam.af=sample(sa.current.fam, sa.groups[2]) # choose the subdominants to stay with with the dominant male
            fox[fox$Fox_id%in%sa.current.fam.am,]$Den_id=new.den.male # move the subdominants with the dominant male
            fox[fox$Fox_id%in%sa.current.fam.af,]$Den_id=new.den.female # move the subdominants with the dominant female
            if(length(sa.groups)>2){# if there are more than 2 groups of subdominants
              for(i in 1:(length(sa.groups[-c(1,2)]))){ #then
                if(i==1){ # locate the remaining groups in a den different of the one chosen by the dominant individuals as well as different from the one chosen by the other subdominant groups
                  sa.current.group=sample(sa.current.fam[!(sa.current.fam%in%c(sa.current.fam.am,sa.current.fam.af))], sa.groups[i+2])  
                  den.for.sa.group=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(new.den.male, new.den.female))], 1)
                  fox[fox$Fox_id%in%sa.current.group,]$Den_id=den.for.sa.group}else{
                    sa.current.group.temp=sample(sa.current.fam[!(sa.current.fam%in%c(sa.current.fam.am, sa.current.fam.af, sa.current.group))], sa.groups[i+2])  
                    den.for.sa.group.temp=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(new.den.male, new.den.female,den.for.sa.group))], 1)
                    fox[fox$Fox_id%in%sa.current.group.temp,]$Den_id=den.for.sa.group.temp
                    sa.current.group=c(sa.current.group, sa.current.group.temp)
                    den.for.sa.group=c(den.for.sa.group, den.for.sa.group.temp)}}}}}}
      
      
      
      # no member of the family shares den except the subdominants. Then the dominant adults den by themselves, while the 
      # subdominants do not den singly (if there are at least 2 of them).
      if(dom.adults.share[[g]][d]==0 & dom.adults.share.sa[[g]][d]==0 & subdom.share.sa[[g]][d]==1){
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){#if at least one of the dominants is alive, then choose a den
          new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1) #chooose the den for the dominant male. It has to exclude the den previously used by the dominat male
          new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id, new.den.male))], 1)}else{ #chooose the den for the dominant female. It has to exclude the den previously used by the dominat male
            new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1) 
            new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%new.den.male)], 1)}
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==1,]$Den_id=new.den.male # dominant male moved to the chosen den
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==0,]$Den_id=new.den.female # dominant female moved to the chosen den
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # identify the subdominants in the current family
        if(length(sa.current.fam)>0){ # if there are subdominants then all of them move to the same den
          if(length(sa.current.fam)%in%c(1,2,3)){ #if there are between 1 - 3 
            new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(new.den.male, new.den.female))],1) # select a den exc;uding the one selected by the dominant adults, and the ones previously used by the subdominants
            fox[fox$Fox_id%in%sa.current.fam,]$Den_id=new.den.sa} # locate the subdominants
          if(length(sa.current.fam)>3){  # if there are more than 3 subdominants then create groups of them that use a common den
            sa.groups=apply(restrictedparts(length(sa.current.fam),length(sa.current.fam))[,-ncol( restrictedparts(length(sa.current.fam),length(sa.current.fam)))], MARGIN = 2, FUN = list)
            sa.groups=lapply(lapply(sa.groups, function(x){unlist(x)}), function(x){x[x!=0 & !(1%in%x)]})
            sa.groups=sa.groups[lapply(sa.groups,length)>0 & lapply(sa.groups,length)>1]
            sa.groups=sample(unlist(sample(sa.groups,1))) # they way subdominants will be grouped in a common den.
            for(i in 1:(length(sa.groups))){ #for each of these groups
              if(i==1){ # for the fisrt group
                sa.current.group=sample(sa.current.fam, sa.groups[i])  #id the subdominants 
                den.for.sa.group=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(new.den.male, new.den.female))], 1) # choose a den exlcuding the one selected by the dominant adults and the ones previously used by the subdominants
                fox[fox$Fox_id%in%sa.current.group,]$Den_id=den.for.sa.group}else{ #locate these subdominants in the chosen den. Do the same for the remaining groups
                  sa.current.group.temp=sample(sa.current.fam[!(sa.current.fam%in%sa.current.group)], sa.groups[i])  # id the subdominants in the current group
                  den.for.sa.group.temp=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(new.den.male, new.den.female, den.for.sa.group))], 1) #chose the den to be used which cannot be the one selected by the dominant adults or other groupsof subdominants.
                  fox[fox$Fox_id%in%sa.current.group.temp,]$Den_id=den.for.sa.group.temp #move the subdominants to the chosen den
                  sa.current.group=c(sa.current.group, sa.current.group.temp)
                  den.for.sa.group=c(den.for.sa.group, den.for.sa.group.temp)}}}}}
      
      
      
      #if the dominant pair does not share den during the current week, but they do share dens with the subdominants, and the subdominants do not share a den,
      # then the male and female dominant occupy a different den, each of them shared the den with a single subdominant (at least 2 alive) and the remaining subdominants use a different den.
      if(dom.adults.share[[g]][d]==0 & dom.adults.share.sa[[g]][d]==1 & subdom.share.sa[[g]][d]==0){ # at least 2 sa alive
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){# as previously: select a den for each dominant adult excluding the one the used in the previous week
          new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3 & fox$Gender==1,]$Den_id)], 1)
          new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3 & fox$Gender==0,]$Den_id, new.den.male))], 1)}else{
            new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1)
            new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%new.den.male)], 1)}
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==1,]$Den_id=new.den.male #move the dominant male to the chosen den
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==0,]$Den_id=new.den.female #move the dominant female to the chosen den
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # id the subdominants of the current family
        if(length(sa.current.fam)>0){ # if there are subdominants
          if(length(sa.current.fam)==1){ # and it is just one subdominant in the family
            den.with.mother=rbinom(n = 1, 1, 0.5) #randomly choose if the unique subdominat stays with the dominant male or female
            if(den.with.mother==1){ # if the single subdominant shares den with the dominant female, then move it to the corresponding den
              fox[which(fox$Fox_id%in%sa.current.fam),]$Den_id=new.den.female}else{
                fox[which(fox$Fox_id%in%sa.current.fam),]$Den_id=new.den.male}} #otherwise move it to the den the dominant male is using
          if(length(sa.current.fam)==2){ # if there are 2 subdominants, each one share the den with one of the dominants
            fox[fox$Fox_id%in%sa.current.fam,]$Den_id=sample(c(new.den.female, new.den.male))} # which sa dens with the af or am is random
          if(length(sa.current.fam)>=3){ # if there are at least 3 subdominants, then one stays with the dominat female, one with the adult males and others by themselves
            sa.current.fam.af=fox[fox$Fox_id%in%sa.current.fam & fox$Den_id!=new.den.female,]$Fox_id #select which sa starys with dominant female
            if(length(sa.current.fam.af)==0){sa.current.fam.af=sample(sa.current.fam,1)}else{
              sa.current.fam.af=sample(sa.current.fam.af, 1)} # choose the subdominant to share with the dominant female
            sa.current.fam.am=fox[fox$Fox_id%in%sa.current.fam & fox$Fox_id!=sa.current.fam.af & fox$Den_id!=new.den.male,]$Fox_id
            if(length(sa.current.fam.am)==0){sa.current.fam.am=sample(sa.current.fam[sa.current.fam!=sa.current.fam.af],1)}else{
              sa.current.fam.am=sample(sa.current.fam.am ,1)}# choose the subdominant to share with the dominant male
            fox[fox$Fox_id%in%sa.current.fam.am,]$Den_id=new.den.male # move the subdominat to share den with the dominant male
            fox[fox$Fox_id%in%sa.current.fam.af,]$Den_id=new.den.female  # move the subdominat to share den with the dominant female
            other.sa=sa.current.fam[!(sa.current.fam%in%c(sa.current.fam.am, sa.current.fam.af))] # what to do with the remainig subdominants
            if(length(other.sa)>0){ #if there are more subdominants in the family
              for(sa in 1:(length(other.sa))){ # for each one of them
                if(sa==1){ # if there is a single remaining subdominant
                  new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fox_id%in%other.sa[sa],]$Den_id, new.den.male, new.den.female))], 1) # select the den which excludes the previous den used by the current subdominant as wells as the den used by the male and female dominant
                  sa.current.fam.den=new.den.sa} # move the remaining subdominant
                if(sa!=1){ i#if there are at least 2 remainig subdominants to locate in a den, each of them take a den by themselves. 
                  new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fox_id%in%other.sa[sa],]$Den_id, new.den.male, new.den.female, sa.current.fam.den))], 1) # # select the den which excludes the previous den used by the current subdominant as wells as the den used by the male and female dominant, and other subdominants
                  sa.current.fam.den=c(sa.current.fam.den,new.den.sa)}
                fox[fox$Fox_id%in%other.sa[sa],]$Den_id=new.den.sa}}}}} # move the subdominat to the chosen den
         

      
      #if no meber of the family shares dens, then each individual moves to a new den by themselves
      if(dom.adults.share[[g]][d]==0 & dom.adults.share.sa[[g]][d]==0 & subdom.share.sa[[g]][d]==0){ # at least 2 sa alive
        if(nrow(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==3,])>0){# as previously: select a den for each dominant adult excluding the one the used in the previous week
          new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id)], 1)
          new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fam_id==paste('Fam_', g, sep = '') & fox$Social==3,]$Den_id, new.den.male))], 1)}else{
            new.den.male=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id), 1)
            new.den.female=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%new.den.male)], 1)}
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==1,]$Den_id=new.den.male  #move the dominant male to the chosen den
        fox[fox$Fam_id==paste('Fam_', g, sep = '')  & fox$Social==3 & fox$Gender==0,]$Den_id=new.den.female #move the dominant female to the chosen den
        sa.current.fam=fox[which(fox$Fam_id==paste('Fam_', g, sep = '') & fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1)),]$Fox_id # id the subdominants of the current family
        if(length(sa.current.fam)>0){ #if there are subdominants in the current fam
          for(sa in 1:(length(sa.current.fam))){ #for each subdominant
            if(sa==1){ # if there is just a single subdominant then chosse a den for it 
              new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fox_id%in%sa.current.fam[sa],]$Den_id, new.den.male, new.den.female))], 1) # select the den which excludes the previous den used by the current subdominant as wells as the den used by the male and female dominant
              sa.current.fam.den=new.den.sa} # move the subdominat to the chosen den
            if(sa!=1){ # if there are several subdominants then chosse a den for them
              new.den.sa=sample(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)[!(as.character(all_dens_avail_by_hr[[g]]@data$Den_id)%in%c(fox[fox$Fox_id%in%sa.current.fam[sa],]$Den_id, new.den.male, new.den.female, sa.current.fam.den))], 1) # # select the den which excludes the previous den used by the current subdominant as wells as the den used by the male and female dominant, and other subdominants
              sa.current.fam.den=c(sa.current.fam.den,new.den.sa)} 
            fox[fox$Fox_id%in%sa.current.fam[sa],]$Den_id=new.den.sa}}} # move the subdominats to the chosen dens
    } # end of the loop for families changing den
  } # end the loop for there are families changing den
  
  fox=fox[mixedorder(fox$Fam_id),] # resort the fox dataset based on family number
  
  # ---------------------------------------------- #
  
  
  
  
  #### LOCATE PUPS WITH MOTHER ####
  
  # Alive pups are located in the same den tha the dominan female
  if(nrow(fox[fox$Social==1 & fox$Alive==1,])>0){ # if there are pups alive
    dens.dom.fem=sapply(c(1:59), function(x){fox[fox$Fam_id==paste('Fam_', x, sep='') & fox$Gender==0 & fox$Social==3,]$Den_id}) # get the den used by the dominant female
    num.pups.alive.per.fam=sapply(paste('Fam_',c(1:59), sep=''), function(x){nrow(fox[fox$Social==1 & fox$Alive==1 & fox$Fam_id==x,])})
    fox[fox$Alive==1 & fox$Social==1,]$Den_id=unlist(sapply(c(1:59), function(x){rep(dens.dom.fem[x],num.pups.alive.per.fam[x])})) # move them with the dominant female
  }

  # ---------------------------------------------- #
  
  
  #### LOCATE DISPERSERS THAT CAN MOVE (NOT TYPE I.2>0) #### 
  
  #Dispersers are in any den of the home range of destination that is not used by the foxes of the corresponding family
  if(nrow(fox[fox$Disp==2 & fox$Alive==1 & fox$Social==2 & fox$Den_id==0,])>0){ # if there are dispersers
    number.dispersers.to.locate.per.fam=sapply(c(1:n_fam), function(x){nrow(fox[fox$Fam_id==paste('Fam_',x,sep='') & fox$Disp==2 & fox$Alive==1 & fox$Social==2 & fox$Den_id==0,])}) # number of disperser at each home range
    dens.temp=sapply(c(1:n_fam), function(x){sample(as.character(all_dens_avail_by_hr[[x]]@data$Den_id[!(all_dens_avail_by_hr[[x]]@data$Den_id%in%fox$Den_id)]), size = number.dispersers.to.locate.per.fam[x], replace = F)}) # select unused dens for each disperser at a given home range
    fox[fox$Disp==2 & fox$Alive==1 & fox$Social==2 & fox$Den_id==0,]$Den_id=unlist(dens.temp)} # move them to the chosen dens
  
  #check point
  if(nrow(fox[fox$Social==3 & is.na(fox$Fox_id) & fox$Alive==1,])>0)stop('dominant alive foxes without fox id')
  if(nrow(fox[fox$Social==3 & is.na(fox$Den_id) & fox$Alive==1,])>0)stop('dominant alive foxes without den')
  if(nrow(fox[fox$Alive==1 & is.na(fox$Den_id),])>0)stop('alive fox without den assigned')
 
  
   
} # end the loop that den sharing is being simulated

