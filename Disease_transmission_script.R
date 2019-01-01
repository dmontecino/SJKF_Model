##############################################################################################
# --- CODE OF THE MODEL TO ASSESS THE ROLE OF DENS IN THE SPREAD, INVASION AND PERSISTENCE---#
# --- OF SARCOPTIC MANGE IN THE SAN JOAQUIN FOX POPULATION LOCATED IN BAKERSFIELD, CA -------#
# --- SPECIFICALLY THIS SCRIPT MODELS SARCOPTIC MANGE TRANSMISSION FOX-FOX, INFESTATION -----#
# --- OF DENS AND DEN TO FOX TRANSMISSION (INDIRECT TRANSMISSION) ---------------------------#
##############################################################################################

# Author:Diego Montecino-Latorre
# dmontecino@ucdavis.edu
# Details of the model are published in the journal "Epidemics" January 2019


# For the parameters of the processes associated with disease transmission, they are 
# located in ... THIS is the model function...


if(track.disease==T){ # If we want to track the disease process 
  
  
  #### INTRODUCE AN EXPOSED FOX IN THE POPULATION ####
  
  if(d==45 & years.run==0){ # introduced an exposed infividual on the first year simulated during the 45th week
    fams.north.bako=which(gIntersects(box, home_ranges, byid = T)) # which families are in the top north of the city
    home_range_of_introduction_opcion=unique(fox[fox$Fam_id%in%paste('Fam_',fams.north.bako, sep='') & fox$Alive==1 & fox$E==0 & fox$S==1 & fox$Social==3,]$Fam_id) # the id's of the families int the north top of the city
    if(length(home_range_of_introduction_opcion)>0){ # if there are families in this top corner (yes is it always tru for all simulations)
      home_range_of_introduction=sample(home_range_of_introduction_opcion,1) # select the family of introduction
      first_exposed_fox=sample(fox[fox$Fam_id==home_range_of_introduction & fox$Alive==1 & fox$E==0 & fox$S==1 & fox$Social==3,]$Fox_id,1) # select the first exposed fox in this family
      fox[which(fox$Fox_id==first_exposed_fox), ]$E=0.1 # as exposed. By the end of the week the E moves to 1
      fox[which(fox$Fox_id==first_exposed_fox), ]$S=0}} # correct susceptible. This exposed fox is not susceptible anymore.
  
  # ---------------------------------------------- #
  
  
  
  ###### INFESTATION OF DENS WITH SARCOPTES SCABIEI #####
  
  # infestation of the den that is used by a fox type I or II
  # if an infected fox is using a den is infested again as it started from day 0.
  
  if(nrow(den.data.set[den.data.set$Den_id%in%unique(c(fox[fox$I.1>0 & fox$Alive==1,]$Den_id, fox[fox$I.2>0 & fox$Alive==1,]$Den_id)),])>0){ # if there are occupied dens with infested foxes
    new.dens.infected=unique(c(fox[fox$I.1>0 & fox$Alive==1,]$Den_id, fox[fox$I.2>0 & fox$Alive==1,]$Den_id)) # identfied these dens
    den.data.set[den.data.set$Den_id%in%new.dens.infected,]$I=0.1} #make them infested
  
  # ---------------------------------------------- #
  
  
  
  ###### SARCOPTIC MANGE TRANSMISSION DUE TO SOCIAL CONTACT, DEN SHARING AND INFESTED DEN #####
  
  # a susceptible fox sharing den with an infectious fox can become infected due to social contact, because they share den or because the infectius foxes infest the den they are using.
  
  if(sum(nrow(fox[fox$I.1>=0.1 & fox$Alive==1,]),nrow(fox[fox$I.2>=0.1 & fox$Alive==1,]))>0){# if there are infectious foxes at all.
    temp.I=lapply(c(1:n_fam), function(x){c(as.character(fox[fox$Fam_id==paste('Fam_',x, sep='') & fox$Alive==1 & fox$S==0 & fox$E==0 & fox$I.1>=0.1 & fox$I.2==0 & fox$Disp%in%c(0,1),]$Fox_id), # id of infectius foxes per family type I
                                            as.character(fox[fox$Fam_id==paste('Fam_',x, sep='') & fox$Alive==1 & fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2>=0.1 & fox$Disp%in%c(0,1),]$Fox_id))}) # id of infectius foxes per family type II
    
    if(nrow(fox[fox$S==1 & fox$Alive==1,])>0){ # if there are susceptible individuals in the system
      
      
      
      temp.S=lapply(c(1:n_fam), function(x){c(as.character(fox[fox$Fam_id==paste('Fam_',x, sep='') & fox$Alive==1 & fox$S==1 & fox$E==0 & fox$I.1==0 & fox$I.2==0 & fox$Disp%in%c(0,1),]$Fox_id))}) # id of susceptibles individuals per family
     
      
      
      # Fox-to-fox direct transmission due to social contact among members of the same family 
      temp.trans.matrix.1=lapply(c(1:59), function(x){if(length(temp.S[[x]])==0){rep(0, length(temp.S[[x]]))}
                                                      else{replicate(length(temp.S[[x]]), rbinom(length(temp.I[[x]]), 1, prob.trans.foxes.contact.fam[j]))}})
      temp.trans.matrix.1=lapply(temp.trans.matrix.1, function(x){if(class(x)=='matrix'){colSums(x)}else{x}}) # summation of all transmission events per susceptible individual (if exposed to more than  infectious fox)   
      temp.trans.matrix.1=lapply(temp.trans.matrix.1, function(x){if(class(x)=='list'){rep(0,length(x))}else{x}}) # summation of all transmission events per susceptible individual   
      
      
      
      # Fox-to-fox direct transmission due to den sharing 
      if(any(sapply(mapply(function(x,y){fox[fox$Fox_id%in%x,]$Den_id%in%fox[fox$Fox_id%in%y,]$Den_id},  temp.S, temp.I), function(z){any(z)==T}))){ # if any susceptible fox is sharing den with an infectious fox
        dens.used.by.susc.fox=lapply(temp.S, function(x){sapply(x,function(y){fox[which(fox$Fox_id==y),]$Den_id})})  # id of dens used by susceptible foxes.
        num.inf.foxes.each.susc.fox.share.den.with=lapply(dens.used.by.susc.fox, function(x){sapply(x, function(y){ #number of infectious individuals sharing den with susceptible fox
          sum(nrow(fox[fox$Den_id%in%y & fox$S==0 & fox$I.1>=0.1 & fox$Alive==1,]),
              nrow(fox[fox$Den_id%in%y & fox$S==0 & fox$I.2>=0.1 & fox$Alive==1,]))})})
        temp.trans.matrix.2=lapply(num.inf.foxes.each.susc.fox.share.den.with, function(x){sapply(x, function(y){
          rbinom(y, 1, prob.trans.foxes.share.den[j])})}) # transmission from infecitous foxes to susceptible foxes because of den sharing 
        temp.trans.matrix.2=lapply(temp.trans.matrix.2, function(x){if(class(x)=='list'){
          sapply(x, function(y){if(length(y)==0){0}else{sum(y)}})}else{x}})} # sum the transmission event (if the susceptible fox is sharing den with more than one infectious fox)
      else{ # if there are not susceptible foxes sharing den with infectious foxes then everything is zero
            temp.trans.matrix.2=lapply(temp.S, function(x){rep(0,length(x))})} 
      
      
  
      # Transmission of sarcoptic mange from an infested den to a susceptible fox using that den. This adds to the previous trnasmission pathways if that is appropriate.
      if(any(fox[fox$S==1,]$Den_id%in%unique(den.data.set[den.data.set$I>=0.1,]$Den_id))){ # if there are susceptible foxes using infested dens
        dens.used.by.susc.fox=lapply(temp.S, function(x){sapply(x,function(y){fox[which(fox$Fox_id==y),]$Den_id})})  # id of dens used by susceptible foxes.
        infested.dens=as.character(den.data.set[den.data.set$I>=0.1,]$Den_id) # id of infested dens
        temp.trans.matrix.3=lapply(dens.used.by.susc.fox, function(x){x%in%infested.dens}) #dens used by teh foxes during the current week that are infested
        temp.trans.matrix.3=lapply(temp.trans.matrix.3, function(x){sapply(x, function(y){if(y==T){
          rbinom(1, 1, prob.trans.den.to.foxes[j])}else{0}})})}
      else{ # if there arent susceptible foxes using the infested dens then there is no transmission
            temp.trans.matrix.3=lapply(temp.S, function(x){rep(0,length(x))})}      
      
      
      
      # Finally here each susceptible individual become exposed by taking into account all the 3 previous transmission pathways and
      # the exposure to all the infecctios individuals and or dens it was exposed to during the current week.
      trans=mapply(rbind, temp.trans.matrix.1, temp.trans.matrix.2, temp.trans.matrix.3) # tranmisssion to the susceptible fox based on the previous 3 mechanisms       
      trans=lapply(trans, function(x){if(length(x)>0){colSums(x)}else{vector('integer', length=0)}}) # sum the tramissions
      trans=lapply(trans, function(y){if(length(y)>0){ifelse(y>=1,1,0)}else{y}}) # tranmisssion =1 , no trasnmission =0

      
      
      # based on the results of the trans matrix: actual transmission of disease. New exposed foxes
      new.exposed.foxes=unlist(temp.S)[unname(unlist(trans))!=0]
      if(length(new.exposed.foxes)>0){
        fox[fox$Fox_id%in%new.exposed.foxes,]$E=0.1 # veery first week to be exposed. This will be converted to 1 one the current week is over
        fox[fox$Fox_id%in%new.exposed.foxes,]$S=0} # correcting the susceptible status of the new exposed foxes.
      
  # ---------------------------------------------- #    
  
      
  ####### SARCOPTIC MANGE TRANSMISSION DUE TO NURSING #####
      
      if(nrow(fox[fox$Social==1 & fox$Alive==1 & fox$S==1,])>0){ # if there are susceptible pups
        if(nrow(fox[fox$Social==3 & fox$Alive==1 & fox$Gender==0 & fox$I.1>=0.1 | fox$Social==3 & fox$Alive==1 & fox$Gender==0 & fox$I.2>=0.1,])>0){ # if there are infectious females
          fams.adult.fem.inf.and.pups.susc=as.character(fox[fox$Social==3 & fox$Alive==1 & fox$Gender==0 & fox$I.1>=0.1 | fox$Social==3 & fox$Alive==1 & fox$Gender==0 & fox$I.2>=0.1,]$Fam_id)[as.character(fox[fox$Social==3 & fox$Alive==1 & fox$Gender==0 & fox$I.1>=0.1 | fox$Social==3 & fox$Alive==1 & fox$Gender==0 & fox$I.2>=0.1,]$Fam_id)%in%as.character(fox[fox$Social==1 & fox$Alive==1 & fox$S==1,]$Fam_id)] # id those females that are infectious and have susceptiebl offspring
          if(length(fams.adult.fem.inf.and.pups.susc)>0){ # if there are
            new.pups.infected=unlist(lapply(fams.adult.fem.inf.and.pups.susc, function(x){
              if(nrow(fox[fox$Fam_id%in%x & fox$Alive==1 & fox$Social==1 & fox$S==1,])==0){ # if there are not susceptible pups nursing frmo an infectious female then
                vector('numeric', length(0))} # nothing happens
              else{ #but if there are
                rbinom(nrow(fox[fox$Fam_id%in%x & fox$Alive==1 & fox$Social==1 & fox$S==1,]), 1, prob.trans.pups.nursing.inf.female[j])}})) # new pups exposed based on probability
            if(any(new.pups.infected==1)){# modify the disease status of new exposed pups if there are
              fox[fox$Fam_id%in%fams.adult.fem.inf.and.pups.susc & fox$Alive==1 & fox$Social==1 & fox$S==1,]$E[new.pups.infected==1]=0.1 #new exposed
              fox[fox$Fam_id%in%fams.adult.fem.inf.and.pups.susc & fox$Alive==1 & fox$Social==1 & fox$S==1 &fox$E==0.1,]$S=0} #not susceptible any more
          }
        }#if there are infectious females
      }#if there are susceptible pups
      
    } # if there are susceptible individuals
  } # if there are infetious foxes        
  
  
   # ---------------------------------------------- #    
  
  
  
  
  ###### SARCOPTIC MANGE TRANSMISSION DUE TO MATING #####
  
  # Disease transmission through mating with the adult dominant of the opposite sex within the matin pair
  
  if(d%in%cheat.t){ #if we are in cheating season
    index.fams.current.week.fem.heat=which(adult.fem.mating.week==d) # families whose dominant pair is mating the current week
    index.fams.current.week.fem.alive=as.numeric(sapply(strsplit(as.character(fox[fox$Gender==0 & fox$Social==3 & fox$Alive==1 & fox$I.2==0,]$Fam_id), "_"), function(x){x[[2]]})) # where the dominant female is alive and not type 2
    index.fams.current.week.fem.heat.alive=index.fams.current.week.fem.heat[index.fams.current.week.fem.heat%in%index.fams.current.week.fem.alive] # where the dominant female is alive and in estrous
    index.fams.with.alive.male=as.numeric(sapply(strsplit(as.character(fox[fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$I.2==0,]$Fam_id), split = '_'), function(x){x[2]})) # where the dominant female is alive and not type 2
    index.fams.with.alive.male.not.mating=index.fams.with.alive.male[!(index.fams.with.alive.male%in%which(adult.fem.mating.week%in%d))] #index of families  where males are not mating during the current week, they are alive, they are not type 2 and, therefore, they could cheat
    index.fams.current.week.fem.alive.male.alive=index.fams.current.week.fem.heat.alive[index.fams.current.week.fem.heat.alive%in%index.fams.with.alive.male] # families with couples that actually mate because the dominant pair is alive, the female is in estrous and non of them is type 2
    
    if(length(index.fams.current.week.fem.alive.male.alive)>0){ # if there are such families during the current week
      
      # transmission of mange from the infectious dominant male to the susceptible dominant female
      for(fam in index.fams.current.week.fem.alive.male.alive){
        if(nrow(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$I.1>0,])==1 & 
           nrow(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1,])==1){ # if the dominant male is infectious and the dominant female is susceptible
          fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1,]$E=rbinom(1,1, prob.trans.foxes.mating[j]) # probabilistic tranmission of mange from the dominant male is infectious to the dominant female
          if(nrow(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1 & fox$E==1,])>0){ # if transmission occurred, then move them to exposed and correct their susceptible status
            fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1 & fox$E==1,]$E=0.1
            fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1 & fox$E==0.1,]$S=0}}
        
      # transmission of mange from the infectious dominant female to the susceptible dominant male
        if(nrow(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$I.1>0,])==1 & 
           nrow(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$S==1,])==1){ 
          fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$S==1,]$E=rbinom(1,1,prob.trans.foxes.mating[j])
          if(nrow(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$S==1 & fox$E==1,])>0){ 
            fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$S==1 & fox$E==1,]$E=0.1
            fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==1 & fox$S==1 & fox$E==0.1,]$S=0}}
        
        fems.mate=unique(c(fems.mate, as.character(fox[fox$Fam_id==paste('Fam_', fam, sep='') & fox$Social==3 & fox$Gender==0,]$Fox_id))) # index of dominant females that mated during the current week    
      }}
    
    
  # Disease transmission through extra mating pair copulations: cheating
    
    for(i in sample(index.fams.current.week.fem.heat.alive)){ # for each dominant females  alive and in estrous
      index.fams.males.cheat.less.twice=as.numeric(sapply(strsplit(as.character(fox[!(fox$Fox_id%in%names(which(table(males.that.have.cheated)>=2))) & fox$Social==3 & fox$Gender==1,]$Fam_id), '_'), function(x){x[[2]]})) # identif dominant males that can cheat (those tha have had less than 2 extra mating pair mates) 
      index.fams.male.can.cheat.week.w=unique(index.fams.males.cheat.less.twice[index.fams.males.cheat.less.twice%in%index.fams.with.alive.male.not.mating]) # males alive and that can cheat because they have done it less than twice until the current female of week d and they are not currently mating with their apair
      if(length(Fams.dom.male.could.cheat.in[[i]][Fams.dom.male.could.cheat.in[[i]]%in%index.fams.male.can.cheat.week.w])>0){ # if the female in heat has chances of males to come and cheat with her
        
        # identify their relatives so they are not invovled in cheating (parents and siblings)
        father.current.dom.males.could.cheat=as.character(fox[fox$Fam_id%in%paste('Fam_',index.fams.male.can.cheat.week.w, sep='') & fox$Social==3 & fox$Gender==1 & fox$Alive==1,]$Father_id)
        father.current.dom.males.could.cheat[which(is.na(father.current.dom.males.could.cheat))]=rep(1, length(which(is.na(father.current.dom.males.could.cheat))))
        mother.current.dom.males.could.cheat=as.character(fox[fox$Fam_id%in%paste('Fam_',index.fams.male.can.cheat.week.w, sep='') & fox$Social==3 & fox$Gender==1 & fox$Alive==1,]$Mother_id)
        mother.current.dom.males.could.cheat[which(is.na(mother.current.dom.males.could.cheat))]=rep(1, length(which(is.na(mother.current.dom.males.could.cheat))))
        current.fem.cheat.father.mother.id=fox[fox$Fam_id==paste('Fam_',i,sep='') & fox$Gender==0 & fox$Social==3, c('Fox_id', 'Mother_id', 'Father_id')]
        current.fem.cheat.father.mother.id[,which(is.na(current.fem.cheat.father.mother.id))]=rep(2, length(which(is.na(current.fem.cheat.father.mother.id))))
        
        #evaluating if the female in heat and potential males going to cheat with her are relatives
        current.female.males.same.father=which(father.current.dom.males.could.cheat%in%as.character(current.fem.cheat.father.mother.id$Father_id))#which potenetial males cheating with are siblings with the actual female
        current.female.males.same.mother=which(mother.current.dom.males.could.cheat%in%as.character(current.fem.cheat.father.mother.id$Mother_id))#which potenetial males cheating with are siblings with the actual female
        current.female.mother.of.males= which(mother.current.dom.males.could.cheat==as.character(current.fem.cheat.father.mother.id$Fox_id))#which potenetial males cheating with are siblings with the actual female#which potential males to cheat is offspring of the female
        current.males.father.of.female= which(father.current.dom.males.could.cheat%in%as.character(current.fem.cheat.father.mother.id$Father_id))#which potenetial males cheating with are siblings with the actual female#which potential males to cheat is offspring of the female
        temp.males.relative.of.females=unique(c(current.female.males.same.father, current.female.males.same.mother, current.female.mother.of.males,current.males.father.of.female))
        temp.males.relative.of.females=ifelse(length(temp.males.relative.of.females)!=0,temp.males.relative.of.females,60) 
        index.fams.with.alive.male.not.mating.not.relatives=index.fams.with.alive.male.not.mating[-temp.males.relative.of.females] # Dominat Males that can go and cheat with the dominant female becuase htey are not realtives
        if(length(Fams.dom.male.could.cheat.in[[i]][Fams.dom.male.could.cheat.in[[i]]%in%index.fams.with.alive.male.not.mating.not.relatives])>0){ # if there are such dominant males
          if(length(Fams.dom.male.could.cheat.in[[i]][Fams.dom.male.could.cheat.in[[i]]%in%index.fams.with.alive.male.not.mating.not.relatives])==1){ #if it is just one, take it
            index.fams.male.come.to.cheat.with.i.fem=Fams.dom.male.could.cheat.in[[i]][Fams.dom.male.could.cheat.in[[i]]%in%index.fams.with.alive.male.not.mating.not.relatives]} # dominat female may cheat with none, one or 2 males when in heat
          if(length(Fams.dom.male.could.cheat.in[[i]][Fams.dom.male.could.cheat.in[[i]]%in%index.fams.with.alive.male.not.mating.not.relatives])>=2){ # if there are more than 1 then randomly select 1
            index.fams.male.come.to.cheat.with.i.fem=sample(Fams.dom.male.could.cheat.in[[i]][Fams.dom.male.could.cheat.in[[i]]%in%index.fams.with.alive.male.not.mating.not.relatives],sample(c(1,2),1))} # dominant female may cheat with none, one or 2 males when in heat
          adult.male.cheating.with.the.i.fem.week.d=as.character(fox[fox$Fam_id%in%paste('Fam_', index.fams.male.come.to.cheat.with.i.fem, sep='') & fox$Social==3 & fox$Gender==1,]$Fox_id) # id the dominat male that will cheat with the current dominant female
          males.that.have.cheated=c(males.that.have.cheated, adult.male.cheating.with.the.i.fem.week.d) #put this dominant male in the set of males that have cheated
          
          ## Actual transmission between the extra mating pair couples
          if(nrow(fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$I.1>0,])>0 & # if the dominant male is infectious 
             nrow(fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Gender==0 & fox$Social==3 & fox$Alive==1 & fox$S==1,])>0){ # and if the dominant female is susceptible
             
              fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1,]$E=rbinom(1,1,prob.trans.foxes.mating[j]) # then the probabilistic realization of sarcoptic mange transmission from the male to the female
            
              if(nrow(fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1 & fox$E==1,])>0){ # if transission occurred, then correct S and E status of the dominant female
                  fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1 & fox$E==1,]$E=0.1      
                  fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Alive==1 & fox$Social==3 & fox$Gender==0 & fox$S==1 & fox$E==0.1,]$S=0}}
          
          
          #the same idea but backwards (if the male is susceptible and the female is infectious)
          if(nrow(fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$S==1,])>0 & 
             nrow(fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Gender==0 & fox$Social==3 & fox$Alive==1 & fox$I.1>0,])>0){ 
            
            fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$S==1,]$E=rbinom(nrow(fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$S==1,]),1,prob.trans.foxes.mating[j])#rbeta(length(adult.male.cheating.with.the.i.fem.week.d),beta.dist.prob.trans.foxes.mating$shape1, beta.dist.prob.trans.foxes.mating$shape2)) # TRANSMISSION FROM THE MALE TO THE FEMALE
            
            if(nrow(fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$S==1 & fox$E==1,])>0){ # if transission occurred, then correct S and E status of the dominant male
                fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$S==1 & fox$E==1,]$E=0.1      
                fox[fox$Fox_id%in%adult.male.cheating.with.the.i.fem.week.d & fox$S==1 & fox$S==1 & fox$E==0.1,]$S=0}}
          
          # add the female to the sets of females that already mate
          fems.mate=unique(c(fems.mate, as.character(fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Gender==0 & fox$Social==3,]$Fox_id))) 
          
        }}}
}

} # end the loop of disease transmission
