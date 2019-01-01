
###################################################
#### -------- START OF THE J ITERATION ------- ####
###################################################

for(j in 1:num.iterations){# num of iterations is set in parameters
  
  
  # Prepare objects needed for the jth iteration
  
  # get the home ranges per family for the jth iteration
  home_ranges=all_home_ranges[[j]]  
  
  # get the dens per home range for the jth iteration
  all_dens_avail_by_hr=all_dens_avail_by_hr_by_it[[j]] 
  
  # get the neighbor home ranges of each home range for the jth iteration
  nb.hr.each.fam.1480=lapply(hr_nb_1480[[j]], function(x){x}) # close neighbor home ranges
  nb.hr.each.fam.1480.2960=lapply(hr_nb_more_1480_less_2960[[j]], function(x){x})  #farer neighbor home ranges
  
  #San joaquin kit fox families to where the adult male can move in order to mate with an extra couple dominant female foxes
  Fams.dom.male.could.cheat.in=nb.hr.each.fam.1480 
  Fams.dom.male.could.cheat.in=mapply(Fams.dom.male.could.cheat.in, lapply(c(1:59), function(x){x}), FUN = function(x,y){x[x!=y]}) 
  

  
  #### -------- START OF THE Y YEAR ------- #### 
  
  for(y in 1:years_simulate){ # for the number of years to simulate    
  

    
    
  #### -------- START OF THE D WEEK ------- #### 
    
    for(d in 1:52){ #for every week  
      
      #------------------------------#
      
      
      # when the simulation of the jth iteration just starts then a population of san joaquin kit foxes and dens must be constructed.
      if(d==1 & years.run==0){
        
        # data set for dens for the current iteration
        dens.per.it=as.character(unique(unlist(lapply(all_dens_avail_by_hr, function(x){x$Den_id})))) # data set for dens
        
        dens.per.it=data.frame(Den_id=as.character(unlist(lapply(all_dens_avail_by_hr, function(x){x$Den_id}))),
                               Land_type=as.character(unlist(lapply(all_dens_avail_by_hr, function(x){x$Frost}))))
        den.data.set=dens.per.it
        den.data.set$I=rep(0,nrow(den.data.set))
        den.data.set=unique(den.data.set)
        
        #------------------------------------------------------#
        
        
        #data set for foxes. in this case, the model loads a fox population built using the script ....
        # This is the script use to simulate a fox population for 8 years until it stabilized in expected population values
        # See... for details and the script to construct this fox populations.
        
        
        fox=fox.no.disease.yet[[j]]
        fox=fox[,-1]
        fox$num=sapply(strsplit(as.character(fox$Fam_id),split = '_'), function(x){as.numeric(x[2])})
        fox$Den_id=as.character(fox$Den_id)
        fox$Fox_id=as.character(fox$Fox_id)
        fox$Fam_id=as.character(fox$Fam_id)
        
        #------------------------------------------------------#
        
        # Load the data about how many weeks were the male positions available 
        # by the time the simulations of uninfested fox populations (see the previous object) were finished. For the following years to be simulated now this object is created in the current script.
        weeks.fams.male.available.to.be.replace.by.nb.male.sa=weeks.fams.male.available.to.be.replace.by.nb.male.sa.all[[j]]
        
        #------------------------------------------------------#
        
        # Same concept but with the dominant female positions available
        weeks.fams.female.available.to.be.replace.by.nb.female.sa=weeks.fams.female.available.to.be.replace.by.nb.female.sa.all[[j]]
        
        # This is an object with the indexees of the females that had mate prior to the simulations of uninfested fox populations were finished
        # These are the females that can have offspring in the first year simulated. For the following years to be simulated now this object is created in the current script.
        fems.mate=fems.mate.all[[j]]
        
        #------------------------------------------------------#
        
        # This is an object with the weeks the dominant females of each family mate
        
        adult.fem.mating.week=adult.fem.mating.week.all[[j]]
        
        #------------------------------------------------------#
        
        # This is an object with the ids of the dominant males that have cheated in the current year's breeding season
        
        males.that.have.cheated=males.that.have.cheated.all[[j]]
        }
      
      
      
      # STUFF THAT HAPPENS IN THE VERY FIRST WEEK EVERY YEAR SIMULATED #
      # If it is the first week of the current year simulated (year begins), establish which females reproduce during
      # the current year (are pregnant and will have offspring if alive and not a type 2 individual) and
      # the week during which the pregnant females give birth
      
      
      if(d==1){
        
        # Establish reproduction and the litter size per dominant female reproducing
        # define the families that will have offspring during the current year
        index.ad.fem.not.rep<-c(1:59)[rbinom(n =n_fam,size = 1, prob = rep.rate )==0]
        fams.ad.fem.not.rep=paste('Fam_', index.ad.fem.not.rep, sep='') #first define the adult females getting pregnant according to a rep rate independently if they are sick or not. This is assign stotastically. 
        index.ad.fem.rep<-c(1:59)[!(c(1:59)%in%index.ad.fem.not.rep)]
        fams.ad.fem.rep=paste('Fam_', index.ad.fem.rep, sep='') #first define the fams where the adult female gets pregnant according to a rep rate independently if they are sick or not. This is assign stotastically. 
        
        
        #number of pups to be born from dominant females reproducing and adding these pups to the fox population data
        pups_n=sample(x = pups.by.fam.init$N, size = n_fam, replace = T, prob = pups.by.fam.init$Prob) #Number of pups. Randomly generated from a prob. distribution.
        pups_n[index.ad.fem.not.rep]<-0 # assigning zero pups to those adult females not reproducing on the first year
        temp=lapply(unique(paste(rep(paste('Fam_',c(1: n_fam),sep=''),pups_n),'n', sep='')), function(x){
                           paste(rep(paste('Fam_',c(1: n_fam),sep=''),pups_n),'n', sep='')[paste(rep(paste('Fam_',c(1: n_fam),sep=''),pups_n),'n', sep='')==x]}) # to create the pups id's
        temp=paste(paste(unlist(lapply(temp, function(x){paste(x,c(1:length(x)), sep='')})) ,'_yr', sep=''), (years.run+1)+3, sep='') 
        pups.temp<-data.frame(Fam_id=rep(paste('Fam_',c(1: n_fam),sep=''),pups_n),
                              Fox_id=temp,
                              Father_id=rep(fox[fox$Social==3 & fox$Gender==1,]$Fox_id,pups_n),
                              Mother_id=rep(fox[fox$Social==3 & fox$Gender==0,]$Fox_id,pups_n),
                              Gender=rbinom(n =sum(pups_n), size = 1, prob = Pup_sex )  ,
                              Social=rep(1,sum(pups_n)),
                              Age=rep(0,sum(pups_n)),
                              Alive=rep(0,sum(pups_n)),
                              Disp=rep(0,sum(pups_n)),
                              S=rep(1,sum(pups_n)),
                              E=rep(0,sum(pups_n)),
                              I.1=rep(0,sum(pups_n)),
                              I.2=rep(0,sum(pups_n)),
                              Den_id=rep(NA,sum(pups_n)))
        rownames(pups.temp)=seq(from = as.numeric(tail(rownames(fox), n=1))+1, to = as.numeric(tail(rownames(fox), n=1))+nrow(pups.temp), by = 1)
        pups.temp$num=unlist(lapply(strsplit(as.character(pups.temp$Fam_id),split='_'), function(x){as.numeric(x[2])})) # indexing families to sort the new fox dataset when the current year pups to be born are added
        fox$num=unlist(lapply(strsplit(as.character(fox$Fam_id),split='_'), function(x){as.numeric(x[2])})) # indexing families to sort the new fox dataset when the current year pups to be born are added
        fox=rbind(fox,pups.temp)#new dataset for the starting of the next year
        
        #sort the fox population data
        fox=fox[order(fox[,15], -fox[,6]),] #sort by FAm_id and Social position
        fox=subset(fox, select = -c(15) ) # erasing the column num. 
      
        
        # defining days of birth of these pups per family 
        weeks.of.birth=sapply(adult.fem.mating.week[index.ad.fem.rep], function(x){if(x<10){x+7}else{1+(7-(52-x))}})
        
        #------------------------------------------------------#
        
        
        # Defining the future dispersers ##
        if(nrow(fox[fox$Social==1 & fox$Disp==0 & fox$I.2==0 & fox$Age==0,])>0){
          fox[fox$Gender==1 & fox$Social==1 & fox$Disp==0 & fox$Age==0,]$Disp=rbinom(n = nrow(fox[fox$Gender==1 & fox$Social==1 & fox$Disp==0 & fox$Age==0,]), size = 1, prob = prob.to.start.dispersing.male)
          fox[fox$Gender==0 & fox$Social==1 & fox$Disp==0 & fox$Age==0,]$Disp=rbinom(n = nrow(fox[fox$Gender==0 & fox$Social==1 & fox$Disp==0 & fox$Age==0,]), size = 1, prob = prob.to.start.dispersing.female)
          
          disp.per.fam=lapply(c(1:n_fam), function(x){fox[fox$Fam_id==paste('Fam_', x, sep='') & fox$Social==1 & fox$Disp==1 & fox$Age==0,]$Fox_id})
          month.dispersers.start.disperse.if.alive=lapply(c(1:n_fam), function(x){sample(c(5:9), length(disp.per.fam[[x]]), replace = T, prob = disp.prob.dist$Dispersal)})
          weeks.dispersers.start.disperse.if.alive=lapply(month.dispersers.start.disperse.if.alive, function(y){sapply(y, function(x){sample(weeks.by.month[[x]], 1)})})
        }
        
        #------------------------------------------#
        
        
        
        # Adding one year to the alive foxes: birthday of foxes 
        if(nrow(fox[fox$Alive==1 & fox$Social!=1,])>0){
          fox[fox$Alive==1 & fox$Social!=1,]$Age=fox[fox$Alive==1 & fox$Social!=1,]$Age+1} # adding one more year. # at the end of february all foxes have one more year independent on when they were born.
        }
      
        #------------------------------------------#
      
      
      
      # Setting up the mating season information 
      # Create vectors containing te information on dominant females that have mated, dominant males that have cheated and the week the mating pairs mated.      
      if(d==48){ # if the breeding season of the current year begins, then 
        fems.mate=vector(mode = 'character', length = 0) # a vector containing the ids of the females that already mate
        males.that.have.cheated=vector('character',0) # a vector containing the ids of the males that have cheated
        adult.fem.mating.week=sapply(c(1:n_fam), function(x){sample(cheat.t, size = 1)})} # a vector containing the ids of the dominant females mating during the current week
      
      #------------------------------------------------------#
      
      
      # Birth of pups. Mother must be alive. 
      # If in the current week a pregnant alive dominant female is giving birth, then the pups are borned
      
      if(d%in%weeks.of.birth){ # if pups are born during the current week
        fams.alive.mothers.by.week.t=fox[fox$Fox_id%in%unique(as.character(fems.mate)) & fox$Social==3 & fox$Alive==1 & fox$I.2==0,]$Fam_id #families with pregnant dominant female alive
        fams.rep.current.week=fams.ad.fem.rep[which(weeks.of.birth==d)] # which families the dominant female reproduc during the current week
        fams.rep.current.week.mother.alive=fams.rep.current.week[fams.rep.current.week%in%fams.alive.mothers.by.week.t] # fams that actually can reproduce in week t becuase the pregnant dominant female is alive
        fams.rep.current.week.mother.dead=fams.rep.current.week[!(fams.rep.current.week%in%fams.alive.mothers.by.week.t)] # fams that could have generated offspring but the pregnant fdominant female is dead.
        
        if(length(fams.rep.current.week.mother.alive)>0){ # if there are alive mothers giving birth 
          fox[fox$Fam_id%in%fams.rep.current.week.mother.alive & fox$Social==1, ]$Alive=1} # the corresponding pups are borned
        if(length(fams.rep.current.week.mother.dead)>0){ # if there are mothers that were pregnant and giving birth in the current week but now they are dead
          pup.not.to.be.borned=fox[fox$Fam_id%in%fams.rep.current.week.mother.dead & fox$Social==1,]$Fox_id # pups not born are removed from the population 
          fox=fox[!(fox$Fox_id%in%pup.not.to.be.borned), ]}}
      
      #------------------------------------------------------#
      
      
      
      # By the time the pups are old enough, they are weaned and become subdominants (2 months after birth)
      if(any(weeks.of.birth+9==d)){
        fams.pups.to.sa.week.w<-fams.ad.fem.rep[which(weeks.of.birth+9==d)] # idenify families whose pups were born 9 weeks ago
        if(length(fams.pups.to.sa.week.w)>0){
          if(nrow(fox[fox$Fam_id%in%fams.pups.to.sa.week.w & fox$Social==1 & fox$Alive==1,])>0){ # identify living pups becoming subdominant during the current week
            pups.to.sa.week.d=fox[fox$Fam_id%in%fams.pups.to.sa.week.w & fox$Social==1 & fox$Alive==1,]$Fox_id # id of pups to become subadults during the current week
            fox[fox$Fox_id%in%pups.to.sa.week.d,]$Social<-2}else{
              pups.to.sa.week.d=vector(mode='character', length=0)}}else{
                pups.to.sa.week.d=vector(mode='character', length=0)}}	
      
      #------------------------------------------------------#
      
      
      # Define foxes that start dispersing in the current week##
      if(d%in%dispersal.t){ # if during the current week there current year foxes that start to disperse. They can only start dispersing in a specific period pf they ear
        foxes.may.start.to.disperse.if.alive=unlist(disp.per.fam)[which(unlist(weeks.dispersers.start.disperse.if.alive)==d)] # foxes that would start disperse 
        foxes.start.to.disperse=fox[fox$Fox_id%in%foxes.may.start.to.disperse.if.alive & fox$Alive==1 & fox$Social==2 & fox$I.2==0,]$Fox_id # and that are living
        if(length(foxes.start.to.disperse)>0){ 
          fox[fox$Fox_id%in%foxes.start.to.disperse,]$Disp=2}} # id disperser foxes
      
      
      #------------------------------------------------------#
      
      # Current dispersers move
      if(nrow(fox[fox$Alive==1 & fox$Disp==2 & fox$I.2==0 & fox$Social==2,])>0){  # if there are disperser in the system not type I2
        dispersers.per.fam.that.can.move=lapply(c(1:n_fam), function(x){fox[fox$Fam_id==paste('Fam_', x, sep='') & fox$Alive==1 & fox$Disp==2 & fox$Social==2 & fox$I.2==0,]$Fox_id}) # identify dispersers that can actually disperse and alive
        index.fam.disp.destiny=lapply(c(1:n_fam), function(x){sample(c(1:n_fam), size = length(dispersers.per.fam.that.can.move[[x]]), replace = T, prob = weight.distance.to.home.ranges[[j]][[x]])}) #determine the destiny of the dispersers
        fox[fox$Fox_id%in%unlist(dispersers.per.fam.that.can.move),]$Fam_id=paste('Fam_', unlist(index.fam.disp.destiny), sep='')
        fox$num=as.numeric(sapply(sapply(as.character(fox$Fam_id), function(x){strsplit(x = x, split = '_')}), function(y){y[2]}))
        fox=fox[order(fox[,'num'], -fox[,'Social']),] #sorting back by family and social status
        fox=subset(fox, select = -c(15) ) # jsut fixing the fox data
        fox[fox$Fox_id%in%unlist(dispersers.per.fam.that.can.move),]$Den_id=0 # the den the disperser was using is not used anymore by the disperser. The den of destination in the home range it moves into is established later (after the foxes of the home range of destination occupy the dens)
        if(nrow(fox[fox$Disp==2 & fox$Den_id!=0 & fox$I.2==0,])>0)stop('disperser located already')} # checking model functioning
      
      #------------------------------------------------------#
      
      
      # Male subdominants compete for available adult position #
      # First the foxes (subdominants) that are located in the set of close neighbor home ranges are considered as potential replacements (using fam.nb.hr.adult.male.dead.close)
        
      ### Subadult compete for available adult male position (if any).
      
      index.fams.adult.male.dead=which(weeks.fams.male.available.to.be.replace.by.nb.male.sa==0) # fams with available dominant male positions that can be reoccupied this week
      index.fams.adult.male.dead=if(length(index.fams.adult.male.dead)>1){sample(index.fams.adult.male.dead)}else{index.fams.adult.male.dead} # only one per week will be reoccupied to make the competitio process programming
        
        if(length(index.fams.adult.male.dead)>0){ # if there are available male dominant positions to be filled the current week
          for(i in index.fams.adult.male.dead){ # for these families 
            temp=lapply(c(1:59), function(x){fox[fox$Fam_id==paste('Fam_', x, sep='') & fox$Social==2 & fox$Alive==1 & fox$Gender==1 & !(fox$I.2>0),]}) #the set of males subdominants per family. The infested type 2 cannot compete
            adult_female_id_temp=fox[fox$Fam_id==paste('Fam_',i, sep='') & fox$Social==3 & fox$Alive==1 & fox$Gender==0,]$Fox_id # the id of the dominant female in the current family with the male dominant position available
            adult_female_mother_id_temp=as.character(fox[fox$Fam_id==paste('Fam_',i, sep='') & fox$Social==3 & fox$Alive==1 & fox$Gender==0,]$Mother_id) # the id of the mother of the dominats female in the current family. This is to id male relatives that cannot compete for the available dominant male position
            if(length(adult_female_id_temp)==0){adult_female_id_temp=NA} # if there are not any adult_female_id then fix it to make ethis fact understandable
            if(is.na(adult_female_id_temp)){adult_female_id_temp=0}else{adult_female_id_temp=adult_female_id_temp}
            if(length(adult_female_mother_id_temp)==0){adult_female_mother_id_temp=NA} 
            if(is.na(adult_female_mother_id_temp)){adult_female_mother_id_temp=0}else{adult_female_mother_id_temp=adult_female_mother_id_temp} # the same for the mother_id
            temp.2=lapply(c(1:59), function(x){temp[[x]][which(!(temp[[x]]$Mother_id%in%c(adult_female_id_temp,adult_female_mother_id_temp))),]}) #subset subdominants males that are not relative of the dominat female
            temp.2=lapply(c(1:59), function(x){if(ncol(temp.2[[x]])==0 & nrow(temp.2[[x]])>0){temp.2[[x]][FALSE,]}else{temp.2[[x]]}}) # correct if there are no male subdominants left
            
            if(nrow(do.call(rbind,temp.2))>0){
              temp.3=lapply(c(1:59), function(x){rep(weight.distance.to.home.ranges.backwards[[j]][[i]][x], nrow(temp.2[[x]]))}) #provide weight to the subdominant males that will compete for the available male position depedngin on their distance to the home ranges
              
              selected.male.sa.take.ad.pos.hr.ad.male.dead=sample(do.call(rbind,temp.2)$Fox_id, size = 1, prob = unlist(temp.3)) # select the winner male subdominant to take the dominamt posittion avaialble
              fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Social==3 & fox$Gender==1 & fox$Alive==0, c(2,3,4,7,8,10:13)]= # modifiy thte data of the winner subdominant
                fox[which(fox$Fox_id==selected.male.sa.take.ad.pos.hr.ad.male.dead), c(2,3,4,7,8,10:13)]
              fox=fox[!(fox$Fox_id%in%selected.male.sa.take.ad.pos.hr.ad.male.dead & fox$Social==2),]} # Delete from the fox dataset the subdominants (now dominant adults) from their original family and to avoid duplicity of the same fox

          }}
        
        #Fams with alive dominant male get a NA in the object determining the number of weeks that the male dominant position will be taken. Becasue they are already taken
        index.fams.adult.male.alive=sapply(strsplit(as.character(fox[fox$Social==3 & fox$Gender==1 & fox$Alive==1,]$Fam_id), '_'), function(x){as.numeric(x[2])})
        if(length(index.fams.adult.male.alive)>0){
          weeks.fams.male.available.to.be.replace.by.nb.male.sa[index.fams.adult.male.alive]=NA}
    
      
      # Subdominant females compete for available adult position #
      # same concepts as previously but now for dominant females
      
        index.fams.adult.female.dead=which(weeks.fams.female.available.to.be.replace.by.nb.female.sa==0) 
        index.fams.adult.female.dead=if(length(index.fams.adult.female.dead)>1){sample(index.fams.adult.female.dead)}else{index.fams.adult.female.dead}
        
        if(length(index.fams.adult.female.dead)>0){
          for(i in index.fams.adult.female.dead){
            temp=lapply(c(1:59), function(x){fox[fox$Fam_id==paste('Fam_', x, sep='') & fox$Social==2 & fox$Alive==1 & fox$Gender==0 & !(fox$I.2>0),]})
            adult_male_id_temp=as.character(fox[fox$Fam_id==paste('Fam_',i, sep='') & fox$Social==3 & fox$Alive==1 & fox$Gender==1,]$Fox_id)
            adult_male_mother_id_temp=as.character(fox[fox$Fam_id==paste('Fam_',i, sep='') & fox$Social==3 & fox$Alive==1 & fox$Gender==1,]$Mother_id)
            if(length(adult_male_id_temp)==0){adult_male_id_temp=NA}
            if(is.na(adult_male_id_temp)){adult_male_id_temp=0}else{adult_male_id_temp=adult_male_id_temp}
            if(length(adult_male_mother_id_temp)==0){adult_male_mother_id_temp=NA}
            if(is.na(adult_male_mother_id_temp)){adult_male_mother_id_temp=0}else{adult_male_mother_id_temp=adult_male_mother_id_temp}
            temp.2=lapply(c(1:59), function(x){temp[[x]][which(temp[[x]]$Father_id!=adult_male_id_temp | temp[[x]]$Mother_id!=adult_male_mother_id_temp),]})
            temp.2=lapply(c(1:59), function(x){if(ncol(temp.2[[x]])==0 & nrow(temp.2[[x]])>0){temp.2[[x]][FALSE,]}else{temp.2[[x]]}})
            
            if(nrow(do.call(rbind,temp.2))>0){
              temp.3=lapply(c(1:59), function(x){rep(weight.distance.to.home.ranges.backwards[[j]][[i]][x], nrow(temp.2[[x]]))})
              
              selected.female.sa.take.ad.pos.hr.ad.female.dead=sample(do.call(rbind,temp.2)$Fox_id, size = 1, prob = unlist(temp.3))
              fox[fox$Fam_id==paste('Fam_', i, sep='') & fox$Social==3 & fox$Gender==0 & fox$Alive==0, c(2,3,4,7,8,10:13)]=
                fox[which(fox$Fox_id==selected.female.sa.take.ad.pos.hr.ad.female.dead), c(2,3,4,7,8,10:13)]
              
              fox=fox[!(fox$Fox_id%in%selected.female.sa.take.ad.pos.hr.ad.female.dead & fox$Social==2),]}
          }}
        
        index.fams.adult.female.alive=sapply(strsplit(as.character(fox[fox$Social==3 & fox$Gender==0 & fox$Alive==1,]$Fam_id), '_'), function(x){as.numeric(x[2])})
        if(length(index.fams.adult.female.alive)>0){
          weeks.fams.female.available.to.be.replace.by.nb.female.sa[index.fams.adult.female.alive]=NA}
      
      
#---------------------------------------------------------------------------------------------------------#
    
    
    
      # DEN DISPERSING AND DEN SHARING !!
      # How are the foxes spread across dens per family during the current week
      # This script is run weekly but during the first week of a new year it is established how the foxes will share dens and when they change dens for the rest of the year

      # run the code hosted in github
      eval(parse(text =getURL("https://raw.githubusercontent.com/dmontecino/SJKF_Model/master/Den_changing_and_sharing_script.R", ssl.verifypeer = FALSE)))
      #https://github.com/dmontecino/SJKF_Model/master/Den_changing_and_sharing_script.R 
#---------------------------------------------------------------------------------------------------------#


      
            
      # INFECTIOUS PROCESS #
      # How is mange transmitted to susceptoble foxes during the current week

      # run the code hosted in github
      eval(parse(text =getURL("https://raw.githubusercontent.com/dmontecino/SJKF_Model/master/Disease_transmission_script.R", ssl.verifypeer = FALSE)))
      #https://github.com/dmontecino/SJKF_Model/master/Disease_transmission_script.R
#---------------------------------------------------------------------------------------------------------#
      
      
      
      
      # COLLECT THE WEEKLY DATA #
      # In the model the foxes die at the end of the week so the data is obtained at this point
      
      # run the code hosted in github
      eval(parse(text =getURL("https://raw.githubusercontent.com/dmontecino/SJKF_Model/master/Data_collection_script.R", ssl.verifypeer = FALSE)))
      #https://github.com/dmontecino/SJKF_Model/blob/master/Data_collection_script.R
#---------------------------------------------------------------------------------------------------------#
      
      
      #Survival of the foxes by the end of the current week
      
        #Pups surviving week t. If the dominant female is dead, and no one adopts them in a week (new dominant female) they all die. ###### 
        
        fams.pup.alive=unique(fox[fox$Alive==1 & fox$Social==1,]$Fam_id) #families with pups currently alive
        if(length(fams.pup.alive)>0){ # if there are such families
          fams.af.dead=fox[fox$Gender==0 & fox$Social==3 & fox$Alive==0,]$Fam_id # families in whch the dominant female is dead
          fams.pups.die=fams.pup.alive[fams.pup.alive%in%fams.af.dead] # families were the pups die becuase the mother is dead
          if(length(fams.pups.die)>0){ #if there are such pups, then they die
            fox[fox$Fam_id%in%fams.pups.die & fox$Social==1,]$Alive=0}} # 'killing' pups in the corresponding family
        
      
        # Pups also die depending on a probability
        
        if(nrow(fox[fox$Alive==1 & fox$Social==1, ])>0){ # if there are pups alive in the system
          fox[fox$Alive==1 & fox$Social==1, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==1, ]), size=1, prob = pup.daily.surv.prob)} #probabilistic realization of the surivival of the pups
        
      
        
        # Survival of the female and male dominants, and the survival of the male and female subdominants that are not dispersing.
        
        fox[fox$Alive==1 & fox$Social==3 & fox$Gender==0, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==3 & fox$Gender==0, ]), size=1, prob = ad.daily.surv.prob) # probabilitic realization of the surivival of the dominant females (1 survives, 0 otherwise)
        fox[fox$Alive==1 & fox$Social==3 & fox$Gender==1, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==3 & fox$Gender==1, ]), size=1, prob = ad.daily.surv.prob) # probabilitic realization of the surivival of the dominant males (1 survives, 0 otherwise)
        fox[fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1) & fox$Gender==0, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1) & fox$Gender==0, ]), size=1, prob = sa.fem.daily.surv.prob) # probabilitic realization of the surivival of the subdominant females (1 survives, 0 otherwise)
        fox[fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1) & fox$Gender==1, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==2 & fox$Disp%in%c(0,1) & fox$Gender==1, ]), size=1, prob = sa.male.daily.surv.prob) # probabilitic realization of the surivival of the subdominant males (1 survives, 0 otherwise)
        
        
        
        # Survival of the male and female subdominants that are dispersing.
        
        fox[fox$Alive==1 & fox$Social==2 & fox$Disp==2 & fox$Gender==0, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==2 & fox$Disp==2 & fox$Gender==0, ]), size=1, prob = disp.sa.fem.daily.surv.prob) # probabilitic realization of the surivival of the disperser females (1 survives, 0 otherwise)
        fox[fox$Alive==1 & fox$Social==2 & fox$Disp==2 & fox$Gender==1, ]$Alive=rbinom(n=nrow(fox[fox$Alive==1 & fox$Social==2 & fox$Disp==2 & fox$Gender==1, ]), size=1, prob = disp.sa.male.daily.surv.prob) # probabilitic realization of the surivival of the disperser males (1 survives, 0 otherwise)
        
        # Death as cause of sarcoptes mange.
        foxes.that.may.die.of.mange=fox[fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2>=4 & fox$Alive==1,]$Fox_id # the set of foxes that die of sarcoptic mange: those infested individuals with at leat a month as infectious type II
        fox[fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2>=4 & fox$Alive==1,]$Alive=rep(0,nrow(fox[fox$S!=1 & fox$E==0 & fox$I.1==0 & fox$I.2>=4 & fox$Alive==1,])) # Correct the alive status of the foxes that die of sarcoptic mange
        foxes.that.died.of.mange=fox[fox$Fox_id%in%foxes.that.may.die.of.mange & fox$Alive==0,]$Fox_id
      
  
        #------------------------------------------------------#
  
      # Disease progression in exposed and infectious individuals
      # Adding one more week to the current sarcoptic mange status of foxes. Infection at the beginning of the week
      fox[fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2>=1,]$I.2=fox[fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2>=1,]$I.2+1 #one more week as infectious type II
      fox[fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2==0.1,]$I.2=fox[fox$S==0 & fox$E==0 & fox$I.1==0 & fox$I.2==0.1,]$I.2+0.9 # new foxes as infectious type II fufill 1 week in this compartment
      fox[fox$S==0 & fox$E==0 & fox$I.1>=1 & fox$I.2==0,]$I.1=fox[fox$S==0 & fox$E==0 & fox$I.1>=1 & fox$I.2==0,]$I.1+1 #one more week as infectious type I
      fox[fox$S==0 & fox$E==0 & fox$I.1==0.1 & fox$I.2==0,]$I.1=fox[fox$S==0 & fox$E==0 & fox$I.1==0.1 & fox$I.2==0,]$I.1+0.9 # new foxes as infectious type I fufill 1 week in this compartment
      fox[fox$S==0 & fox$E>=1 & fox$I.1==0 & fox$I.2==0,]$E=fox[fox$S==0 & fox$E>=1 & fox$I.1==0 & fox$I.2==0,]$E+1 #those exposed one more week as exposed
      fox[fox$S==0 & fox$E==0.1 & fox$I.1==0 & fox$I.2==0,]$E=fox[fox$S==0 & fox$E==0.1 & fox$I.1==0 & fox$I.2==0,]$E+0.9 # new exposed finish their first week as exposed
      
      # Moving infested foxes to the next sarcoptic mange compartment
      if(nrow(fox[fox$S==0 & fox$E==2 & fox$I.1==0 & fox$I.2==0,])>0){ # if there are exposed foxes for 2 weeks
        fox[fox$S==0 & fox$E==2 & fox$I.1==0 & fox$I.2==0,]$I.1=0.1 # they become infectious type I
        fox[fox$S==0 & fox$E==2 & fox$I.1==0.1 & fox$I.2==0,]$E=0}  # correcting exposed status in those foxes just moved to infectious type I
      
      if(nrow(fox[fox$S==0 & fox$E==0 & fox$I.1==8 & fox$I.2==0,])>0){ # if there are foxes as infectious type I for 8 weeks  htye vbecome infectious type II
        fox[fox$S==0 & fox$E==0 & fox$I.1==8 & fox$I.2==0,]$I.2=0.1 # they become infectious type II
        fox[fox$S==0 & fox$E==0 & fox$I.1==8 & fox$I.2==0.1,]$I.1=0}  # correcting infectious status in those foxes that just moved to infectious type I
      
      if(any(fox$E>2))stop('Exposed for more than 2') # check point
      
      # Infestation progression of dens
      den.data.set[den.data.set$I>=1,]$I=den.data.set[den.data.set$I>=1,]$I+1
      if(nrow(den.data.set[den.data.set$I==0.1,])>0){
        den.data.set[den.data.set$I==0.1,]$I=1} # add one more week to those infested dens as such
      
      #------------------------------------------------------#
      
      
      # Available dominant positions have been avaialble for one more week
      weeks.fams.male.available.to.be.replace.by.nb.male.sa=weeks.fams.male.available.to.be.replace.by.nb.male.sa-1 # the same objects weeks but discounting one
      weeks.fams.male.available.to.be.replace.by.nb.male.sa=ifelse(weeks.fams.male.available.to.be.replace.by.nb.male.sa<0,0,weeks.fams.male.available.to.be.replace.by.nb.male.sa) # correcting so it does not have negaetive values
      weeks.fams.female.available.to.be.replace.by.nb.female.sa=weeks.fams.female.available.to.be.replace.by.nb.female.sa-1 #noe for the dominant female posistions
      weeks.fams.female.available.to.be.replace.by.nb.female.sa=ifelse(weeks.fams.female.available.to.be.replace.by.nb.female.sa<0,0,weeks.fams.female.available.to.be.replace.by.nb.female.sa) #correctino so it does not become negative
      
      #------------------------------------------------------#
      
      
      
      # Dominant foxes that become infectious type 2 loose their dominancy and behave as subdominants
      # They spend the very first week as infectious type 2 with the dominancy
      
      dom.foxes.loose.status.due.infection=fox[fox$I.2>=1 & fox$Social==3 & fox$Alive==1,] # dominant adults that are infectious type II
      if(nrow(dom.foxes.loose.status.due.infection)>0){ # if there are such foxes. Reset the data and make these positions available
        fox[fox$Fox_id%in%dom.foxes.loose.status.due.infection$Fox_id, ]$I.2=0 
        fox[fox$Fox_id%in%dom.foxes.loose.status.due.infection$Fox_id, ]$S=1
        fox[fox$Fox_id%in%dom.foxes.loose.status.due.infection$Fox_id, ]$Mother_id=NA
        fox[fox$Fox_id%in%dom.foxes.loose.status.due.infection$Fox_id, ]$Father_id=NA
        fox[fox$Fox_id%in%dom.foxes.loose.status.due.infection$Fox_id, ]$Alive=0
        fox[fox$Fox_id%in%dom.foxes.loose.status.due.infection$Fox_id,]$Fox_id=NA
        dom.foxes.loose.status.due.infection$Social=2 # and they behave as subdominants
        fox=rbind(fox,dom.foxes.loose.status.due.infection) # update the fox dataset
        fox$num=as.numeric(sapply(sapply(fox$Fam_id, function(x){strsplit(x = x, split = '_')}), function(y){y[2]}))
        fox=fox[order(fox[,'num'], -fox[,'Social']),] #sorting back the fox dataset by family and social status
        fox=subset(fox, select = -c(15) )} #fixing the dataset

      #------------------------------------------------------#
      

      
        # Defining the new families that became with dominant positions avaialble and establish the weeks when these position will be avaiable to be retaken 
        # by subdominats competing for the available positions.
      
        index.of.new.fams.without.dominant.adult.male=sapply(strsplit(as.character(fox[fox$Fam%in%paste('Fam_', c(1:n_fam), sep='') & fox$Social==3 & fox$Alive==0 & fox$Gender==1,]$Fam_id), '_'), function(x){as.numeric(x[2])}) # families in which the male dominant position is available
        index.of.new.fams.without.dominant.adult.male=index.of.new.fams.without.dominant.adult.male[index.of.new.fams.without.dominant.adult.male%in%which(is.na(weeks.fams.male.available.to.be.replace.by.nb.male.sa))] # new ones
        if(length(index.of.new.fams.without.dominant.adult.male)>0){ # if there are such families then define when these positions will be retaken
          weeks.fams.male.available.to.be.replace.by.nb.male.sa[index.of.new.fams.without.dominant.adult.male]=sample(weeks.adult.male.position.is.refill.ater.death, size = length(index.of.new.fams.without.dominant.adult.male), replace = T)} # randomly select in how many weeks these avaialble dominant male positions will be retaken (if subdominants compete for it)
        
        #now the same concept but for dominant females
        index.of.new.fams.without.dominant.adult.female=sapply(strsplit(as.character(fox[fox$Fam%in%paste('Fam_', c(1:n_fam), sep='') & fox$Social==3 & fox$Alive==0 & fox$Gender==0,]$Fam_id), '_'), function(x){as.numeric(x[2])})
        index.of.new.fams.without.dominant.adult.female=index.of.new.fams.without.dominant.adult.female[index.of.new.fams.without.dominant.adult.female%in%which(is.na(weeks.fams.female.available.to.be.replace.by.nb.female.sa))]
        if(length(index.of.new.fams.without.dominant.adult.female)>0){
          weeks.fams.female.available.to.be.replace.by.nb.female.sa[index.of.new.fams.without.dominant.adult.female]=sample(weeks.adult.female.position.is.refill.ater.death, size = length(index.of.new.fams.without.dominant.adult.female), replace = T)}
      
      
      #------------------------------------------------------#
      
      
      # Save data about death due to sarcoptic mange
       
      #   Fams_extincted[[j]][[52*years.run+d]]=ifelse(length(which(sapply(lapply(paste('Fam_', c(1:59), sep=''), function(x){fox[fox$Fam_id==x,]$Alive}), function(y){all(y==0)})))>0, 
      #                                                paste('Fam_', which(sapply(lapply(paste('Fam_', c(1:59), sep=''), function(x){fox[fox$Fam_id==x,]$Alive}), function(y){all(y==0)})), sep=''),
      #                                                vector('integer',length = 0))
      #   Dead_adults_mange[[j]][[52*years.run+d]]=as.character(fox[fox$Fox_id%in%foxes.that.died.of.mange & fox$Social==3,]$Fox_id)# id of adults dead becuase of mange
      #   Dead_subadults_mange[[j]][[52*years.run+d]]=as.character(fox[fox$Fox_id%in%foxes.that.died.of.mange & fox$Social==2,]$Fox_id) # id of adults dead becuase of mange
      #   Dead_pups_mange[[j]][[52*years.run+d]]=as.character(fox[fox$Fox_id%in%foxes.that.died.of.mange & fox$Social==1,]$Fox_id) # id of adults dead becuase of mange
      
      #------------------------------------------------------#
      
      # The dominant positions that become available during the current week get the Fox_id and disease status clear.
      # Dead dominants are recognized then because they do not have a Fox_id
      
        if(nrow(fox[fox$Alive==0 & fox$Social==3, ])>0){
          fox[fox$Alive==0 & fox$Social==3, ]$Fox_id<-NA
          fox[fox$Alive==0 & fox$Social==3, ]$S<-1
          fox[fox$Alive==0 & fox$Social==3, ]$E<-0
          fox[fox$Alive==0 & fox$Social==3, ]$I.1<-0
          fox[fox$Alive==0 & fox$Social==3, ]$I.2<-0}

      #------------------------------------------------------#
      
      
      # Clear the fox dataset to leave only living foxes and dominant position (dead or alive)
      
      fox=fox[fox$Alive==1 | is.na(fox$Fox_id) | fox$Alive==0 & fox$Social==1 & is.na(fox$Den_id),]
      #live foxes, dead dominants to mantain their position and pups to be born without den assigned yet 
      
      #check point  
      if(all(table(fox[fox$Social==3,]$Fam_id)==2)==F)stop('Dominant positions missing in at least one family')
      if(any(table(fox$Fox_id)>1)==T)stop('Repeated fox in the dataset') # check point

      #------------------------------------------------------#
      
      #show the week of the year just completed if the option is activated
      if(show.week.of.the.year.finished==T){cat("d=", d,", ",sep="")} 
      
      # move d to the next week
      if(d==52){years.run=years.run+1}
      
      # close the loop of the current week    
      } 
      
      #------------------------------------------------------#
        
    #show the week of the year just completed if the option is activated
    if(show.year.finished==T){cat("y=",y,", ",sep="")} 
    
    # start the next year in the first week			
    d=1 
    
    # stop when all years have been simulated  
    if(years.run==years_simulate){break} 

  }# close the loop of the current year
  
      #------------------------------------------------------#
  
  years.run=0 # start the next iteration from the first year
  
  #show the iteration just completed if the option is activated
  if(show.iteration.finished==T){cat("j=",j,", ",sep="")}
  
}# close the loop of the iterations

      #------------------------------------------------------#
 
output=lapply(to.save, get)
names(output)=to.save

assign("output", output, envir=globalenv())

