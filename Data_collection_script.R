##############################################################################################
# --- CODE OF THE MODEL TO ASSESS THE ROLE OF DENS IN THE SPREAD, INVASION AND PERSISTENCE---#
# --- OF SARCOPTIC MANGE IN THE SAN JOAQUIN FOX POPULATION LOCATED IN BAKERSFIELD, CA -------#
# --- SPECIFICALLY THIS SCRIPT COLLECTS THE DATA OF THE CURRENT WEEK ------------------------#
##############################################################################################

# Author:Diego Montecino-Latorre
# dmontecino@ucdavis.edu
# Details of the model are published in the journal "Epidemics" January 2019

# The following lines collect the data for the week d of the years.run year of the iteration j. Briefly: it collected the data of the current week

# These objects are matrices with the number of rows equal to the number of iterations and the colums equal to the current week (if 2 years are simulated then 52*2 columns)
# lists contain nested lists: the higher level is the iteration and the each iteration list contains a list per simulated week (if 2 years are simulated then 52*2 columns) with the information (id's)
# of the current week.

# Matrices with total numbers
N[j,52*years.run+d]=nrow(fox[fox$Alive==1,])
Adults[j,52*years.run+d]=nrow(fox[fox$Social==3 & fox$Alive==1,])
Adults_male[j,52*years.run+d]=nrow(fox[fox$Social==3 & fox$Alive==1 & fox$Gender==1,])
Adults_fem[j,52*years.run+d]=nrow(fox[fox$Social==3 & fox$Alive==1 & fox$Gender==0,])
Subadults[j,52*years.run+d]=nrow(fox[fox$Social==2 & fox$Alive==1,])
Disperse[j,52*years.run+d]=nrow(fox[fox$Social==2 & fox$Alive==1 & fox$Disp==2,])
Subadults_male[j,52*years.run+d]=nrow(fox[fox$Social==2 & fox$Alive==1 & fox$Gender==1,])
Subadults_fem[j,52*years.run+d]=nrow(fox[fox$Social==2 & fox$Alive==1 & fox$Gender==0,])
Pups[j,52*years.run+d]=nrow(fox[fox$Social==1 & fox$Alive==1,])
Exposed[j,52*years.run+d]= nrow(fox[fox$E>=0.1 & fox$Alive==1,])
Infected1[j,52*years.run+d]= nrow(fox[fox$I.1>=0.1 & fox$Alive==1,])
Infected2[j,52*years.run+d]=nrow(fox[fox$I.2>=0.1 & fox$Alive==1,])

# Lists with the id's
Fams_Exposed[[j]][[52*years.run+d]]=unique(as.character(fox[fox$E>=0.1 & fox$Alive==1,]$Fam_id))
Fams_Infected1[[j]][[52*years.run+d]]=unique(as.character(fox[fox$I.1>=0.1 & fox$Alive==1,]$Fam_id))
Fams_Infected2[[j]][[52*years.run+d]]=unique(as.character(fox[fox$I.2>=0.1 & fox$Alive==1,]$Fam_id))
Fams_mange[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$I.2>=0.1 | fox$Alive==1 & fox$I.1>=0.1 | fox$Alive==1 & fox$E>=0.1 ,]$Fam_id))
Adult_sus[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==3 & fox$S==1,]$Fox_id)) 
Adult_exp[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==3 & fox$E>0,]$Fox_id)) 
Adult_i1[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==3 & fox$I.1>0,]$Fox_id)) 
Adult_i2[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==3 & fox$I.2>0,]$Fox_id)) 
Subadult_sus[[j]][[52*years.run+d]]= unique(as.character(fox[fox$Alive==1 & fox$Social==2 & fox$S==1,]$Fox_id))
Subadult_exp[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==2 & fox$E>0,]$Fox_id)) 
Subadult_i1[[j]][[52*years.run+d]]= unique(as.character(fox[fox$Alive==1 & fox$Social==2 & fox$I.1>0,]$Fox_id))
Subadult_i2[[j]][[52*years.run+d]]= unique(as.character(fox[fox$Alive==1 & fox$Social==2 & fox$I.2>0,]$Fox_id))
Pup_sus[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==1 & fox$S==1,]$Fox_id)) 
Pup_exp[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==1 & fox$E>0,]$Fox_id)) 
Pup_i1[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==1 & fox$I.1>0,]$Fox_id)) 
Pup_i2[[j]][[52*years.run+d]]=unique(as.character(fox[fox$Alive==1 & fox$Social==1 & fox$I.2>0,]$Fox_id)) 
Dens_susc[[j]][[52*years.run+d]]=unique(as.character(den.data.set[den.data.set$I==0,]$Den_id))
Dens_inf[[j]][[52*years.run+d]]=unique(as.character(den.data.set[den.data.set$I>0,]$Den_id))


