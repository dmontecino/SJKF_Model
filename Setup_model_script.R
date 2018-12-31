##############################################################################################
# --- CODE OF THE MODEL TO ASSESS THE ROLE OF DENS IN THE SPREAD, INVASION AND PERSISTENCE---#
# --- OF SARCOPTIC MANGE IN THE SAN JOAQUIN FOX POPULATION LOCATED IN BAKERSFIELD, CA -------#
##############################################################################################

# Author:Diego Montecino-Latorre
# dmontecino@ucdavis.edu
# Details of the model are published in the journal "Epidemics" January 2019

###########################
# --- LOAD THE LIBRARY ---#
###########################

# function to load packages 
using=function(...) {
  libs=unlist(list(...))
  req=unlist(lapply(libs,require,character.only=TRUE))
  need=libs[req==FALSE]
  n=length(need)
  if(n>0){
    libsmsg=if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg=paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
    if(winDialog(type = c("yesno"), libsmsg)=="YES"){       
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
}

################################
# --- LOAD PACKAGES NEEDED --- #
################################
# if packages needed are not installed, it will ask for permission to install them.

using("sp","rgeos","pedometrics","partitions", "gtools")

############################
# --- LOAD FILES NEEDED ---#
############################

# Load the data needed to run the model
# The data consists of 59 home ranges (1000 realizations of home ranges - 1 per iteration), a data informed number of dens per home range
# the first set of neighbor home ranges for each home range per iteration (closer than 1480 m from the centroid - a home diameter), 
# the second set of neighbor home ranges for each home range per iteration (between 1480 - 2960 m from the centroid),
# the weigths of home ranges as destination of disperser individuals when moving away from the origin home range
# the weigths of home ranges as destination of disperser individuals when moving backwards (excludes home range of origin)
# the spatial location where to insert the first exposed individual (a spatial polygon that selects the home ranges in which the first exposed fox can be inserted)

## --- HOME RANGES ACROSS ITERATIONS ---- ##
# download as temp and load the all the simulated home ranges per iteration. Same applies for the following loads
# the location of the home ranges is informed by Bakersfield's suitability for san joaquin kit foxes across the city

# takes about 5 mins to load

# start.time=Sys.time()
# test=readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7UnhPbXU1cW4zRkU", open = "rb"))
# readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7UnhPbXU1cW4zRkU", open = "rb"))
# end.time=Sys.time()



## --- DENS PER HOME RANGE ACROSS ITERATIONS ---- ##
# the location of the dens is informed bu the land types withing the home ranges (see Frost 2005)

# takes about 5 mins to load
all_dens_avail_by_hr_by_it=readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7ZlgzcU9JaDM3emM", open = "rb"))



## --- SET OF CLOSE NEIGHBOR HOME RANGES PER HOME RANGE ACROSS ITERATIONS ---- ##

# takes about 2 seconds
hr_nb_1480=readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7ZUJMcmNXblZpczQ", open = "rb"))



## --- SET OF FARER NEIGHBOR HOME RANGES PER HOME RANGE ACROSS ITERATIONS ---- ##

# takes about 2 seconds
hr_nb_more_1480_less_2960=readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7T25YV1RIdmNvRDg", open = "rb"))



## --- WEIGHT OF HOME RANGES TO REACH BY DISPERSERS PER ITERATION ---- ##
# this file is 25 MB
# takes about 25 seconds

weight.distance.to.home.ranges=readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7WWZlVlhoRVZOWEk", open = "rb"))



## --- WEIGHT OF HOME RANGES TO REACH BY DISPERSERS EXCLUDING THE ORIGIN ONE PER ITERATION ---- ##
# this file is 3 MB 
# takes about 4 seconds

weight.distance.to.home.ranges.backwards=readRDS(url(description="https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7RUw3THA2WGlsRDg", open = "rb"))




## --- SPATIAL BOX TO SELECT HOME RANGES TO INTRODUCE THE FIRST EXPOSED INDIVIDUAL --- ##

# #spatial box to sample home ranges where to allocate the first sick individual
box=SpatialPointsDataFrame(data.frame(x=c(82742.43, 87783.89), y=c(-283617.6, -289991.8)), data = data.frame(Id=c(1,2)), proj4string = CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
box=bbox2sp(obj = box, sp = 'SpatialPolygonsDataFrame', keep.crs = T)




### --- FOX POPULATIONS TO START THE SIMULATIONS IN EPIDEMIC SCENARIO --- ###

# the fox population
# takes about 5 seconds
fox.no.disease.yet=readRDS(url(description="https://drive.google.com/uc?export=download&id=1_TYsebDfieA7NEF0ees9VuahatUH0VJk", open = "rb"))

# ----- other features of the fox population by the time the simulation stopped ---#

# Load the data about how many weeks were the male positions available 
# by the time the simulations of uninfested fox populations (see the previous object) were finished. For the following years to be simulated now this object is created in the current script.
# takes about 2 seconds
weeks.fams.male.available.to.be.replace.by.nb.male.sa.all=readRDS(url(description="https://drive.google.com/uc?export=download&id=1-HWHKMJa6F-YltBOXu6jD9DKVml32lHL", open = "rb"))

# Same concept but with the dominant female positions available
# takes about 2 seconds
weeks.fams.female.available.to.be.replace.by.nb.female.sa.all=readRDS(url(description="https://drive.google.com/uc?export=download&id=1-GF37qIfRqEr7G_awVZYLtyVMD2I5RfF", open = "rb"))

# This is an object with the indexees of the females that had mate prior to the simulations of uninfested fox populations were finished
# These are the females that can have offspring in the first year simulated. For the following years to be simulated now this object is created in the current script.
# takes about 2 seconds
fems.mate.all=readRDS(url(description="https://drive.google.com/uc?export=download&id=1-C2hNqOPXcr3rnsJXp7ohGaXkBx83XsP", open = "rb"))

# the week during which each adult dominat female will mate. Some of the females may have not mate by the time the simulation ended but they were already set to mate during the current breeding period.
# takes about 2 seconds
adult.fem.mating.week.all=readRDS(url(description="https://drive.google.com/uc?export=download&id=1-DcpxVc5VyP2_elbsC9FXYdEcr0JA_WI", open = "rb"))

# list of males that have cheated alerady by the time the simulation to construct the fox populations was over.
males.that.have.cheated.all=readRDS(url(description="https://drive.google.com/uc?export=download&id=1-49sOC1GOxsLAmi2bA05ptvtOqSAmrhv", open = "rb"))


###########################################################
### ---- TIME UNITS AND TIME PERIODS IN THE MODEL ---- ####
###########################################################

# definition of the weeksof the year contained in each month of the model's standard year
weeks.by.month=list(c(1:4), c(5:8), c(9:13), c(14:17), c(18:22), c(23:26), c(27:30), c(31:35), c(36:39), c(40:43), c(44:48), c(49:52))  

#weeks of the year when breeding occurs (as defined by Koopman et al 1998)
breed.t=c(48:52,1:5) 

#weeks of the year when pup rearing occurs (as defined by Koopman et al 1998)
rearing.t=c(6:18) # weeks when it is pup rearing period

#weeks of the year without breeding or rearing pups
other.t=c(19:47)

# weeks of the year when dispersal juveniles can star dispersing. May to September.
dispersal.t=c(19:39) 

# weeks of the year when breeding and cheat can occur.
cheat.t=c(48:52,1:2)

#############################
### --- FIXED OBJECTS --- ###
#############################

# total number of familes of foxes and also home ranges.
n_fam=59

# The number of years so far in the simulation. At the beggining of course is zero years run.  
years.run=0 

# object to establish that the foxes do share dens
sharing<- 'Yes' 

#object to establish that the disease is tracked
track.disease=T 

###########################################
##### ------ MODEL PARAMETERS ------- #####
###########################################

# For details on the model parameters refer to the published manuscript in the journal "Epidemics": 
# "Assesing the role of dens ..."



### ---- SAN JOAQUIN KIT FOX NATURAL HISTORY PARAMETERS ---- ###

#litter sizes and the corresponding probabilities of each one
pups.by.fam.init=data.frame(N=c(1,2,3,4,5,6), Prob=c(0.095,0.238,0.429,0.095,0.095,0.048)) #Spencer 1992

# the number of weeks it takes for a subdominant to take the dominant poisiton of the dominant male and female, respectively.
weeks.adult.male.position.is.refill.ater.death<-c(2:7)
weeks.adult.female.position.is.refill.ater.death<-c(2:7)

# weekly survival probability of adults
ad.anual.mort.prob=1-0.78
ad.anual.mort.rate=-log(1-ad.anual.mort.prob)
ad.weekly.mort.prob=1-exp(-ad.anual.mort.rate/52)
ad.weekly.surv.prob=ad.daily.surv.prob=1-ad.weekly.mort.prob #name mantained to avoid changing label in each script

# weekly survival probability of female subdominants
sa.fem.anual.mort.prob=1-0.535
sa.fem.anual.mort.rate=-log(1-sa.fem.anual.mort.prob)
sa.fem.weekly.mort.prob=1-exp(-sa.fem.anual.mort.rate/43) # 43 from 304/7
sa.fem.weekly.surv.prob=sa.fem.daily.surv.prob=1-sa.fem.weekly.mort.prob #name mantained to avoid changing label in each code
sa.fem.weekly.surv.prob=sa.fem.daily.surv.prob=0.994

# weekly survival probability of male subdominants
sa.male.anual.mort.prob=1-0.62
sa.male.anual.mort.rate=-log(1-sa.male.anual.mort.prob)
sa.male.weekly.mort.prob=1-exp(-sa.male.anual.mort.rate/15) # 15 from 104/7
sa.male.weekly.surv.prob=sa.male.daily.surv.prob=1-sa.male.weekly.mort.prob #name mantained to avoid changing label in each code
sa.male.weekly.surv.prob=sa.male.daily.surv.prob=0.988

# weekly survival probability of pups
pup.anual.mort.prob=1-0.8
pup.anual.mort.rate=-log(1-pup.anual.mort.prob)
pup.weekly.mort.prob=1-exp(-pup.anual.mort.rate/15) # 15 from 104/7
pup.weekly.surv.prob=pup.daily.surv.prob=1-pup.weekly.mort.prob #name mantained to avoid changing label in each code

# weekly survival probability of dispersers
disp.sa.fem.daily.surv.prob= 0.6 
disp.sa.male.daily.surv.prob=0.6 

# pregnancy probability of a dominat female. The name of the object is misleading but mantain to avoid script problems.
preg_repro=rep.rate=0.788 

# probability a male pup born in the current year becomes a diperser
prob.to.start.dispersing.male=0.35 #0.5

# probability a female pup born in the current year becomes a diperser
prob.to.start.dispersing.female=0.1 #0.23

# Parameters defining when they will start to disperse
#object defining probabilities to start dispersing per month Following Koopman 1998.
disp.prob.dist=data.frame(Month=month.name, Dispersal=c(9.92, 0.60, 4.33, 0.00, 4.41, 9.81, 28.53, 24.82, 11.72, 4.29, 4.30, 8.02))
disp.prob.dist=disp.prob.dist[disp.prob.dist$Month%in%c('May', 'June', 'July', 'August', 'September'),] #subsetting to May:September as a model simplification
disp.prob.dist$Dispersal=disp.prob.dist$Dispersal/sum(disp.prob.dist$Dispersal) # #provide weights



# ----DEN SHARING AND CHANGING PARAMETERS --- #
# The following parameters are informed by Koopman et al, 1998

# Probability to change den during the breeding period and the pup rearing period.
# These periods have a single prob becuase Koopman 1998 says there are not statistically different 
# in terms of the number of dens used in both seasons.

prob.den.change.breed.t.rearing.t=1-exp(-6/22)
prob.den.change.breed.t.rearing.t.week.1.18=1-exp(-5.72/18) 
# it is expected that 7 dens will be used in total during these seasons combined, but 4 weeks have to be discounted (the last 4 weeks of th previous year) so the expected is 5.72 dens in these 18 weeks.

#probability to change during the early weeks of the breeding season
prob.den.change.breed.t.48.52=1-exp(-0.59/4) # here the expected in the last 5 weeks of the year is 1.59 dens to be used but in the very first week of this period, all foxes are changed to a new den in order to 
# fulfill the period rules. Therefore, it is one less den and one less week

prob.den.change.other=1-exp(-13/28) #similarly but for the rest of the year (not during the breeding - pup rearing period as deinfed by Koopman et al., 1998)



#################################################
## --- OBJECTS TO SAVE DATA FROM THE MODEL --- ##
#################################################

N= # Total population alive per day
Adults= #Total alive adults per day
Adults_male= # Total alive dominant males per day
Adults_fem= # Total alive dominant females per day
Subadults= # Total alive subadults per day
Subadults_male= # Total alive subdominant males per day
Subadults_fem= # Total alive subdominant females per day
Disperse=
Pups= # Total alive pups per day
Exposed= # Total alive exposed foxes per day
Infected1= # Total alive infected 1 per day
Infected2=matrix(NA, nrow=1000, ncol=52*years_simulate)  # Total alive infected 2 per day
Fams_Exposed=
Fams_Infected1=
Fams_Infected2=
Fams_mange=
Fams_extincted=
Adult_sus= # id of susceptible adults per day per family
Adult_exp= # id of exposed adults per day per family
Adult_i1= # id of infectious1 adults per day per family
Adult_i2= # id of infectious2 adults per day per family
Subadult_sus= # id of susceptible subadults per day per family
Subadult_exp= # id of exposed subadults per day per family
Subadult_i1= # id of infectious1 subadults per day per family
Subadult_i2= # id of infectious2 subadults per day per family
Pup_sus= # id of susceptible pup per day per family
Pup_exp= # id of exposed pup per day per family
Pup_i1= # id of infectious1 pup per day per family
Pup_i2= # id of infectious2 pup per day per family
Dead_adults_mange= # id of adults dead becuase of mange
Dead_subadults_mange= # id of adults dead becuase of mange
Dead_pups_mange= # id of adults dead becuase of mange
Dens_susc=  # id of susceptible dens
Dens_inf=lapply(vector('list', 1000), function(x){vector('list', 52*years_simulate)}) # id of infested dens
#Probs=matrix(NA, nrow = 5, ncol=1000)

# Character vector with the names of the objects to save as output
to.save=c('N', 'Adults', 'Adults_male', 'Adults_fem', 'Subadults', 'Subadults_male','Subadults_fem', 'Disperse', 'Pups', 'Exposed', 'Infected1', 'Infected2', 'Fams_Exposed',
          'Fams_Infected1', 'Fams_Infected2', 'Fams_mange', 'Fams_extincted', 'Adult_sus', 'Adult_exp', 'Adult_i1', 'Adult_i2', 'Subadult_sus', 'Subadult_exp', 'Subadult_i1', 'Subadult_i2',
          'Pup_sus', 'Pup_exp', 'Pup_i1', 'Pup_i2', 'Dead_adults_mange', 'Dead_subadults_mange', 'Dead_pups_mange', 'Dens_susc', 'Dens_inf')

