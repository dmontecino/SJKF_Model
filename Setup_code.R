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
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
    if(winDialog(type = c("yesno"), libsmsg)=="YES"){       
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
}


# --- LOAD PACKAGES NEEDED --- #
# if packages needed are not installed, it will ask for permission to install them.

using("sp","rgeos","pedometrics","partitions", "gtools")

###########################
# --- LOAD DATA NEEDED ---#
###########################

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


###########################################################
### ---- TIME UNITS AND TIME PERIODS IN THE MODEL ---- ####
###########################################################

# definition of the weeksof the year contained in each month of the model's standard year
weeks.by.month<-list(c(1:4), c(5:8), c(9:13), c(14:17), c(18:22), c(23:26), c(27:30), c(31:35), c(36:39), c(40:43), c(44:48), c(49:52))  

# weeks of the year when dispersal juveniles can star dispersing. May to September.
dispersal.t=c(19:39) 

# weeks of the year when breeding and cheat can occur.
cheat.t<-c(48:52,1:2)

#############################
### --- FIXED OBJECTS --- ###
#############################

# total number of familes of foxes and also home ranges.
n_fam=59


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



