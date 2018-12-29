############################################################################
#################### DEN CONDITIONS PREVIOUS STUDIES #######################
############################################################################

### Girard 2001 
##  Average temperatures in unoccupied dens were 11.5  2.93C in April, 22.0  2.13 in august and
# 13.0  1.79C in January. This is for kit fox, Mojave dessert california. not clear if soil or air tmemperatures

# Humidity in kit fox dens is 38% (at 15.5 C soil temperature and 18-22C the air temperature
# inside the den with fox inside. 
# it would be higher in summer; Golightly 1981 )
#this was in winter

# the temp of aarwolf burrows at 2.5 m into the burrow was relatively constant during summer
# and averages 27.2 plus minus 0.2 despiste large daily flucjtuations at the soil surface 
# where temperatures ranged from a maximum of 39.5 to 15 C. 
# During Winter mena burrow temperatures were also constant, averaging 12.2 plus minus 1.2 
# substatially lower than that recorded in summer; but it is still considerably warmer 
# tha outside the den which averaged 7.6 plus minus 6.4 (anderson & richardson 2005)

#Low temperature (10-15 ~ ) and high r.h. prolonged survival of all life stages. 
#At 10-15~ females and nymphs survived 1-3 weeks at 97% r.h., 1-2 weeks at 75% r.h. 
#and 5-8 days at 45% r.h. At 20-25 ~ C, survival was significantly reduced but all 
#life-stages survived at least 2 days at 25% r.h. and 5-6 days at 75-100% r.h. Arlian 1989 

# Table I. Arlian 1984

##########################
## Load Packages needed ##
##########################

library(sp)
library(rgeos)
library(pedometrics)
library(partitions)
#library(epiR)
library(gtools)

###################
#load files needed#
###################

# all_home_ranges=readRDS('/data/home/diego/SJKF/Model_Objects/all_home_ranges_per_iteration.RDS')
# all_dens_avail_by_hr_by_it=c=readRDS('/data/home/diego/SJKF/Model_Objects/all_dens_avail_by_hr_by_it_final_complete.rds') # all dens in each home range including those not belonging to the family.
# hr_nb_1480=readRDS('/data/home/diego/SJKF/Model_Objects/neighbor_home_ranges_1480.RDS')
# hr_nb_more_1480_less_2960=readRDS('/data/home/diego/SJKF/Model_Objects/neighbor_home_ranges_btn_1480_and_2960.RDS')
# weight.distance.to.home.ranges=readRDS('/data/home/diego/SJKF/Model_Objects/distances_among_home_ranges_centroids_complete.RDS')
# weight.distance.to.home.ranges.backwards=readRDS('/data/home/diego/SJKF/Model_Objects/distances_among_home_ranges_centroids_complete.RDS')

all_home_ranges=readRDS('C:/Users/monton/Desktop/SJKF_Model/Model_objects/all_home_ranges_per_iteration.RDS')
all_dens_avail_by_hr_by_it=c=readRDS('C:/Users/monton/Desktop/SJKF_Model/Model_objects/all_dens_avail_by_hr_by_it_final_complete.rds')
hr_nb_1480=readRDS('C:/Users/monton/Desktop/SJKF_Model/Model_objects/neighbor_home_ranges_1480.RDS')
hr_nb_more_1480_less_2960=readRDS('C:/Users/monton/Desktop/SJKF_Model/Model_objects/neighbor_home_ranges_btn_1480_and_2960.RDS')
weight.distance.to.home.ranges=readRDS('C:/Users/monton/Desktop/SJKF_Model/Model_objects/distances_among_home_ranges_centroids_complete.RDS')
weight.distance.to.home.ranges.backwards=readRDS('C:/Users/monton/Desktop/SJKF_Model/Model_objects/distances_among_home_ranges_centroids_complete_backwards.RDS')

# #spatial box to sample home ranges where to allocate the first sick individual
 box<-SpatialPointsDataFrame(data.frame(x=c(82742.43, 87783.89), y=c(-283617.6, -289991.8)), data = data.frame(Id=c(1,2)), proj4string = CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
 box<-bbox2sp(obj = box, sp = 'SpatialPolygonsDataFrame', keep.crs = T)

##############################################
############ INPUTS FOR THE MODEL ############
##############################################

num.iterations=1000

years.run=0 # the number of years so far in the simulation. At the beggining of course is zero years run.  

years_simulate=8 #number of years to simulate

sharing<- 'Yes' 

track.disease=T # F works

##########################
## Objects to save data ##
##########################

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
  Probs=matrix(NA, nrow = 5, ncol=1000)
  
to.save=c('N', 'Adults', 'Adults_male', 'Adults_fem', 'Subadults', 'Subadults_male','Subadults_fem', 'Disperse', 'Pups', 'Exposed', 'Infected1', 'Infected2', 'Fams_Exposed',
          'Fams_Infected1', 'Fams_Infected2', 'Fams_mange', 'Fams_extincted', 'Adult_sus', 'Adult_exp', 'Adult_i1', 'Adult_i2', 'Subadult_sus', 'Subadult_exp', 'Subadult_i1', 'Subadult_i2',
          'Pup_sus', 'Pup_exp', 'Pup_i1', 'Pup_i2', 'Dead_adults_mange', 'Dead_subadults_mange', 'Dead_pups_mange', 'Dens_susc', 'Dens_inf')

###################
### TIME UNITS ####
###################

days.per.month=list(c(1:31), c(32:59)) 
weeks.by.month<-list(c(1:4), c(5:8), c(9:13), c(14:17), c(18:22), c(23:26), c(27:30), c(31:35), c(36:39), c(40:43), c(44:48), c(49:52))  

breed.t=c(48:52,1:5) # weeks when it is breeding period
rearing.t=c(6:18) # weeks when it is pup rearing period
other.t=c(19:47)
dispersal.t=c(19:39) # May first to September 30
mate.t<-c(c(1:46),c(335:365))
day.year.per.week<-split(c(1:365), ceiling(seq_along(c(1:365))/7))
day.year.per.week<-day.year.per.week[!(day.year.per.week%in%day.year.per.week[[53]])]
day.year.per.week[[52]]<-c(day.year.per.week[[52]], 365) #days of the year per week. only the last week has one more day than it should (8 vs 7)  
mate.t<-unname(which(sapply(lapply(day.year.per.week, function(x){x%in%mate.t}), function(y){any(y)})==T))
cheat.t<-c(48:52,1:2)

################################
### PROBABILITIES OF EVENTS ####
################################

#prob.change den in breed.t and rearing
prob.den.change.breed.t.rearing.t=1-exp(-6/22)
prob.den.change.breed.t.rearing.t.week.1.18=1-exp(-5.72/18) #numbers of dens used breed.t and rearing.t. These are a single prob becuase Koopman 1998 
#says there are not statistically difference between the number of dens used in both seasons.
# it is 7 dens total but in discounting the previous 4 weeks of this season (the last 4 weeks of th previous year) the expected is 5.72 dens in these 18 weeks.

prob.den.change.breed.t.48.52=1-exp(-0.59/4) # here the expected in the last 5 weeks of the year is 1.59 dens but in the very first week all are changed to a new den to 
# fulfill the period rules, so it is one less den and one less week

prob.den.change.other=1-exp(-13/28) #similarly

#ad.daily.surv.prob= exp(-((1-0.78)/(365/7))) #0.9994 #probability of adult daily survival. From Frost and Cypher 0.9 annual survivla probability (this is for 9 months) in Bakerfield and 0.7. WE try with 0.8 annual survivial prob.
ad.anual.mort.prob=1-0.78
ad.anual.mort.rate=-log(1-ad.anual.mort.prob)
ad.weekly.mort.prob=1-exp(-ad.anual.mort.rate/52)
ad.weekly.surv.prob=ad.daily.surv.prob=1-ad.weekly.mort.prob #name mantained to avoid changing label in each code

#sa.fem.daily.surv.prob=exp(-((1-0.54)/(304/7))) #values around annual prob survival of 0.2 - 0.4. Cypher mentions: a little higher. 
sa.fem.anual.mort.prob=1-0.535
sa.fem.anual.mort.rate=-log(1-sa.fem.anual.mort.prob)
sa.fem.weekly.mort.prob=1-exp(-sa.fem.anual.mort.rate/43) # 43 from 304/7
sa.fem.weekly.surv.prob=sa.fem.daily.surv.prob=1-sa.fem.weekly.mort.prob #name mantained to avoid changing label in each code
sa.fem.weekly.surv.prob=sa.fem.daily.surv.prob=0.994

#sa.male.daily.surv.prob=exp(-((1-0.61)/(104/7))) # divided by 7 to get the weeks
sa.male.anual.mort.prob=1-0.62
sa.male.anual.mort.rate=-log(1-sa.male.anual.mort.prob)
sa.male.weekly.mort.prob=1-exp(-sa.male.anual.mort.rate/15) # 15 from 104/7
sa.male.weekly.surv.prob=sa.male.daily.surv.prob=1-sa.male.weekly.mort.prob #name mantained to avoid changing label in each code
sa.male.weekly.surv.prob=sa.male.daily.surv.prob=0.988

#pup.daily.surv.prob=exp(-((1-0.8)/(104/7)))# probability of daily survival for pups. Data available is from red foxes: 0.8 survival. from mid febraury to May 31st: 105 days
pup.anual.mort.prob=1-0.8
pup.anual.mort.rate=-log(1-pup.anual.mort.prob)
pup.weekly.mort.prob=1-exp(-pup.anual.mort.rate/15) # 15 from 104/7
pup.weekly.surv.prob=pup.daily.surv.prob=1-pup.weekly.mort.prob #name mantained to avoid changing label in each code

rep.rate=0.788 # reproductive rate.

### DISPERSERS ####

sa.disp.mort.prob.10.days=0.65
sa.disp.mort.rate.10.days=-log(1-sa.disp.mort.prob.10.days)
sa.disp.weekly.mort.prob=1-exp(-sa.disp.mort.rate.10.days/(10/7)) # 15 from 104/7
sa.disp.weekly.surv.prob=1-sa.disp.weekly.mort.prob #name mantained to avoid changing label in each code
#prob.to.start.dispersing=0.1
disp.sa.fem.daily.surv.prob= 0.6#sa.disp.weekly.surv.prob
disp.sa.male.daily.surv.prob=0.6 #sa.disp.weekly.surv.prob

prob.to.start.dispersing.male=0.35 #0.5
prob.to.start.dispersing.female=0.1 #0.23

## When would they start to disperse
disp.prob.dist=read.csv('C:/Users/monton/Desktop/SJKF_Model/Model_objects//Dispersal.juveniles.koopman.2000.csv') #90% of dispersers ar younger than one year if considering koopman et al 2000 (81%) and Scrivner (95% (46/48)). We will not consider older than 1 year to disperse.
disp.prob.dist=disp.prob.dist[disp.prob.dist$Month%in%c('May', 'June', 'July', 'August', 'September'),]
disp.prob.dist$Dispersal=disp.prob.dist$Dispersal/sum(disp.prob.dist$Dispersal)

##########################
###### FIXED OBJECTS #####
##########################

n_fam=59 # total number of familes of foxes and also home ranges.
pups.by.fam.init=data.frame(N=c(1,2,3,4,5,6), Prob=c(0.095,0.238,0.429,0.095,0.095,0.048)) # distribution on the number of pups per family (SPencer 1992)
#adults.by.fam.init=data.frame(N=c(2,3,4,5,6), Prob=c(0.2,0.3,0.3,0.1,0.1)) #distribution number of adults b family and their probability. #Number of adults by family from expert opinion (Dr. Cypher). Dominants and subdominants foxes are obtained from the adults.
adults.by.fam.init=data.frame(N=c(2,3,4,5,6), Prob=c(0.1,0.1,0.3,0.3,0.2)) #distribution number of adults b family and their probability. #Number of adults by family from expert opinion (Dr. Cypher). Dominants and subdominants foxes are obtained from the adults.
weeks.adult.male.position.is.refill.ater.death<-c(2:7)
weeks.adult.female.position.is.refill.ater.death<-c(2:7)

################################
###### DISEASE PARAMETERS ######
################################

# Probability of transmission from infected family members
#week.prob.fam.trans=0.5
#prob.trans.foxes.contact.fam=0.35 # this is the model for parameterizing the beta distribution
#beta.dist.prob.trans.foxes.contact.fam=epi.betabuster(mode=prob.trans.foxes.contact.fam, conf = .90, greaterthan = F, 0.6)
#prob.trans.foxes.contact.fam=rbeta(n = 1000, shape1 = beta.dist.prob.trans.foxes.contact.fam$shape1, shape2=beta.dist.prob.trans.foxes.contact.fam$shape2)
#hist(rbeta(n = 1000, shape1 = beta.dist.prob.trans.foxes.contact.fam$shape1, shape2=beta.dist.prob.trans.foxes.contact.fam$shape2))
#rate.of.weekly.contact.each.fox.in.family=3/7
#prob.trans.when.in.ad.contact=runif(n = 1000, min = 0.5, max = 0.8)
#rate.of.trans.each.fox.in.fam=rate.of.weekly.contact.each.fox.in.family*prob.trans.when.in.ad.contact
prob.trans.foxes.contact.fam=runif(min = 0.15, max = 0.3, n = 1000)
#prob.trans.foxes.contact.fam=rep(0.25, 1000)


# Probability of transmission from infected dispersers falling in another family 
prob.trans.dispersers.contact.foxes.other.fam=rep(0, 1000) #rep(0.05, 1000)

# Probability of transmission from foxes while sharing dens (this includes also that the dens are infested)
#week.prob.share.den.trans=0
#prob.trans.foxes.share.den=0.7
#beta.dist.prob.trans.foxes.share.den=epi.betabuster(mode=prob.trans.foxes.share.den, conf = .95, greaterthan = T, 0.5)
#prob.trans.foxes.share.den=rbeta(n = 1000, shape1 = beta.dist.prob.trans.foxes.share.den$shape1, shape2=beta.dist.prob.trans.foxes.share.den$shape2)
#hist(rbeta(n = 10000, shape1 = beta.dist.prob.trans.foxes.share.den$shape1, shape2=beta.dist.prob.trans.foxes.share.den$shape2))
#prob.trans.foxes.share.den=rep(0, 1000)
#prob.trans.foxes.share.den=runif(n = 1000, min = 0.4, max = 0.75)

#Probability of trasmission from infested dens to susceptible foxes
#week.prob.den.trans=0
#prob.trans.den.to.foxes=0.4
#beta.dist.prob.trans.den.to.foxes=epi.betabuster(mode=prob.trans.den.to.foxes, conf = .95, greaterthan = F, 0.5)
#prob.trans.den.to.foxes=rbeta(n = 1000, shape1 = beta.dist.prob.trans.den.to.foxes$shape1, shape2=beta.dist.prob.trans.den.to.foxes$shape2)
#hist(rbeta(n = 10000, shape1 = beta.dist.prob.trans.den.to.foxes$shape1, shape2=beta.dist.prob.trans.den.to.foxes$shape2))
#prob.trans.den.to.foxes=runif(n = 1000, min = 0.2, max = 0.5)
#prob.trans.den.to.foxes=rep(0,1000)

# Probability of transmission through mating
# prob.trans.foxes.mating=1
# beta.dist.prob.trans.foxes.mating=epi.betabuster(mode=prob.trans.foxes.mating, conf = .95, greaterthan = T, 0.9)
# prob.trans.foxes.mating=rbeta(n = 1000, shape1 = beta.dist.prob.trans.foxes.mating$shape1, shape2=beta.dist.prob.trans.foxes.mating$shape2)
# hist(rbeta(n = 10000, shape1 = beta.dist.prob.trans.foxes.mating$shape1, shape2=beta.dist.prob.trans.foxes.mating$shape2))
prob.trans.foxes.mating=runif(n = 1000, min = 1, max = 1)
#prob.trans.foxes.mating=rep(0, 1000)

# Probability of transmission through nursing
# prob.trans.pups.nursing.inf.female=1
# beta.dist.prob.trans.pups.nursing.inf.female=epi.betabuster(mode=prob.trans.pups.nursing.inf.female, conf = .95, greaterthan = T, 0.95)
# prob.trans.pups.nursing.inf.female=rbeta(n = 1000, shape1 = beta.dist.prob.trans.pups.nursing.inf.female$shape1, shape2=beta.dist.prob.trans.pups.nursing.inf.female$shape2)
# hist(rbeta(n = 10000, shape1 = beta.dist.prob.trans.pups.nursing.inf.female$shape1, shape2=beta.dist.prob.trans.pups.nursing.inf.female$shape2))
prob.trans.pups.nursing.inf.female=runif(n = 1000, min = 1, max = 1)
#prob.trans.pups.nursing.inf.female=rep(0, 1000)

# Probability survival foxes in I.2
fox.with.mange.I.2.weekly.mort.rate= 1/(21/7) #28 days expected to survive
fox.with.mange.I.2.weekly.mort.prob= 1-exp(-(fox.with.mange.I.2.weekly.mort.rate)) #25 days expected to die by as I.2 
fox.with.mange.I.2.weekly.surv.prob= 1-fox.with.mange.I.2.weekly.mort.prob 
#fox.with.mange.I.2.weekly.surv.prob=0

#Citation??
# 70 days after infection to death Mörner and Christensson 1984 
# 90 days after infection to death Mörner and Christensson 1984
# death 120 days Bornstein 1995
# 100 days Newman 2002

#Probability to move from E status to I.1
#prob.to.become.I.1=1-exp(-(1/(14/7))) #14 days expected Arlian and D. L. Vyszenski-Moher (to be sure 2 weeks. could be one)

#Probability to move from I.1 status to I.2
#prob.to.become.I.2=1-exp(-(1/(21/7))) #22 days expected as I1 Citation??
# 50 days later hyperkertaosis (bornstei 1995)
# 2 months Stone 1972
# 50 days Mörner and Christensson
# 40 days Stone (1972) 60 days in red foxes
# 50 days Samuel 1981
# 60 days Mörner and Christensson
# 50 - 60 days Bornstein 1995
# 40 days Bornstein 1991
# 50 days Little 1998
