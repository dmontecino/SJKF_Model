##############################################################################################
# --- CODE OF THE MODEL TO ASSESS THE ROLE OF DENS IN THE SPREAD, INVASION AND PERSISTENCE---#
# --- OF SARCOPTIC MANGE IN THE SAN JOAQUIN FOX POPULATION LOCATED IN BAKERSFIELD, CA -------#
# --- SPECIFICALLY THIS SCRIPT RUNS THE MODEL AND PRODUCES OUTPUT IN YOUR R ENVIRONMENT -----#
##############################################################################################

# Author:Diego Montecino-Latorre
# dmontecino@ucdavis.edu
# Details of the model are published in the journal "Epidemics" January 2019


  
  ######################
  #--- load library ---#
  ######################
  
  # the RCurl is not installed, installed and load it, otherwise just load it
  if (!require('RCurl')) install.packages('RCurl'); library('RCurl') # to run script from github
  
  #just dealing with a bug
  closeAllConnections() # this is needed to prevent a bug when calling objects through the url function
  
  ################################
  # Define paramters of interest #
  ################################
  
  epidemic=TRUE # Do you want to model an epidemic or an endemic scenario
  num.iterations=2 # number of iterations per scenario
  years_simulate=2 # number of year each scenario is simulated
  show.year.finished = TRUE # do you want to print the week just finished
  show.week.of.the.year.finished = TRUE # do you want to print the year just finished
  show.iteration.finished = TRUE # do you want to print the iteration just finished
  
  # Disease parameters
  
  # pSocial in Table 2.
  prob.trans.foxes.contact.fam.min = 0.15 # the minimum value of the weekly trasnmission probability of sarcoptic mange due to social contact among foxes of the same family
  prob.trans.foxes.contact.fam.max = 0.3 # the maximum value of the weekly trasnmission probability of sarcoptic mange due to social contact among foxes of the same family
  
  # pMate in Table 2.
  prob.trans.foxes.mating.min = 0.9 # the minimum value of the weekly trasnmission probability of sarcoptic mange to the mating pair
  prob.trans.foxes.mating.max = 1 # the maximum value of the weekly trasnmission probability of sarcoptic mange to the mating pair
  
 # pNurse in Table 2.
  prob.trans.pups.nursing.inf.female.min = 0.95 # the minimum value of the weekly trasnmission probability of sarcoptic mange to nursing pups from the lactating mother (dominant female)
  prob.trans.pups.nursing.inf.female.max = 1 # the maximum value of the weekly trasnmission probability of sarcoptic mange to nursing pups from the lactating mother (dominant female)
  
  #pShare in Table 2.
  prob.trans.foxes.share.den.min = 0.4 # the minimum value of the weekly trasnmission probability of sarcoptic mange due to foxes sharing a den
  prob.trans.foxes.share.den.max = 0.7 # the maximum value of the weekly trasnmission probability of sarcoptic mange due to foxes sharing a den
  
  #pDenfox in Table 2.
  prob.trans.den.to.foxes.min = 0.3 # the minimum value of the weekly trasnmission probability of sarcoptic mange from an infested den to a susceptible fix using it
  prob.trans.den.to.foxes.max = 0.6 # the maximum value of the weekly trasnmission probability of sarcoptic mange from an infested den to a susceptible fix using it
  
  # Weeks survival Sarcoptes scabiei off-host within den
  # Modify them to create the different sub experiments
  # for details about these parameters see section 2.4 of the manuscript
  weeks.s.scabiei.surv.cold.open = 1 # number of weeks S. scabiei can survive off-host within dens located in ‘Open’, ‘Transition’, ‘Industrial’, and ‘Linear’ land types during the cold season
  weeks.s.scabiei.surv.cold.residential = 3 # number of weeks S. scabiei can survive off-host within dens located in ‘Residential’, ‘Commercial’, and ‘Manicured’ land types during the cold season
  weeks.s.scabiei.surv.cold.sump = 2 # number of weeks S. scabiei can survive off-host within dens located in "Water catchment basins" during the cold season
  weeks.s.scabiei.surv.warm.open = 0.1 # number of weeks S. scabiei can survive off-host within dens located in ‘Open’, ‘Transition’, ‘Industrial’, and ‘Linear’ land types during the warm season
  weeks.s.scabiei.surv.warm.residential = 3 # number of weeks S. scabiei can survive off-host within dens located in ‘Residential’, ‘Commercial’, and ‘Manicured’ land types during the warm season
  weeks.s.scabiei.surv.warm.sump = 1 # number of weeks S. scabiei can survive off-host within dens located in "Water catchment basins" during the warm season

  #################
  # RUN THE MODEL #
  #################
  
  
  # Script to set up the model
  eval(parse(text = getURL("https://raw.githubusercontent.com/dmontecino/SJKF_Model/master/Setup_model_script.R", ssl.verifypeer = FALSE)))
  # This code loads the data needed to run the model such as the home ranges per iteration,
  # and the dens per iteration. These files are directly loaded as temp files from google drive
  # The code also loads the parameters not associates with disease transmission but with the naturla history of the san joaquin kit fox
  # It also creates objects to save the output from the model as well as objects with time periods
  
  # load the home ranges #
  # modify the "your_path/all_home_ranges_per_iteration.RDS" to the location of the all_home_ranges_per_iteration.RDS in your computer
  # go to the README if you do not know where to download this file from.
  # all_home_ranges=readRDS("/Your_path/all_home_ranges_per_iteration.RDS")
  
  # The model script
  eval(parse(text = getURL("https://raw.githubusercontent.com/dmontecino/SJKF_Model/master/Natural_history_script.R", ssl.verifypeer = FALSE)))
  
  # the object output contains the data created by the model
  
  
  ##----------------------------------##
  
 
  
  # Asess the output (e.g., the number of infested foxes) 
  
  matplot(t(output$N), type="l", ylim=c(0,400))


