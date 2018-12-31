Hi everyone. This README contains the information needed to run the model constructed to assess the role of dens in the spread, 
establishment, and persistence of sarcoptic mange in the San Joaquin kit fox population, located in Bakersfield, California. This model was published in the journal "Epidemics" with the title: "Assessing the role of dens in the spread, establishment, and persistence of sarcoptic mange in an endangered canid", by Diego Montecino-Latorre et al., (2019). 

This README will not go through the details of the model. For that check the article.

## DO NOT MOVE TO THE INSTRUCTION SECTION YET!! READ THE FOLLOWING

The model is divided in 6 scripts I thought this will make it easier to understand it. These are:

* The setup of the model (https://github.com/dmontecino/SJKF_Model/blob/master/Setup_model_script.R)

* The den changing and sharing process (https://github.com/dmontecino/SJKF_Model/blob/master/Den_changing_and_sharing_script.R)

* The disease transmission process.

* Other natural history processes of the species in Bakersfield.

* Data collection.

* The actual model function to set your parameters of interest such as the number of iterations, the number of years to simulate, if you want to run an epidemic or endemic scenario, and the transmission parameters for den associated transmission pathways. This script actually runs the model.


## HOW DOES IT WORK?

The ONLY script you need to download to run the model is the one containing the function: 
located at ...

All packages needed will be installed automatically if not installed. You will be requiered to approve the installation. the Installed packages as well as those that were already installed in your computer will be loaded automatically.

All files needed to run the model, such as the spatial objects containing the home ranges and others will be automatically downloaded as 
a temp file and the objects will be loaded in your R session. The only exception is explained below (The largest file needs to be downloaded by the user).

The model function will run the other scripts automatically from their current Github location. To actually read these scripts, go to the model function script and check the links. Or you can navigate the project folder (you are already on it).

The model works by modifying 2 datasets every week: the fox dataset and the den dataset. The "fox" object contains data about (guess what) the foxes, and obviously the second one, the "den.data.set" object, contains information about the dens. These 2 datasets are connected as the foxes occupy dens, but the infestation status of the dens is shown is the dens dataset. You will not see these datasets if you do not want to, except at the end of the simulation when the dataset for the last week should be available in your environment.

The fox dataset contains the following columns:

*Fam_id = the id of the family the fox belongs to. When they move to another family group this value changes.

*Fox_id = the fox id. This is unique for each fox even after they die.

*Father_id = the id of the father of the fox. To avoid them to compete, use the same den if they should not and mate (if female). 

*Mother_id = the id of the mother of the fox. To avoid them to compete, use the same den if they should not and mate (if male).

*Gender = male =1 , female=0.

*Social = 1,2,3, pups, subdominants and dominants, respectively.

*Age = number of years. 

*Alive = 1 for alive, 0 otherwise. The foxes dying during the week  ones are cleaned from the dataset.

*Disp = 0, 1, 2 : subdominants that will not disperse, subdominants that will disperse, and subdominants dispersing, respectively.  0 does not become 2 but 1 does become 2 once the fox starts dispersing.

*S = susceptibility status to sarcoptic mange (1, if susceptible 0 otherwise). See details in the journal article.

*E = exposed status to sarcoptic mange (1, if exposed 0 otherwise). See details in the journal article.

*I.1 = infectious type 1 status of the fox (1, if infectious type 1, 0 otherwise). See details in the journal article.

*I.2 = infectious type 2 status of the fox (1, if infectious type 2, 0 otherwise). See details in the journal article. 

*Den_id = The den that the fox is currently using.

The den dataset contains the following columns:

*Den_id: as previously explained.

*Land_type: the land type in which the den is located in (See details in the journal article). 

*I: the sarcoptes scabiei infestation status of the den (1 if infested, 0 otherwise).

## INSTRUCTIONS ##

Do not run this model in a computer with less than 8 Mb RAM. I built it in a 16 MB RAM computer and everything worked just fine including functionalities beyond running the model.

Download the file containing the home ranges from this link: https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7UnhPbXU1cW4zRkU

Download the model function script from this link and open it in R.

Modify line .. of the model function script to load the object containing the home ranges. Of you are not planning on running the 1000 iterations available, subset this list to the number you want to.







