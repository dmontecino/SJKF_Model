Hi everyone. This README contains the information needed to run the model constructed to assess the role of dens in the spread, 
establishment, and persistence of sarcoptic mange in the San Joaquin kit fox population, located in Bakersfield, California. This model was published in the journal "Epidemics" with the title: "Assessing the role of dens in the spread, establishment, and persistence of sarcoptic mange in an endangered canid", by Diego Montecino-Latorre et al., (2019). 

This README will not go through the details of the model. For that check the article.

## DO NOT MOVE TO THE INSTRUCTION SECTION YET!! READ THE FOLLOWING

The model is divided in 6 scripts I thought this will make it easier to understand it. These are:

* The setup of the model (https://github.com/dmontecino/SJKF_Model/blob/master/Setup_model_script.R)

* The den changing and sharing process (https://github.com/dmontecino/SJKF_Model/blob/master/Den_changing_and_sharing_script.R)

* The disease transmission process (https://github.com/dmontecino/SJKF_Model/blob/master/Disease_transmission_script.R)

* Natural history processes of the species in Bakersfield. This is the main chunk of code that calls the 2 previous scripts and the following one (https://github.com/dmontecino/SJKF_Model/blob/master/Natural_history_script.R)

* The data collection on a weekly basis (https://github.com/dmontecino/SJKF_Model/blob/master/Data_collection_script.R)

* The actual model function to set your parameters of interest such as the number of iterations, the number of years to simulate, if you want to run an epidemic or endemic scenario, the transmission parameters for den associated transmission pathways, and the number of week S. scabiei can survive off-host within den per type of land type and season. This script actually runs the model (https://github.com/dmontecino/SJKF_Model/blob/master/Model_function.R).


## HOW DOES IT WORK?

The ONLY script you need to download to run the model is the one containing the function: Model_function_script.R

All packages needed will be installed automatically if not installed. You will be requiered to approve the installation. the Installed packages as well as those that were already installed in your computer will be loaded automatically.

All files needed to run the model, such as the spatial objects containing the home ranges and others will be automatically downloaded as 
a temp file and the objects will be loaded in your R session. The only exception is explained below (The largest file needs to be downloaded by the user).

The model function will run the other scripts automatically from their current Github location. To actually read these scripts, check the links above. Or you can navigate the project folder (you are already on it).

The model works by modifying 2 datasets every week: the fox dataset and the den dataset. The "fox" object contains data about (guess what) the foxes, and obviously the second one, the "den.data.set" object, contains information about the dens. These 2 datasets are connected as the foxes occupy dens, but the infestation status of the dens is shown is the dens dataset. You will not see these datasets if you do not want to, except at the end of the simulation when the dataset for the last week should be available in your environment.

The fox dataset contains the following columns:

* Fam_id = the id of the family the fox belongs to. When they move to another family group this value changes.
* Fox_id = the fox id. This is unique for each fox even after they die.
* Father_id = the id of the father of the fox. To avoid them to compete, use the same den if they should not and mate (if female). 
* Mother_id = the id of the mother of the fox. To avoid them to compete, use the same den if they should not and mate (if male).
* Gender = male =1 , female=0.
* Social = 1,2,3, pups, subdominants and dominants, respectively.
* Age = number of years. 
* Alive = 1 for alive, 0 otherwise. The foxes dying during the week  ones are cleaned from the dataset.
* Disp = 0, 1, 2 : subdominants that will not disperse, subdominants that will disperse, and subdominants dispersing, respectively.  0 does not become 2 but 1 does become 2 once the fox starts dispersing.
* S = susceptibility status to sarcoptic mange (1, if susceptible 0 otherwise). See details in the journal article.
* E = exposed status to sarcoptic mange (1, if exposed 0 otherwise). See details in the journal article.
* I.1 = infectious type 1 status of the fox (1, if infectious type 1, 0 otherwise). See details in the journal article.
* I.2 = infectious type 2 status of the fox (1, if infectious type 2, 0 otherwise). See details in the journal article. 
* Den_id = The den that the fox is currently using.

The den dataset contains the following columns:

* Den_id: as previously explained.
* Land_type: the land type in which the den is located in (See details in the journal article). 
* I: the sarcoptes scabiei infestation status of the den (1 if infested, 0 otherwise).

## INSTRUCTIONS ##

Do not run this model in a computer with less than 8 MB RAM. I built it in a 16 MB RAM computer and everything worked just fine including functionalities beyond running the model.

Download the file containing the home ranges from this link: https://drive.google.com/uc?export=download&id=0B_DpR28UsQR7UnhPbXU1cW4zRkU
This is the only file that does not download automatically. I do not why but my guess it is due the size of the file.

Download the script Model_function_script.R from this link https://github.com/dmontecino/SJKF_Model/blob/master/Model_function.R 

Open the Model_function_script.R in R

Modify the path in line 81 of the Model_function_script.R to load the object containing the home ranges. If you are not planning on running the 1000 iterations available, subset this list to the number you want to.

Set the values of the parameters as you which. The explanation of each parameter is in the script.

Once you are satified with the values of the parameters of interest run the script.

## OUTPUT

The output of the model comes in the object "output" which contains matrices and lists per object. The matrices rows equal the number of iterations (max 1,000) and the number of columns equal to the total number of years simulated multiplied by the number of weeks per year simulated (always 52). These matrices are:

* N: the number of alive foxes
* Adults: The number of dominant foxes
* Adults_male: the number of dominant males
* Adults_fem: the number of dominant females
* Subadults: the number of subdominants
* Disperse: the number of dispersers
* Subadults_male: the number of male subdominants
* Subadults_fem: the number of female subdominants
* Pups: the number of pups
* Exposed: the number of exposed foxes
* Infected1: the number of infectious type I foxes
* Infected2: the number of infectious type II foxes

I the case of the lists, they contain the Family, Fox or den id per week of the different items. The strucure of these lists has 2 levels: the highest one is the number of iterations. The lower one is the current week with the corresponding id's
There is an option to supress this output. See "Instructions".

* Fams_Exposed: Families with at least one member exposed
* Fams_Infected1: Families with at least one member infectious type I
* Fams_Infected2: Families with at least one member infectious type II
* Fams_mange: Families with mange
* Adult_sus: Susceptible dominants
* Adult_exp: Exposed dominants
* Adult_i1: Infectious type I dominants
* Adult_i2: Infectious type II dominants
* Subadult_sus: Susceptible subdominants
* Subadult_exp: Exposed subdominants
* Subadult_i1: Infectious type I subdominants
* Subadult_i2: Infectious type II subdominants
* Pup_sus: Susceptible pups
* Pup_exp: Exposed pups
* Pup_i1: Infectious type I pups
* Pup_i2: Infectious type II pups
* Dens_susc: Uninfested dens
* Dens_inf: Infested dens

## Assess the output

The object "output" is a list that contains the matrices and lists
For example, if you want to access the San Joaquin fox population, then do: output$N 




