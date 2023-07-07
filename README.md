# Sphingid caterpillars conspicuous patches do not function as distractive marks or warning against predators 
Here is the R code and associated data files for the manuscript, <i>Sphingid caterpillars contrast markings do not function as distractive marks against predators 
</i>, that has been accepted for publication by Ecology and Evolution (July 2023).

There's a total of 12 data files and 4 R code files included in this repository.

<b>Data files</b>
<br>
Three sites were used in the study: 
<br>
Breakheart Reservation (abb. BH) 
<br>
Great Brook Farm State Park (abb. GB)
<br>
Bradley Palmer State Park (abb. BP)

Most data files will note Site, Cluster Number, Color,	Pattern, Predation,	Predator,	Number Bite Marks,	Location Bite Marks,	Days from Start,	Month,	Model ID,	Mean,	Std,	Se, n, Attacks, and AttackRate.


Site - location where models were placed (BP, BH or GB)
<br>Cluster Number - location where models were placed within each site (1-14). BH was assigned as site 1, GB was site 2 and BP was site 3. For example, cluster number 3.14 represents BP cluster 14.
<br>Color - color of artificial model (red or green)
<br>Pattern - pattern of artificial model (patterned or solid)
<br>Predation - was the model predated (yes=1, no=0)
<br>Predator - type of predator (arthropod, bird, rodent, mammal, unknown, none)
<br>Number Bite Marks - number of complete bite marks that could be counted(ex. upper and lower beak mark counted as 1)
<br>Location Bite Marks - location of bite mark on the model (middle, end, middle & end, none) 
<br>Days from Start - number of days from the start of the study when models were collected (7,25,42,58,78,92)
<br>Month - month that models were deployed at the sites (late July, early August, late August, early September, late September, mid-October)
<br>Model ID - unique number each model was given (1-1008)
<br>Mean - mean
<br>Std - standard deviation
<br>Se - standard error
<br>n - sample size 
<br>Attacks - number of attacks 
<br>AttackRate - calculated weekly attack rates for all predator taxa for each site per experimental trial and in total for each trial (total attacked/total number of recovered caterpillars per site or trial) 


<b>R Code</b>
<br>
JBAttackRateExploration - contains code calculating attack rates of different predators and visualizing by month and site
<br>
JB AttackRatesByTreatment - contains code calculating attack rates of different predators and visualizing by color, pattern and treatment(color and pattern together)
<br>
JBGlm(er)Code - contains code of glm and glmer models of different predator taxa with and without duplicate predators included for comparison. also contains models with site included as a fixed effect to accompany the figures and discussion found in the supplementary materials
<br>
JBiNatData - contains code for visualization of larvae color data from inaturalist and the multinomial logistic regression
<br>
When files are read in the r code, the csv data files must be in the same folder


Please contact Julia Barrone if you have further questions - julia.barrone001@umb.edu
