#packages needed
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)
library(afex)
library(emmeans)
library(lsmeans)


####################################################################
##Data from trials 1-6 (includes trial 1 but excludes models 
#with splotches washed off also modified to include one predator per model) 
predationtotal <- read.csv("ModifiedPredationTotal.csv")
predationtotal <- na.omit(predationtotal) #omit NAs 
predationtotal <- transform(predationtotal, Treatment=paste(Color, Pattern, sep="_"))
predationtotal$Cluster.Number <- as.factor(predationtotal$Cluster.Number)#Cluster number as a factor


#dataframe of just inverts, none & unknown 
inverts_only <- subset(predationtotal, Predator %in% c("Arthropod", "None", "Unknown"))

#dataframe of just verts, none & unknown 
verts_only <- subset(predationtotal, Predator %in% c("Bird", "Mammal", "Rodent", "None", "Unknown"))

#dataframe of just rodents 
rodents_only <- subset(predationtotal, Predator %in% c("Rodent", "None", "Unknown"))

#dataframe of just birds, none & unknown 
birds_only <- subset(predationtotal, Predator %in% c("Bird", "None", "Unknown"))



#####################
#LOOK AT TREATMENT AND SEE IF IT IS CORRELATED WITH LOCATION OF BITE MARKS
#####################

#Are the variables independent or is there a non-random association? 

#just june, july, aug, sept for chi squared test
chi_bites <- predationtotal %>% filter(Bite_Mark_Location == "end"|
                                         Bite_Mark_Location == "middle"|Bite_Mark_Location == "middle & end")
arth <- chi_bites %>% filter(Predator == "Arthropod")

bird <- chi_bites %>% filter(Predator == "Bird")

rod <- chi_bites %>% filter(Predator == "Rodent")

#chisquare test of independence treatment and location of bite marks 
arthbites <- table(arth$Treatment, arth$Bite_Mark_Location)

chiarth <- chisq.test(arthbites)


#chisquare test of independence treatment and location of bite marks 
birdbites <- table(bird$Treatment, bird$Bite_Mark_Location)

chibird <- chisq.test(birdbites)

#chisquare test of independence treatment and location of bite marks 
rodbites <- table(rod$Treatment, rod$Bite_Mark_Location)

chirod <- chisq.test(rodbites)

#variables independent 


###############
#GLM MODELS WITH DUPLICATE DATA ADDED TO INDIVIDUAL TAXA - ARTHROPODS, BIRDS, and RODENTS
#mammals not run separately due to small sample size
###############


#rodents only with duplicate data included
rodentsduplicate <- read.csv("Rodentwithdup.csv")
rodentsduplicate <- transform(rodentsduplicate, Treatment=paste(Color, Pattern, sep="_"))

#subset to only predation 
rodentspredonly <- subset(rodentsduplicate,Predation!="0")

#TABLE SHOWING COUNTS OF PREDATORS FOR EACH TREATMENT
roddup <- table(rodentspredonly$Treatment, rodentspredonly$Location.Bite.Marks, rodentspredonly$Predator)

#TABLE SHOWING COUNTS OF PREDATORS FOR EACH TREATMENT
roddup <- table(rodentsduplicate$Treatment, rodentsduplicate$Predator)

updatedrodent <- glmer(Predation~Color+Pattern + Month + (1|Site:Cluster.Number), 
                       family=binomial(link = "logit"), 
                       data=rodentsduplicate)
summary(updatedrodent)
#no color and pattern interaction, removed from model

#Obtain p-values for a mixed-model
mixed(updatedrodent, rodentsduplicate, method = 'LRT')
#color significant

#########

#birds only with duplicate data included
birdsduplicate <- read.csv("Birdwithdup.csv")
birdsduplicate <- transform(birdsduplicate, Treatment=paste(Color, Pattern, sep="_"))

#subset to only predation 
birdspredonly <- subset(birdsduplicate,Predation!="0")

#TABLE SHOWING COUNTS OF PREDATORS FOR EACH TREATMENT
birddup <- table(birdspredonly$Treatment, birdspredonly$Location.Bite.Marks, birdspredonly$Predator)

#TABLE SHOWING COUNTS OF PREDATORS FOR EACH TREATMENT
birddup <- table(birdsduplicate$Treatment, birdsduplicate$Predator)

#TABLE SHOWING COUNTS OF LOCATION OF BITE MARKS FOR EACH TREATMENT
birdbites<- table(birdsduplicate$Treatment, birdsduplicate$Location.Bite.Marks, birdsduplicate$Predator)

updatedbird <- glmer(Predation~Color+Pattern+ Month + (1|Site:Cluster.Number), 
                     family=binomial(link = "logit"), 
                     data=birdsduplicate)
summary(updatedbird)
#no color and pattern interaction, removed from model

#Obtain p-values for a mixed-model
mixed(updatedbird, birdsduplicate, method = 'LRT')
#color and month significant

#########

#arthropods only with duplicate data included
arthsduplicate <- read.csv("Arthwithdup.csv")
arthsduplicate <- transform(arthsduplicate, Treatment=paste(Color, Pattern, sep="_"))

#subset to only predation 
arthspredonly <- subset(arthsduplicate,Predation!="0")

#TABLE SHOWING COUNTS OF PREDATORS FOR EACH TREATMENT
arthbites <- table(arthspredonly$Treatment, arthspredonly$Location.Bite.Marks, arthspredonly$Predator)


#TABLE SHOWING COUNTS OF PREDATORS FOR EACH TREATMENT
arthdup <- table(arthsduplicate$Treatment, arthsduplicate$Predator)

#variance is 0 when random effects included. tried site alone, Cluster number alone and Cluster number 
#nested into site
updatedarth <- glm(Predation~Color+Pattern + Month, 
                   family=binomial(link = "logit"), 
                   data=arthsduplicate)

summary(updatedarth)
#no color and pattern interaction, removed from model

#Obtain p-values
anova(updatedarth, test="Chisq")
#color and month significant

#########################################
#Post-hoc tests

lsmeans(allpredatorsglm,list(pairwise~Month),adjust="tukey")

#month - inverts & verts 
lsmeans(updatedarth,list(pairwise~Month),adjust="tukey")
lsmeans(vertstestglm, list(pairwise~Month),adjust="tukey")

#treatment - inverts & verts 
lsmeans(arthtxt,list(pairwise~Treatment),adjust="tukey")
lsmeans(verttxt, list(pairwise~Treatment),adjust="tukey")

#pattern - inverts & verts 
lsmeans(updatedarth,list(pairwise~Pattern),adjust="tukey")
lsmeans(vertstestglm, list(pairwise~Pattern),adjust="tukey")

#birds - color, month
lsmeans(updatedbird,list(pairwise~Month),adjust="tukey")
lsmeans(updatedbird,list(pairwise~Color),adjust="tukey")

#rodents - color
lsmeans(updatedrodent,list(pairwise~Color),adjust="tukey")



#####################
#GLM MODELS DUPLICATES EXCLUDED
#####################

#variance is 0 when random effects included. tried site alone, Cluster number alone and Cluster number 
#nested into site
#all predator together
allpredatorsglm <- glm(Predation~Color+Pattern+Month, 
                       family=binomial(link = "logit"), 
                       data=predationtotal)
summary(allpredatorsglm)
#no color and pattern interaction, removed from model

anova(allpredatorsglm, test="Chisq")
#no significant on color or pattern (due to inverts)
#month significant

##########################################
#verts - mammals, rodents, birds
vertstestglm <- glmer(Predation~Color+Pattern + Month + (1|Site:Cluster.Number), 
                      family=binomial(link = "logit"), 
                      data=verts_only)

summary(vertstestglm)
#no color and pattern interaction, removed from model


#Obtain p-values for a mixed-model
mixed(vertstestglm, verts_only, method = 'LRT')
#color, pattern and month significant

##################
#variance is 0 when random effects included. tried site alone, Cluster number alone and Cluster number 
#nested into site
arthtestglm <- glm(Predation~Color+Pattern + Month, 
                      family=binomial(link = "logit"), 
                      data=inverts_only)
summary(arthtestglm)

anova(arthtestglm, test="Chisq")
#color and month significant 

#################
#rodents 
rodenttestglm <- glmer(Predation~Color+Pattern + Month + (1|Site:Cluster.Number), 
                     family=binomial(link = "logit"), 
                     data=rodents_only)
summary(rodenttestglm)
#no color and pattern interaction, removed from model

#Obtain p-values for a mixed-model
mixed(rodenttestglm, rodents_only, method = 'LRT')
#significant increased predation on red models- lost with data excluding duplicates

##########################################
#birds
birdtestglmer <- glmer(Predation~Color+Pattern + Month + (1|Site:Cluster.Number), 
                       family=binomial(link = "logit"), 
                       data=birds_only)
summary(birdtestglmer)
#no color and pattern interaction, removed from model

#Obtain p-values for a mixed-model
mixed(birdtestglmer, birds_only, method = 'LRT')
#color and month significant


#####################################################
#Models with Site included as fixed effect - discussion in supplementary materials
#####################################################

#all predator together
allpredatorsite <- glm(Predation~Color+Pattern+Month+Site, 
                       family=binomial(link = "logit"), 
                       data=predationtotal)
summary(allpredatorsite)


anova(allpredatorsite, test="Chisq")
#month and site significant 

#########

rodentsite <- glmer(Predation~Color + Pattern + Month + Site + (1|Cluster.Number), 
                       family=binomial(link = "logit"), 
                       data=rodentsduplicate)
summary(rodentsite)

#Obtain p-values for a mixed-model
mixed(rodentsite, rodentsduplicate, method = 'LRT')
#color and site significant

#########
birdsite <- glmer(Predation~Color + Pattern + Month + Site + (1|Cluster.Number), 
                     family=binomial(link = "logit"), 
                     data=birdsduplicate)
summary(birdsite)


#Obtain p-values for a mixed-model
mixed(birdsite, birdsduplicate, method = 'LRT')
#color, month and site significant 

#########

arthsite <- glm(Predation~Color + Pattern + Month + Site, 
                   family=binomial(link = "logit"), 
                   data=arthsduplicate)

summary(arthsite)

anova(arthsite, test="Chisq")
#color and month significant

#########
#verts - mammals, rodents, birds
vertsite <- glmer(Predation~Color + Pattern + Month + Site + (1|Cluster.Number), 
                      family=binomial(link = "logit"), 
                      data=verts_only)

summary(vertsite)

mixed(vertsite, verts_only, method = 'LRT')
#color, pattern, month and site significant 

##################
#post-hoc tests
lsmeans(arthsite,list(pairwise~Site),adjust="tukey")
lsmeans(birdsite,list(pairwise~Site),adjust="tukey")
lsmeans(rodentsite,list(pairwise~Site),adjust="tukey")
lsmeans(allpredatorsite,list(pairwise~Site),adjust="tukey")
lsmeans(vertsite, list(pairwise~Site),adjust="tukey")


