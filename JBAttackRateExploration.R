#packages needed
library(readr)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)


#read in data 
predationtotal <- read.csv("ModifiedPredationTotal.csv")
predationtotal <- na.omit(predationtotal) #omit NAs 
predationtotal <- transform(predationtotal, Treatment=paste(Color, Pattern, sep="_"))

#put months in order
predationtotal <- predationtotal %>% mutate(Month = fct_relevel(Month,"Late July",
                                                  "Early August",
                                                  "Late August",
                                                  "Early September",
                                                  "Late September",
                                                  "Mid-October"))


#get counts of different variables 
Cluster_counts <- predationtotal  |> 
  group_by(Site,Month,Cluster) |> count()

Trial_Count_Color_Pattern <- predationtotal |>  
  group_by(Site,Month,Color, Pattern) |> count() 


Trial_Count_Color_Pattern_Pred <- predationtotal |>  
  group_by(Site,Month,Color, Pattern) |> 
  summarise(Pred_sum  = sum(Predation))

Attack_Rate <- left_join(Trial_Count_Color_Pattern, Trial_Count_Color_Pattern_Pred,
                         by = c("Site", "Month", "Color", "Pattern")) 

# compute attack rate with data from the two columns
# Pay attention to the units attack rate per site per trial in a week

Attack_Rate <- Attack_Rate |> mutate(Att_rate = Pred_sum/n)

# save the data
write_csv(Attack_Rate, "Attack_rate.csv")

# look at data with boxplots
ggplot(Attack_Rate) + geom_boxplot(aes(Color,Att_rate ))

# No difference
ggplot(Attack_Rate) + geom_boxplot(aes(Pattern,Att_rate ))

# No difference
all <- ggplot(Attack_Rate) + geom_boxplot(aes(Site,Att_rate ))+
  theme(text = element_text(size = 20)) + ylab("All Predators")

# Bradley Palmer has a higher rate 

# to investigate trial  convert number to character variable

Attack_Rate$Trial <- as.character(Attack_Rate$Trial)

all_pred <- ggplot(Attack_Rate)  + geom_boxplot(aes(Month,Att_rate ))+
  theme(text = element_text(size = 20)) + ylab("All Predators")


# clear seasonal pattern in attack rate  Lower in the first and last trials

# check across site
ggplot(Attack_Rate)  + geom_boxplot(aes(Month,Att_rate )) + facet_wrap(~Site)

# yes the seasonal pattern generally holds up occurs across sites 
# exceptions are low at BH in 2 trial, High at BP in 6th trail, 



######################################################

# Repeat the analysis using vertebrates and non vertebrates
# Are the seasonal impacts due to differences in predators?

# First rename the "vert" column to "Predator_type"

Complete_Caterpillar_Data <- predationtotal |> 
  rename(Predator_type = Vert)

Trial_Count_Color_Pattern_Pt <- Complete_Caterpillar_Data |>  
  group_by(Site,Month,Color, Pattern) |> count() 

# gives n = 193 down from  964

Trial_Count_Color_Pattern_Pred_Pt <- count(Complete_Caterpillar_Data,
                                           Predator_type,Site,Month, Color, Pattern)



Trial_Count_Color_Pattern_Pred_Pt

Invert_Predation_count<- Trial_Count_Color_Pattern_Pred_Pt |> 
  filter(Predator_type == "Invertebrate")

Vert_Predation_count <-Trial_Count_Color_Pattern_Pred_Pt |> 
  filter(Predator_type == "Vertebrate")

# Rename columns n

Invert_Predation_count  <- Invert_Predation_count |> 
  rename(Invert_pred_Count = n)

Vert_Predation_count  <- Vert_Predation_count |> 
  rename(Vert_pred_Count = n)

Vert_Predation_count$Trial <- as.character(Vert_Predation_count$Trial)

Attack_Rate <- left_join(Attack_Rate, Vert_Predation_count ,
                         by = c("Site", "Month", "Color", "Pattern")) 

Invert_Predation_count$Trial <- as.character(Invert_Predation_count$Trial)

Attack_Rate <- left_join(Attack_Rate, Invert_Predation_count ,
                         by = c("Site", "Month", "Color", "Pattern")) 



# compute attack rate with data from the two columns
# Pay attention to the units attack rate per site per trial in a week

Attack_Rate <- Attack_Rate |> mutate(Att_rate_Vert_Predation_count/n)
Attack_Rate <- Attack_Rate |> mutate(Att_rate_Invert_Predation_count/n)

Attack_Rate <- Attack_Rate |> rename (Predator_Invert = Predator_type.y)
Attack_Rate <- Attack_Rate |> rename(Predator_Vert = Predator_type.x )


# The NAs need to be converted to 0s

Attack_Rate <-Attack_Rate |> 
  mutate (Predator_Invert = ifelse(is.na(Predator_Invert), "Invertebrate", Predator_Invert)) |> 
  mutate (Predator_Vert = ifelse(is.na(Predator_Vert), "Vertebrate", Predator_Vert)) |> 
  mutate (Invert_pred_Count = ifelse(is.na(Invert_pred_Count), 0, Invert_pred_Count)) |> 
  mutate (Vert_pred_Count = ifelse(is.na(Vert_pred_Count), 0, Vert_pred_Count)) 

Attack_Rate <-Attack_Rate |> 
  mutate(Att_rate_Vert = Vert_pred_Count/n, Att_rate_Invert = Invert_pred_Count/n)


# save the data
write_csv(Attack_Rate, "Attack_rate_Vert_Invert.csv")

# look at data with boxplots for Inverts

ggplot(Attack_Rate) + geom_boxplot(aes(Color,Att_rate_Invert ))

#green a bit over red
ggplot(Attack_Rate) + geom_boxplot(aes(Pattern,Att_rate_Invert ))
# solid over pattern

#STANDARD ERROR 
se <- function(x) sqrt(var(x)/length(x))

#Cluster # 
invertsite <- Attack_Rate %>% 
  group_by(Site,Att_rate_Invert) %>% 
  summarise(mean = mean(n),
            std = sd(n), 
            se = se(n))


invertsite <- ggplot(Attack_Rate) + geom_boxplot(aes(Site,Att_rate_Invert ))+
  theme(text = element_text(size = 20)) + ylab("Vertebrates")



# fairly equal

inverttrial <- ggplot(Attack_Rate)  + geom_boxplot(aes(Month,Att_rate_Invert))+
  theme(text = element_text(size = 20)) + ylab("Vertebrates")

# clear seasonal pattern in attack rate  Lower in the first and second trials,
# trails off 5 trail quite variable

# check across site
ggplot(Attack_Rate)  + geom_boxplot(aes(Month,Att_rate_Invert )) + facet_wrap(~Site)

# the seasonal pattern generally holds up occurs across sites 
#  but BH has its highest in 5th trial and BP is quite variablein 5th ,
# while is variable 
# 


# look at data with boxplots for Verts

ggplot(Attack_Rate) + geom_boxplot(aes(Color,Att_rate_Vert ))

# red over green oppositeve of inverts
ggplot(Attack_Rate) + geom_boxplot(aes(Pattern,Att_rate_Vert ))
#  pattern over solid opposite of inverts

vertsite <- ggplot(Attack_Rate) + geom_boxplot(aes(Site,Att_rate_Vert ))+
  theme(text = element_text(size = 20)) + ylab("Vertebrates")
#######higher for Verts

# Bradley Palmer has a higher rate, very clear and not seen in inverts

verttrial <- ggplot(Attack_Rate)  + geom_boxplot(aes(Month,Att_rate_Vert ))+
  theme(text = element_text(size = 20)) + ylab("Vertebrates")

# No seasonal pattern in attack rate for verts, Lower in one, three and four trials
# Highest in fifth trial

# check across site
ggplot(Attack_Rate)  + geom_boxplot(aes(Month,Att_rate_Vert )) + facet_wrap(~Site)

# BH even and GB mostly even except week 5,  BP generally higher and uneven


###################################################

#redoing the plots as barcharts 
siteattacks <- read.csv("vertinvertattackratessites.csv")
siteattacks <- siteattacks %>% mutate(Month = fct_relevel(Month,"Late July",
                                                          "Early August",
                                                          "Late August",
                                                          "Early September",
                                                          "Late September",
                                                          "Mid-October"))


#by site 
site_att <- siteattacks %>% 
  group_by(Site,Vert) %>% 
  summarise(mean = mean(Attack.Rates),
            std = sd(Attack.Rates), 
            se = se(Attack.Rates))

vert <- site_att  %>% filter(Vert == "Vertebrate")

invert <- site_att %>% filter(Vert == "Invertebrate")  


#visualize vertebrate site attack rate  
vert_site <- ggplot(vert) +
  ggtitle("Vertebrates")+
  geom_bar(aes(x=Site, y=mean), stat="identity", alpha=1, fill = "skyblue")+
  geom_errorbar( aes(x=Site, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5)+
  theme_light()+
  ylim(0, .4)+
  theme(strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5,size=18,
                                  vjust=-6),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black', size = 1),
        axis.text.y = element_text(face="bold"),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        text = element_text(color = "black", size = 18))


#visualize invertebrate site attack rate
invert_site <- ggplot(invert) +
  ggtitle("Arthropods")+
  geom_bar(aes(x=Site, y=mean), stat="identity", alpha=1, fill = "skyblue")+
  geom_errorbar( aes(x=Site, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5)+
  theme_light()+
  ylim(0, .4)+
  theme(strip.background = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5,size=18),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black', size = 1),
        axis.text.y = element_text(face="bold"),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        text = element_text(color = "black", size = 18))


#by Month
month_att <- siteattacks %>% 
  group_by(Month,Vert) %>% 
  summarise(mean = mean(Attack.Rates),
            std = sd(Attack.Rates), 
            se = se(Attack.Rates))

vert_month <- month_att  %>% filter(Vert == "Vertebrate")

invert_month <- month_att %>% filter(Vert == "Invertebrate")  


#visualize vertebrate month attack rate  
vmonth <- ggplot(vert_month) +
  ggtitle("Vertebrates") +
  geom_bar(aes(x=Month, y=mean), stat="identity", alpha=1, fill = "skyblue")+
  geom_errorbar( aes(x=Month, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5)+
  theme_light()+
  ylim(0, .5)+
  theme(strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5,size=20,
                                  vjust=-7),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black', size = 1),
        axis.text.y = element_text(face="bold"),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        text = element_text(color = "black", size = 24))



#visualize invertebrate month attack rate
inmonth <- ggplot(invert_month) +
  ggtitle("Arthropods") +
  geom_bar(aes(x=Month, y=mean), stat="identity", alpha=1, fill = "skyblue")+
  geom_errorbar( aes(x=Month, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5)+
  theme_light()+
  ylim(0, .5)+
  theme(strip.background = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5,size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black', size = 1),
        axis.text.y = element_text(face="bold"),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        text = element_text(color = "black", size = 24))+
  scale_x_discrete(labels = c("Late July" = "Late\nJuly",
                              "Early August" = "Early\nAugust",
                              "Late August" = "Late\nAugust",
                              "Early September" = "Early\nSeptember",
                              "Late September" = "Late\nSeptember",
                              "Mid-October" = "Mid-\nOctober"))


#combined for all predation by site
siteatt <- plot_grid(vert_site + theme(axis.text.x= element_blank(),
                                       axis.title.x = element_blank(),
                                       text = element_text(size = 16),
                                       plot.title = element_text(size=16)), 
                     
                     invert_site + theme(axis.text.x = element_text(size = 16),
                                         plot.title = element_text(size=16)),
                     
                     ncol = 1)


y.grob <- textGrob("Attack Rate (7 days)", vjust = .3, 
                   gp=gpar(col="black", fontsize=17), rot=90)

b <- grid.arrange(arrangeGrob(siteatt, left = y.grob))


ggsave('sitepred.png', b , device = "png", width=5, height= 5, units="in", dpi = 600)


#combined for all predation by month

monthatt <- plot_grid(vmonth + theme(axis.text.x= element_blank(),
                                     axis.title.x = element_blank(),
                                     text = element_text(size = 24),
                                     plot.title = element_text(size=26)), 
                      
                      inmonth + theme(axis.text.x = element_text(size = 24),
                                      plot.title = element_text(size=26)),
                      ncol = 1)


y.grob <- textGrob("Attack Rate (7 days)", 
                   gp=gpar(col="black", fontsize=28), rot=90)

m <- grid.arrange(arrangeGrob(monthatt, left = y.grob))

ggsave('monthpred.png', m , device = "png", width=12, height= 10, units="in", dpi = 600)
