#packages needed
library(readr)
library(tidyverse)
library(dplyr)
library(ggpattern)
library(rstudioapi)
library(ggpubr)
library(cowplot)
library(ggprism)
library(patchwork)
library(gridExtra)
library(grid)


#############################
#read in attack rate csv for all predators 
finalattackrates <- read.csv("Attack_rate_Invert_BRLM.csv")


#filter
finalar <- finalattackrates %>% filter(Predator == "Arthropod"|
                                         Predator == "Bird"|Predator == "Mammal"|Predator == "Rodent")

#filter arthropods and vertebrates
vertinvert <- finalattackrates %>% filter(Predator == "Arthropod"|
                                            Predator == "Vertebrate")
##########################
#read in attack rate csv for all predators with treatment(color and pattern combined)
attacktreatment <- read.csv("TreatmentVertInvert.csv")

#just vertebrates 
vertatt <- attacktreatment %>% filter(Predator == "Vertebrate")

#just arthropods
artatt <- attacktreatment %>% filter(Predator == "Arthropods")

#STANDARD ERROR 
se <- function(x) sqrt(var(x)/length(x))


#calculate mean, std, and se - treatment, color and pattern by predator
txtpredator <- attacktreatment%>%  
  group_by(Treatment,Color, Pattern,Predator) %>% 
  summarise(mean = mean(Attack_rate),
            std = sd(Attack_rate), 
            se = se(Attack_rate))                                                                        

#calculate mean, std, and se - treatment, color and pattern verts 
vertsonly <- vertatt %>%  
  group_by(Treatment,Color, Pattern) %>% 
  summarise(mean = mean(Attack_rate),
            std = sd(Attack_rate), 
            se = se(Attack_rate))

#calculate mean, std, and se - treatment, color and pattern arthropods
artonly <- artatt %>%  
  group_by(Treatment,Color, Pattern) %>% 
  summarise(mean = mean(Attack_rate),
            std = sd(Attack_rate), 
            se = se(Attack_rate))


#######################

#visualize vertebrate and arthropod attack rates by treatment
arthandverts <- ggplot(txtpredator) +
  theme_light()+
  geom_col_pattern(
    aes(x=Treatment, y=mean, fill = Treatment, pattern = Pattern),
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = .1,
    pattern_spacing = 0.05,
    position = position_dodge2(preserve = 'single'),
  ) +
  scale_pattern_manual(
    values = c("stripe", "none", "stripe", "none")
  ) + 
  
  scale_fill_manual(values = c("#00BA38",
                               "#00BA38", 
                               "red", 
                               "red"))+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        axis.ticks.x=element_blank(), #get rid of everything in the x axis
        axis.title.x=element_blank(),        axis.text = element_text(color = "black"),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black'),
        axis.line.x= element_blank(),
        axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 20))+
  facet_grid(~Predator)+
  geom_errorbar( aes(x=Treatment, ymin=mean-se, ymax=mean+se), width=.2, colour="black", alpha=1, size=.5,
                 position = position_dodge(.9))+
  guides(pattern = guide_legend(override.aes = list(fill="white")))+
  ylab("Attack Rate (7 days)") 

#vert treatment only
verttxtonly <- ggplot(vertsonly) +
  theme_light()+
  geom_col_pattern(
    aes(x=Treatment, y=mean, fill = Treatment, pattern = Pattern),
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = .1,
    pattern_spacing = 0.05,
    position = position_dodge2(preserve = 'single'),
  ) +
  scale_pattern_manual(
    values = c("stripe", "none", "stripe", "none")
  ) + 
  
  scale_fill_manual(values = c("#00BA38",
                               "#00BA38", 
                               "red", 
                               "red"))+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        axis.ticks.x=element_blank(), #get rid of everything in the x axis
        axis.title.x=element_blank(),        axis.text = element_text(color = "black"),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black'),
        axis.line.x= element_blank(),
        axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 20))+
  geom_errorbar( aes(x=Treatment, ymin=mean-se, ymax=mean+se), width=.2, colour="black", alpha=1, size=.5,
                 position = position_dodge(.9))+
  guides(pattern = guide_legend(override.aes = list(fill="white")))+
  ylab("Attack Rate (7 days)") 

#arthropod treatment only
arttxtonly<- ggplot(artonly) +
  theme_light()+
  geom_col_pattern(
    aes(x=Treatment, y=mean, fill = Treatment, pattern = Pattern),
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = .1,
    pattern_spacing = 0.05,
    position = position_dodge2(preserve = 'single'),
  ) +
  scale_pattern_manual(
    values = c("stripe", "none", "stripe", "none")
  ) + 
  
  scale_fill_manual(values = c("#00BA38",
                               "#00BA38", 
                               "red", 
                               "red"))+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        axis.ticks.x=element_blank(), #get rid of everything in the x axis
        axis.title.x=element_blank(),        axis.text = element_text(color = "black"),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black'),
        axis.line.x= element_blank(),
        axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 20))+
  geom_errorbar( aes(x=Treatment, ymin=mean-se, ymax=mean+se), width=.2, colour="black", alpha=1, size=.5,
                 position = position_dodge(.9))+
  guides(pattern = guide_legend(override.aes = list(fill="white")))+
  ylab("Attack Rate (7 days)") 


ggsave('predandtxt.png', arthandverts, device = "png", width=7, height= 6, units="in", dpi = 600)

#########################################

#read in attack rates for just vertebrates/arthropods
vertinvertattackrate <- read.csv("Attack_rate_Vert_Invert.csv")

##calculate mean, std, and se - vertebrates color only
vertcolor <- vertinvertattackrate %>% 
  group_by(Color, Predator_Vert) %>% 
  summarise(mean = mean(Att_rate_Vert),
            std = sd(Att_rate_Vert), 
            se = se(Att_rate_Vert))

#calculate mean, std, and se - invertebrates color only
invertcolor <- vertinvertattackrate %>% 
  group_by(Color, Predator_Invert) %>% 
  summarise(mean = mean(Att_rate_Invert),
            std = sd(Att_rate_Invert), 
            se = se(Att_rate_Invert))

#################
#read in csv combined of vert/invert color 
vertinvertcolor <- read.csv("invertvertcolorattackrates.csv")


#graph
redgreen <- ggplot(vertinvertcolor) +
  geom_bar(aes(x=Color, y=mean, fill = Color), stat="identity", alpha=1) +
  facet_grid(~Predator) +geom_errorbar( aes(x=Color, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5)+
  theme_light()+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        legend.position = c(.93,.97),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black', size = 1),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        text = element_text(color = "black", size = 24))+
  scale_fill_manual("legend", values = c("Green" = "#00BA38","Red" = "red"))+
  ylab("Attack Rate (7 days)") 

ggsave('vertinvertcolattackrate.png', redgreen, device = "png", width=9, height= 5, units="in", dpi = 600)


##################
#just pattern now 

#calculate mean, std, and se verts only - Pattern
vertpattern <- vertinvertattackrate %>% 
  group_by(Pattern, Predator_Vert) %>% 
  summarise(mean = mean(Att_rate_Vert),
            std = sd(Att_rate_Vert), 
            se = se(Att_rate_Vert))

#calculate mean, std, and se inverts only - Pattern
invertpattern <- vertinvertattackrate %>% 
  group_by(Pattern, Predator_Invert) %>% 
  summarise(mean = mean(Att_rate_Invert),
            std = sd(Att_rate_Invert), 
            se = se(Att_rate_Invert))


#read in csv of vert/invert pattern
vertinvertpattern <- read.csv("invertvertpatternattackrates.csv")


# visualize vert/invert attack rate by pattern
patternattack <- ggplot(vertinvertpattern, aes(x=Predator, y = mean, fill=Pattern) ) +
  geom_col_pattern(
    aes(pattern = Pattern),
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = .1,
    pattern_spacing = 0.05,
    position = position_dodge2(preserve = 'single'),
  ) +
  scale_pattern_manual(
    values = c("stripe","none")
  ) + 
  scale_fill_manual(values = c( "lightgrey","lightgrey"))+
  theme_light()+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        legend.position = c(.93,.97),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.text.x = element_text(color="black", size = 25),
        axis.line = element_line(colour = 'black', size = 1),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        text = element_text(color = "black", size = 24))+
  geom_errorbar( aes(x=Predator, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5,
                 position = position_dodge(.9))+
  ylab("Attack Rate (7 days)")

#save
patternattack + savePlotAsImage(
  "patternattacks.png",  # path to place to store image
  format = c("png"),
  1000,
  1000
)


###################################################
#by predator and color 
predar <- finalattackrates %>% 
  group_by(Predator, Color) %>% 
  summarise(mean = mean(Attack_Rate),
            std = sd(Attack_Rate), 
            se = se(Attack_Rate))

#visualize predator attack rates by color 
predgroups <- ggplot(predar)+
  geom_bar(aes(x=Color, y=mean, fill = Color), stat="identity", alpha=1) +
  facet_grid(~Predator) +geom_errorbar( aes(x=Color, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5)+
  theme_light()+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        legend.position = c(.8,.8),
        axis.text.x =element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line = element_line(colour = 'black', size = .5),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        text = element_text(size = 22))+
  scale_fill_manual("legend", values = c("Green" = "#00BA38","Red" = "red"))+
  ylab("Attack Rate (7 days)")



#by predator and pattern 
patternpred <- finalattackrates %>% 
  group_by(Pattern, Predator) %>% 
  summarise(mean = mean(Attack_Rate),
            std = sd(Attack_Rate), 
            se = se(Attack_Rate))


#visualize predator attack rates by pattern
patternpreds <- ggplot(patternpred, aes(x=Predator, y=mean, fill = Pattern) ) +
  geom_col_pattern(
    aes(pattern = Pattern),
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = .1,
    pattern_spacing = 0.05,
    position = position_dodge2(preserve = 'single'),
  ) +
  scale_pattern_manual(
    values = c("stripe","none")
  ) + 
  scale_fill_manual(values = c( "lightgrey","lightgrey"))+
  theme_light()+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        legend.position = c(.8,.8),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.text.x = element_text(color="black", size = 18),
        axis.line = element_line(colour = 'black'),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        text = element_text(color = "black", size = 22))+
  geom_errorbar( aes(x=Predator, ymin=mean-se, ymax=mean+se), width=0.2, colour="black", alpha=1, size=.5,
                 position = position_dodge(.9))+
  ylab("Attack Rate (7 days)")


#combine above graphs
combo <- plot_grid(predgroups + theme(axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.text.x = element_blank()),
                   
                   patternpreds + theme(axis.title.y = element_blank(),
                                        axis.title = element_blank()),
                   ncol = 1)

grob <- textGrob("Attack Rate (7 days)", vjust = .3, 
                 gp=gpar(col="black", fontsize=19), rot=90)

predcombo <- grid.arrange(arrangeGrob(combo, left = grob))

ggsave('predcombo.png', predcombo , device = "png", width=6.5, height= 5.5, units="in", dpi = 600)