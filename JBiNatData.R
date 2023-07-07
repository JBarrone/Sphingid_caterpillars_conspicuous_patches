#packages needed
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)
library(afex)
library(nnet)
library(lubridate)
library(readxl)

##########################
#COLOR DATA - JUST 4 COLOR CATEGORIES: BROWN, GREEN, ORANGE, RED
##########################

#larvae color data from inaturalist
larvaecolor <- read.csv("updatedlarvaecolorinat.csv")

larvaecolor <- larvaecolor %>% filter(Month == "June" | Month == "July"|
                                        Month == "August"|Month == "September"|Month == "October")

#chi-squared test looking at month and color
chisq.test(larvaecolor$Color, larvaecolor$Month, correct=FALSE, simulate.p.value = TRUE)
#month and color are not independent


#count how many larvae by color - brown and green most frequent, 
updatedcolorcounts <- larvaecolor %>% count(Color)

#count how many larvae by color grouped by instar
updatedinstarcolor <- larvaecolor %>% group_by(Stage) %>% count(Color)

#count how many larvae by color grouped by month
updatedmonthcolor <- larvaecolor %>% group_by(Month) %>% count(Color)
updatedmonthcolor$Month <- factor(updatedmonthcolor$Month, levels = c("March", "April", "May", 
                                                                     "June", "July", "August", "September", "October", "November"))

#don't include March or April, May, or Nov (small sample sizes)
updatedmonthcolor <- updatedmonthcolor %>% filter(Month == "June" | Month == "July"|
                                                  Month == "August"|Month == "September"|Month == "October")


#graph showing proportions of caterpillar colors throughout the months
proportions <- ggplot(data = updatedmonthcolor,
                         mapping=aes(fill=Color, y=n, x=Month, color=Color))+
                    geom_bar(position='stack', stat='identity', colour="black") +
  scale_fill_manual(values = c("Brown" = "chocolate4",
                                 "Green" = "forestgreen",
                                 "Orange" = "orange",
                                 "Red" = "red"))+
  scale_color_manual(values = c("Brown" = "chocolate4",
                               "Green" = "forestgreen",
                               "Orange" = "orange",
                               "Red" = "red"))+
  theme_light()+
  theme(legend.title = element_blank(),
    strip.background = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(colour = 'black', size = 1.2),
        axis.line.y = element_line(colour = 'black', size = 1),
        axis.line.x = element_line(colour = 'black', size = 1),
        axis.line = element_line(colour = 'black', size = 1),
        text = element_text(color = "black", size = 22))+
  ylab("Number of Caterpillars")

ggsave('colorproportions.pdf', proportions, device = "pdf", width=9.6, height= 6, units="in", dpi = 400)


#########################
#VIEW LATITUDE VS DAY OF YEAR - COLOR DATA
#########################

larvaecolorinat <- read_excel("updatedlarvaecolorinat.xlsx", 
                                      col_types = c("numeric", "text", "text", 
                                                    "text", "text", "text", "numeric", 
                                                    "date", "text", "text", "text", "text", 
                                                    "text", "text", "numeric", "numeric", 
                                                    "text", "text", "text", "text"))

#create column for day of the year
larvaecolorinat <- mutate (larvaecolorinat,day_of_year = yday(observed_on))

larvaecolorinat$Stage <- as.factor(larvaecolorinat$Stage)

str(larvaecolorinat)

#visualize day of year by latitude 
latitudegraph <- ggplot(data = larvaecolorinat %>% filter(Month == "June"| Month == "July"|
                                                                    Month == "August"|Month == "September"|Month == "October"),
                        mapping=aes(x=day_of_year, y=latitude, colour = Color))+
  geom_point()+ facet_grid(~Color)+
  scale_colour_manual(values = c("Brown" = "chocolate4",
                                 "Green" = "forestgreen",
                                 "Orange" = "orange",
                                 "Red" = "red"))+
  theme_bw()+
  theme(legend.title = element_blank(), #get rid of legend label
        strip.background = element_blank(),
        axis.line = element_line(colour = 'black', size = .5),
        axis.text.y = element_text(face="bold"),
        axis.ticks.y = element_line(colour = 'black', size = .5),
        panel.border = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        text = element_text(size = 28, color = 'black'),
        legend.position="none")+
  scale_x_continuous(limits = c(150, 325))+
  xlab("Day of Year") + ylab("Latitude")+
  # stat_smooth(geom="line", color="black", size=1)
  stat_smooth(geom="line", method="lm", color="black", size=1)



ggsave('latitudeplots.png', latitudegraph, device = "png", width=12, height= 6.5, units="in", dpi = 400)


###########################################
#MULTINOMIAL LOGISTIC REGRESSION####
###########################################

#larvae color data from inaturalist - don't include March or April, May, or Nov (small sample sizes)
larvaecoloromitmonths <- larvaecolor %>% filter(Month == "June" | Month == "July"|
                                               Month == "August"|Month == "September"|Month == "October")

#check structure of data
str(larvaecoloromitmonths)

#the outcome variable is color (categorical)- make sure it's a factor 
larvaecoloromitmonths$Color <- as.factor(larvaecoloromitmonths$Color)

#the predictor is month (ordinal), put it in order
larvaecoloromitmonths$Month <- factor(larvaecoloromitmonths$Month, levels = c("June", "July", "August", "September", "October"))

#and make a numeric version for testing. replace Month with Month_num if you want to run this with month as a continuous variable.
larvaecoloromitmonths$Month_num <- as.numeric(larvaecoloromitmonths$Month)

# check which color is first in order - will be used as reference
levels(larvaecoloromitmonths$Color)
#brown as reference

#fit the model
multinomreg <- multinom(Color ~ Month, data = larvaecoloromitmonths)

#check the output
summary(multinomreg)
#we get ecoeffs and std errors.
#each row is 1 comparison
#in this case, June and brown are baselines
#the first row compares green to brown.
#ln(P(color = green)/P(color = brown)) = intercept + b1(month = 2) + b2(month = 3) + b3(month = 4)
#the log odds of being green vs being brown will decrease by 0.6 if moving from july to august
#the log odds of being green vs being brown will dcrease by 1.36 if moving from august to september

#we can see changes in log odds, but nnet does not give us p-values by default. we have to run our own Z tests and calculate p-values

z <- summary(multinomreg)$coefficients/summary(multinomreg)$standard.errors
z

# 2-tailed z multinomreg
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#Now we can add to our initial interpretation:
#the log odds of being green vs being brown will decrease significantly  by 0.6 if moving from july to august

#keep brown as reference, fairly constant and high proportions 

odds_ratios <- exp(summary(multinomreg)$coefficients)
data.frame(t(odds_ratios))

#All else being equal, the month of august decreases the relative odds 
#of a green caterpillar versus brown by approximately 45% and increases the 
#relative odds of an orange caterpillar vs brown by approximately 86%. 

#to get predicted probabiltiy values, we need to set up an empty dataframe with the predictors of interest - in this case we want 1 column containing our months of interest
newdata <- data.frame(Month =c("June", "July", "August", "September", "October"))

#use the model to predict color probabilities
predict(multinomreg, newdata = newdata, "prob")

predict(multinomreg, newdata = newdata,interval = 'confidence')

#convert to a tibble for plotting
predictions <- as_tibble(predict(multinomreg, newdata = newdata, "prob")) %>%
  mutate(Month =c("June", "July", "August", "September", "October"))

#put month in the right order so that the graphs look right
predictions$Month <- factor(predictions$Month, levels = c("June", "July", "August", "September", "October"))

#and make it long
predictions_long <- predictions %>%
  pivot_longer(-Month, names_to = "color", values_to = "probability")


#plot predicted probabilities for each color for each month
probs<- ggplot(predictions_long, aes(x = Month, y = probability, group = color)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(strip.background = element_blank(),
        axis.ticks.x=element_blank(), #get rid of everything in the x axis
        axis.title.x=element_blank(),
        text = element_text(size = 19.5, face = "bold"))+
  facet_wrap(~color) + ylab("Probability")


ggsave('colorprobs.png', probs, device = "png", width=13, height= 5, units="in", dpi = 600)





