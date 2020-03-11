# Title: WWF BAT 3 - Upper Berbice: data analysis
# Description: Mammal camera trap data analysis with camera trap locations in a rainforest
#              recently opened for logging.
# Project: Winston-Cobb memorial project
# Author: Meshach Pierre
# Date: 16th January 2020

#Clean up the environment
rm(list=ls())

#################################### PACKAGES ###########################################
# call several packages at once
# from: https://gist.github.com/RobertMyles/96665be044540e9ac668ee697dac6db7
packages <- c("tidyverse","lubridate","vegan","reshape2","BiodiversityR")
lapply(packages, library, character.only = TRUE)
rm(packages)
#################################### LOAD DATA ###########################################
capture.tb <- read_csv("data/capture_clean.csv")

head(capture.tb)
tail(capture.tb)
glimpse(capture.tb)

#################################### QUICK PREP ###########################################
# Read factor columns to factors
factor_cols <- c(names(capture.tb[,c(1,2,7,8,9,10)]))
factor_cols

capture.tb <- capture.tb %>% 
  mutate_each_(funs(factor(.)), factor_cols) %>% 
  glimpse()

rm(factor_cols)

################################SPECIES ACCUM CURVE######################################

#Make dataframe with just mammals
capture_mammals.tb <- capture.tb %>% 
  filter(name_order=="M")

#Make dataframe with species observations by day count
spec_acc.tb <- capture_mammals.tb %>% 
  filter(name_order != "B") %>%
  select("date_dayno","name_sci") %>% 
  table() %>% 
  as.data.frame() %>% 
  spread(name_sci,Freq)

#Make dataframe without date_dayno
spec_acc_BDR.tb <- spec_acc.tb[,-1]

#Make into a matrix for specaccum
spec_acc.mat <- as.matrix(spec_acc.tb[,-1])

#Species accumulation curve generation
spec_acc.curve <-specaccum(spec_acc.mat, method="random", permutations=100)

#Make specaccum output into a dataframe. Code for the following taken from 
#Rovero & Zimmermann (2016) Camera Trapping for Wildlife Research
spec_curve.df <- data.frame(spec_acc.curve$sites, spec_acc.curve$richness, spec_acc.curve$sd)
colnames(spec_curve.df) <- c("Camera.trap.days", "species", "sd")

#Plot
specaccum.plot <- ggplot(spec_curve.df, aes(x=Camera.trap.days, y=species)) +
  geom_line(aes(y=species-sd), colour="grey50", linetype="dotted") +
  geom_line(aes(y=species+sd), colour="grey50", linetype="dotted") +
  theme_bw() +
  labs(x = "Trap nights", y="Species richness") +
  geom_line()

#View plot
specaccum.plot

#Save plot
ggsave(file="specaccum_plot.eps", path = "plots", plot=specaccum.plot, width=5, height=3)
ggsave(file="specaccum_plot.png", path = "plots", plot=specaccum.plot, width=5, height=3)

#Species richness
specpool(spec_acc.mat)

diversityresult(spec_acc_BDR.tb, y=NULL, index="inverseSimpson", method="pooled", digits=5)

######################################## RAI ##############################################

#Load CT setup datasheet, check if okay and sum + output total trap nights
ct_setup.tb <- read_csv("data/ct_setup_raw.csv")
glimpse(ct_setup.tb)

#Some cameras with too few trap nights. Remove from dataframe
ct_setup.tb <- ct_setup.tb %>% 
  filter(trap_nights>30) %>% 
  glimpse

#output total trap nights to a variable trapnights.var
trapnights.var <- sum(ct_setup.tb$trap_nights)

#make new tb with rai by species, first grouping, then summarizing
species_rai.tb <- capture.tb %>% 
  select(name_order, name_sci, count_ind) %>% 
  group_by(name_sci) %>% 
  summarize(sum(count_ind)) %>% 
  rename(sum_ind = `sum(count_ind)`) %>% 
  mutate(rai = (sum_ind/trapnights.var*100))

#view the tb
species_rai.tb

name_order <- capture.tb %>% 
  select(name_order, name_sci) %>% 
  arrange(name_sci) %>% 
  unique() %>% 

species_rai.tb <- species_rai.tb %>% 
  arrange(rai) 
  
  
glimpse(species_rai.tb)

species_rai.tb
species_rai.tb$rai_round <- round(species_rai.tb$rai, digits = 3)
species_rai.tb

species_rai.tb <- species_rai.tb %>% 
  filter(name_order != "B" & name_sci != "Dasypus sp.") %>% 
  mutate(name_sci = fct_reorder(name_sci, desc(rai))) %>% 
  arrange(rai_round)

#plot RAIs
rai.plot <- ggplot(species_rai.tb, aes(x=name_sci, y=rai)) +
  geom_bar(stat = "identity") +
  labs(x = "Scientific name", y="RAI") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

rai.plot

#save rai plot
ggsave(file="rai_plot.eps", path = "plots", plot=rai.plot, width=5, height=4)
ggsave(file="rai_plot.png", path = "plots", plot=rai.plot, width=5, height=4)

citation(package = "vegan")
citation(package = "BiodiversityR")
citation(package = "base")
