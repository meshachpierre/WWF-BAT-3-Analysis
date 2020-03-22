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
packages <- c("tidyverse","lubridate","vegan","reshape2","BiodiversityR", "cowplot")
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

#Make dataframe with just mammals
capture.tb <- capture.tb %>% 
  filter(name_order=="M")

################################ SPECIES ACCUM CURVE ######################################

#Make dataframe with species observations by day count
spec_acc.tb <- capture.tb %>% 
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
  filter(trap_nights!=0) %>% 
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

(species_rai.tb <- species_rai.tb %>% 
  arrange(rai)) 

species_rai.tb
species_rai.tb$rai_round <- round(species_rai.tb$rai, digits = 3)
species_rai.tb

species_rai.tb <- species_rai.tb %>% 
  mutate(name_sci = fct_reorder(name_sci, desc(rai))) %>% 
  arrange(rai_round)

#plot RAIs
(rai.plot <- ggplot(species_rai.tb, aes(x=name_sci, y=rai)) +
  geom_bar(stat = "identity") +
  labs(x = "Scientific name", y="RAI") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm")))


#save rai plot
ggsave(file="rai_plot.eps", path = "plots", plot=rai.plot, width=5, height=4)
ggsave(file="rai_plot.png", path = "plots", plot=rai.plot, width=5, height=4)


############################ BOOTSTRAP RAI's ####################################

#Load CT setup datasheet, check if okay and sum + output total trap nights
ct_setup.tb <- read_csv("data/ct_setup_raw.csv")
glimpse(ct_setup.tb)

#Some cameras with too few trap nights. Remove from dataframe
ct_setup.tb <- ct_setup.tb %>% 
  filter(trap_nights!=0) %>% 
  select(station,trap_nights) %>% 
  rename("id_station" = station) %>%  
  glimpse

ct_setup.tb$id_station <- as.factor(ct_setup.tb$id_station)

#Confirm that the camera traps with zero trap nights were removed
capture_2.tb <- capture.tb %>% 
  filter(id_station != "CT2A" & id_station != "CT2B" & id_station != "CT4N")

#add in trap night column
capture_2.tb <- left_join(capture_2.tb,ct_setup.tb, "id_station") %>% 
  glimpse()

#select relevant columns/variables, group by species and station, then collapse by these values
#and spread into wide tibble with each species as a column and each CT as a row
capture_wide.tb <- capture_2.tb %>% 
  select(id_station, trap_nights, name_sci, count_ind) %>% 
  group_by(id_station,name_sci) %>% 
  summarise(trap_nights = unique(trap_nights),
            count_ind = (sum(count_ind)/trap_nights)*100) %>% 
  spread(name_sci,count_ind) %>% 
  glimpse()

#Remove trap night column and make NA's into 0's
capture_wide.tb <- capture_wide.tb[,c(-2,-1)]
capture_wide.tb[is.na(capture_wide.tb)] <- 0

glimpse(capture_wide.tb)

#functions to bootstrap individual columns
boot_ci <- function(x){
  reps <- numeric(1000)
  for(i in 1:1000) reps[i] <- mean(sample(x, replace = T))
  quantile(reps, c(.025, .975))
}

boot_mean <- function(x){
  reps <- numeric(1000)
  for(i in 1:1000) reps[i] <- mean(sample(x, replace = T))
  mean(reps)
}

#Apply functions to tibble and place results into variables
capture_boots_ci <- sapply(capture_wide.tb, function(x) boot_ci(x))
capture_boots_mean <- sapply(capture_wide.tb, function(x) boot_mean(x))

#make dataframe, then tibble
(capture_boots <- (as.data.frame(rbind(capture_boots_ci, capture_boots_mean))))
values <- c("ci_l","ci_u","mean")
capture_boots <- cbind(values,capture_boots)
(capture_boots <- as.tibble(capture_boots))

capture_boots_long <- melt(capture_boots, id=c("values"))
capture_boots_long <- spread(capture_boots_long, values, value)

#rearrange df
(capture_boots_long <- capture_boots_long %>% 
  rename("name_sci" = variable,
         "rai_boot" = mean) %>% 
  mutate(name_sci = fct_reorder(name_sci, desc(rai_boot))) %>% 
  arrange(rai_boot))

capture_boots_long

capture_boots_long <- left_join(capture_boots_long, species_rai.tb, "name_sci") %>% 
  mutate(name_sci = fct_reorder(name_sci, desc(rai))) %>% 
  arrange(rai)

capture_boots_long

write_csv(capture_boots_long,path="data/rai_result_both_(boots-reg).csv")

#plot
(rai_boots.plot <- ggplot(capture_boots_long, aes(x=name_sci, y=rai_boot)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=ci_l,ymax=ci_u),width=.5) +
  labs(x = "Scientific name", y="RAI") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm")))

#save bootstrapped rai plot
ggsave(file="rai_boots_plot.eps", path = "plots", plot=rai_boots.plot, width=5, height=4)
ggsave(file="rai_boots_plot.png", path = "plots", plot=rai_boots.plot, width=5, height=4)

#################################### LOAD DATA ###########################################
capture_bsl.tb <- read_csv("data/capture_clean_bsl.csv")

head(capture.tb)
tail(capture.tb)
glimpse(capture.tb)

#################################### QUICK PREP ###########################################
# Read factor columns to factors
factor_cols <- c(names(capture.tb[,c(1,4,5,6)]))
factor_cols

capture.tb <- capture.tb %>% 
  mutate_each_(funs(factor(.)), factor_cols) %>% 
  glimpse()

rm(factor_cols)

#Make dataframe with just mammals
capture_bsl.tb <- capture_bsl.tb %>% 
  filter(name_order=="M")

#Check mammals and remove leopardus sp.
capture_bsl.tb <- capture_bsl.tb %>% 
  filter(name_sci!="Leopardus sp") %>% 
  filter(id_station != "BSLL35")

capture_bsl.tb <- droplevels(capture_bsl.tb)
summary(capture_bsl.tb$name_sci)

capture_bsl.tb$count_ind <- as.double(capture_bsl.tb$count_ind)
summary(capture_bsl.tb$count_ind)
#### There is an NA on count_ind in line 307 of the datasheet
glimpse(capture_bsl.tb)

################################ SPECIES ACCUM CURVE ######################################

#Make dataframe with species observations by day count
spec_acc_bsl.tb <- capture_bsl.tb %>% 
  select("date_dayno","name_sci") %>% 
  table() %>% 
  as.data.frame() %>% 
  spread(name_sci,Freq)

#Make dataframe without date_dayno
spec_acc_BDR_bsl.tb <- spec_acc_bsl.tb[,-1]

#Make into a matrix for specaccum
spec_acc_bsl.mat <- as.matrix(spec_acc_bsl.tb[,-1])

#Species accumulation curve generation
spec_acc_bsl.curve <-specaccum(spec_acc_bsl.mat, method="random", permutations=100)

#Make specaccum output into a dataframe. Code for the following taken from 
#Rovero & Zimmermann (2016) Camera Trapping for Wildlife Research
spec_curve_bsl.df <- data.frame(spec_acc_bsl.curve$sites, spec_acc_bsl.curve$richness, spec_acc_bsl.curve$sd)
colnames(spec_curve_bsl.df) <- c("Camera.trap.days", "species", "sd")

#Plot
specaccum.plot_bsl <- ggplot(spec_curve_bsl.df, aes(x=Camera.trap.days, y=species)) +
  geom_line(aes(y=species-sd), colour="grey50", linetype="dotted") +
  geom_line(aes(y=species+sd), colour="grey50", linetype="dotted") +
  theme_bw() +
  labs(x = "Trap nights", y="Species richness") +
  geom_line()

#View plot
specaccum.plot_bsl

#Save plot
ggsave(file="specaccum_plot_bsl.eps", path = "plots", plot=specaccum.plot_bsl, width=5, height=3)
ggsave(file="specaccum_plot_bsl.png", path = "plots", plot=specaccum.plot_bsl, width=5, height=3)

#Species richness
specpool(spec_acc_bsl.mat)

diversityresult(spec_acc_BDR_bsl.tb, y=NULL, index="inverseSimpson", method="pooled", digits=5)

######################################## RAI ##############################################

#Load CT setup datasheet, check if okay and sum + output total trap nights
ct_setup_bsl.tb <- read_csv("data/bsl_ct_setup_raw.csv")
glimpse(ct_setup_bsl.tb)

#Some cameras with too few trap nights. Remove from dataframe
(ct_setup_bsl.tb <- ct_setup_bsl.tb %>% 
  filter(trap_nights!=0) %>%
  filter(trap_nights!=1) %>% 
  glimpse)

#output total trap nights to a variable trapnights.var
trapnights_bsl.var <- sum(ct_setup_bsl.tb$trap_nights)

#make new tb with rai by species, first grouping, then summarizing
(species_rai_bsl.tb <- capture_bsl.tb %>% 
  select(name_order, name_sci, count_ind) %>% 
  group_by(name_sci) %>% 
  summarize(sum(count_ind)) %>% 
  rename(sum_ind = `sum(count_ind)`) %>% 
  mutate(rai = (sum_ind/trapnights.var*100)))

#view the tb
species_rai_bsl.tb

(species_rai_bsl.tb <- species_rai_bsl.tb %>% 
    arrange(rai)) 

species_rai_bsl.tb
species_rai_bsl.tb$rai_round <- round(species_rai_bsl.tb$rai, digits = 3)
species_rai_bsl.tb

(species_rai_bsl.tb <- species_rai_bsl.tb %>% 
  mutate(name_sci = fct_reorder(name_sci, desc(rai))) %>% 
  arrange(rai_round))

#plot RAIs
(rai.plot_bsl <- ggplot(species_rai_bsl.tb, aes(x=name_sci, y=rai)) +
    geom_bar(stat = "identity") +
    labs(x = "Scientific name", y="RAI") +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    theme(axis.text.x = element_text(face = "italic")) +
    theme(plot.margin=unit(c(1,1,1,1), "cm")))


#save rai plot
ggsave(file="rai_plot_bsl.eps", path = "plots", plot=rai.plot_bsl, width=5, height=4)
ggsave(file="rai_plot_bsl.png", path = "plots", plot=rai.plot_bsl, width=5, height=4)


############################ BOOTSTRAP RAI's ####################################

#Load CT setup datasheet, check if okay and sum + output total trap nights
ct_setup_bsl.tb <- read_csv("data/bsl_ct_setup_raw.csv")
glimpse(ct_setup_bsl.tb)

#Some cameras with too few trap nights. Remove from dataframe
ct_setup_bsl.tb <- ct_setup_bsl.tb %>% 
  filter(trap_nights!=0 & trap_nights!=1) %>% 
  select(station,trap_nights) %>% 
  rename("id_station" = station) %>%  
  glimpse

ct_setup_bsl.tb$id_station <- as.factor(ct_setup_bsl.tb$id_station)

#add in trap night column
capture_2_bsl.tb <- left_join(capture_bsl.tb,ct_setup_bsl.tb, "id_station") %>% 
  glimpse()

#select relevant columns/variables, group by species and station, then collapse by these values
#and spread into wide tibble with each species as a column and each CT as a row
capture_wide_bsl.tb <- capture_2_bsl.tb %>% 
  select(id_station, trap_nights, name_sci, count_ind) %>% 
  group_by(id_station,name_sci) %>% 
  summarise(trap_nights = unique(trap_nights),
            count_ind = (sum(count_ind)/trap_nights)*100) %>% 
  spread(name_sci,count_ind) %>% 
  glimpse()

#Remove trap night column and make NA's into 0's
capture_wide_bsl.tb <- capture_wide_bsl.tb[,c(-2,-1)]
capture_wide_bsl.tb[is.na(capture_wide_bsl.tb)] <- 0

glimpse(capture_wide_bsl.tb)

#functions to bootstrap individual columns
boot_ci <- function(x){
  reps <- numeric(1000)
  for(i in 1:1000) reps[i] <- mean(sample(x, replace = T))
  quantile(reps, c(.025, .975))
}

boot_mean <- function(x){
  reps <- numeric(1000)
  for(i in 1:1000) reps[i] <- mean(sample(x, replace = T))
  mean(reps)
}

#Apply functions to tibble and place results into variables
capture_boots_ci_bsl <- sapply(capture_wide_bsl.tb, function(x) boot_ci(x))
capture_boots_mean_bsl <- sapply(capture_wide_bsl.tb, function(x) boot_mean(x))

#make dataframe, then tibble
(capture_boots_bsl <- (as.data.frame(rbind(capture_boots_ci_bsl, capture_boots_mean_bsl))))
values_bsl <- c("ci_l","ci_u","mean")
capture_boots_bsl <- cbind(values_bsl,capture_boots_bsl)
(capture_boots_bsl <- as_tibble(capture_boots_bsl))

capture_boots_long_bsl <- melt(capture_boots_bsl, id=c("values_bsl"))
capture_boots_long_bsl <- spread(capture_boots_long_bsl, values_bsl, value)

#rearrange df
(capture_boots_long_bsl <- capture_boots_long_bsl %>% 
    rename("name_sci" = variable,
           "rai_boot" = mean) %>% 
    mutate(name_sci = fct_reorder(name_sci, desc(rai_boot))) %>% 
    arrange(rai_boot))

capture_boots_long_bsl

capture_boots_long_bsl <- left_join(capture_boots_long_bsl, species_rai_bsl.tb, "name_sci") %>% 
  mutate(name_sci = fct_reorder(name_sci, desc(rai))) %>% 
  arrange(rai)

capture_boots_long_bsl

write_csv(capture_boots_long_bsl,path="data/rai_result_bsl_both_(boots-reg).csv")

#plot
(rai_boots.plot_bsl <- ggplot(capture_boots_long_bsl, aes(x=name_sci, y=rai)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin=ci_l,ymax=ci_u),width=.5) +
    labs(x = "Scientific name", y="RAI") +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    theme(axis.text.x = element_text(face = "italic")) +
    theme(plot.margin=unit(c(1,1,1,1), "cm")))

#save bootstrapped rai plot
ggsave(file="rai_boots_plot_bsl.eps", path = "plots", plot=rai_boots.plot_bsl, width=6, height=4)
ggsave(file="rai_boots_plot_bsl.png", path = "plots", plot=rai_boots.plot_bsl, width=6, height=4)

######################## JOINT PLOTS ############################

(specaccum.plot_both <- plot_grid(specaccum.plot, specaccum.plot_bsl, labels = "AUTO"))
(rai_boots.plot_both <- plot_grid(rai_boots.plot, rai_boots.plot_bsl, labels = "AUTO"))

#save specaccuum plot with both datasets
ggsave(file="specaccum.plot_both.eps", path = "plots", plot=specaccum.plot_both, width=6, height=4)
ggsave(file="specaccum.plot_both.png", path = "plots", plot=specaccum.plot_both, width=6, height=4)
#save bootstrapped rai plot with both datasets
ggsave(file="rai_boots.plot_both.eps", path = "plots", plot=rai_boots.plot_both, width=6, height=4)
ggsave(file="rai_boots.plot_both.png", path = "plots", plot=rai_boots.plot_both, width=6, height=4)

######################## CITATIONS ##############################

citation(package = "vegan")
citation(package = "BiodiversityR")
citation(package = "base")
