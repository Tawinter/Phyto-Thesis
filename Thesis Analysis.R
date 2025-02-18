#Thesis Analysis


#Grapphing packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(formattable)
library(scales)
library(lubridate)
library(lemon)
library(viridis)

#Regression of co-occurrence packages
library(nlme)
library(DescTools)
library(mblm)

#PLS analysis packages
library(pls)
library (plsVarSel)

#Generalized Linear Model packages
library(faraway)
library(MASS)

############General Trend Graphs Beings Here#########################

#Q.1 Seasonal Pattern Graphs

#Refer to Month Sums.R file for data manipulation

totalch <- read.csv("totalch.csv", stringsAsFactors = TRUE)

colnames(totalch)<- c("Year", "Month", "Station", "Species", "Sum")

totalch <- 
  totalch %>%
  mutate_at("Station", str_replace, "CML", "UNH Pier")

totalch <- 
  totalch %>%
  mutate_at("Station", str_replace, "Hampton", "HHHR2")

mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

totalch$MoAb <- mymonths[ totalch$Month ]


#Dot plot of individual years DONE

totalch$MoAb = factor(totalch$MoAb, levels = month.abb)

ggplot(totalch, aes(x = MoAb, y = Sum)) + 
  geom_point(aes(fill = factor(Year)), size = 2, shape = 21) +
  scale_fill_manual(values = c("#440154FF", "#39568CFF", "#1F968BFF", 
                      "#3CBB75FF", "#95D840FF", "#FDE725FF")) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_discrete(limits = month.abb) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45)) +
  labs(fill = "Year") +
  xlab('Month')+
  ylab(bquote('Log sum abundance '(cells~L^-1))) +
  facet_grid(rows = vars(Species), cols = vars(Station))

#Box and whisker of month sum, years combined using totalch.csv from previous graph DONE
setwd("D:/R/phyto-thesis")

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Temp", "Salinity")

cml_mos <- cml[ , c("Month", "Station", "Alex", "Large_PN", "Small_PN")]

hampton_mos <- hampton[ , c("Month", "Station", "Alex", "Large_PN", "Small_PN")]

cmol <- gather(cml_mos, Species, Abundance, Alex, Large_PN, Small_PN)

hmol <- gather(hampton_mos, Species, Abundance, Alex, Large_PN, Small_PN)

chmo <- rbind(cmol, hmol)

mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

chmo$MoAb <- mymonths[ chmo$Month ]

write.csv(chmo,'ch_boxplot.csv', row.names = FALSE)

chmo <- read.csv("ch_boxplot.csv", stringsAsFactors = TRUE)

chmo$MoAb = factor(chmo$MoAb, levels = month.abb)

Alex_limit <- log10(7.5)
Large_PN <- log10(2000)
Small_PN <- log10(15000)

cell_limits <- data.frame(group = unique(chmo$Species), hline = c(0.875, 3.301, 4.176))

ggplot(chmo, aes(x = MoAb, y = Abundance)) + 
  geom_boxplot() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_discrete(limits = month.abb) +
  geom_hline(data = chmo %>% filter(Species == "Alex"),
             aes(yintercept = 7.5), col = "purple", linetype = 1)+
  geom_hline(data = chmo %>% filter(Species == "Large_PN"),
             aes(yintercept = 2000), col = "purple", linetype = 1)+
  geom_hline(data = chmo %>% filter(Species == "Small_PN"),
             aes(yintercept = 15000), col = "purple", linetype = 1)+
  theme_bw() + 
  xlab('Month')+
  ylab(bquote('Log total abundance of all years '(cells~L^-1))) +
  facet_grid(rows = vars(Species), cols = vars(Station), scales = "free_y")


#How are the populations changing over time?

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", "Temp", "Salinity")

#Two panel graph with max of all species and years on a single graph, locations separate, DIDN'T USE

cml_max <- cml %>% 
  group_by(Year, Station) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), max, na.rm = TRUE)

hampton_max <- hampton %>% 
  group_by(Year, Station) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), max, na.rm = TRUE)

maxch <- rbind(cml_max, hampton_max)

maxch <- gather(maxch, Species, Max, Alex, Large_PN, Small_PN)

write.csv(maxch,'maxch.csv', row.names = FALSE)

maxch <- read.csv("maxch.csv", stringsAsFactors = TRUE)

ggplot(maxch, aes(x = Year, y = Max))  + 
  geom_point(aes(fill = factor(Species)), size = 3, shape = 21) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  labs(fill = "Species") +
  xlab('Year')+
  ylab(bquote('Log max abundance '(cells~L^-1))) +
  facet_grid(cols = vars(Station))


#Two panel graph with sum of all species and years on a single graph, locations separate DONE
cml_sum <- cml %>% 
  group_by(Year, Station) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

hampton_sum <- hampton %>% 
  group_by(Year, Station) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

sumch <- rbind(cml_sum, hampton_sum)

sumch <- gather(sumch, Species, Sum, Alex, Large_PN, Small_PN)

write.csv(sumch,'sumch.csv', row.names = FALSE)

sumch <- read.csv("sumch.csv", stringsAsFactors = TRUE)

ggplot(sumch, aes(x = Year, y = Sum))  + 
  geom_point(aes(fill = factor(Species), shape = (factor(Species))), size = 3) +
  scale_fill_manual(name = "Species",
                    labels = c("Alex", "Large_PN", "Small_PN"),
                    values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_shape_manual(name = "Species",
                     labels = c("Alex", "Large_PN", "Small_PN"),
                     values = c(21, 22, 23)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  xlab('Year')+
  ylab(bquote('Log sum abundance '(cells~L^-1))) +
  facet_grid(cols = vars(Station))


#Are they co-occurring?

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Temp", "Salinity")

cml <- cml[, c("Week", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")]

comb <- cml %>% full_join(hampton)

comb <- gather(comb, Size_class, Abundance, Large_PN:Small_PN, factor_key=TRUE)

write.csv(comb, "combinedch.csv", row.names = FALSE)

combch <- read.csv("combinedch.csv" , stringsAsFactors = TRUE)

ggplot(coocur, aes(x = Abundance, y = Alex))  + 
  geom_point(size = 3) +
  geom_smooth(method="lm", se=FALSE, linetype = 'dashed', fullrange = TRUE) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  xlab(expression(paste("Log ", italic("Pseudo-nitzschia "), "abundance ", (cells~L^-1))))+
  ylab(expression(paste("Log ", italic("Alexandrium "), "abundance ", (cells~L^-1))))+
  facet_rep_wrap(~ interaction(Size_class, Station), scales='free_x', repeat.tick.labels = 'bottom')


#Filtering out zeros to get count for co occurance table
coocur <- combch %>% filter(Alex > 0, Abundance > 0)

write.csv(coocur, "coocur.csv", stringsAsFactors = TRUE)

table(coocur$Station, coocur$Size_class, coocur$Year)

#Regression of co ocurrance

#Spearman rank correlation as the data is not normally distributed
  #Attempted a spearman but there were ties and the p-value could not be calculated correctly

#Kendall test running all the data
cor.test(coocur$Abundance,coocur$Alex, method="kendall")

#Running four individual Kendall tests for each graph
lh_alex <- coocur %>% filter(Station == "HHHR2", Size_class == "Large_PN")

cor.test(lh_alex$Abundance, lh_alex$Alex, method="kendall")

KendallTauB(lh_alex$abundance,lh_alex$Alex)


sh_alex <- coocur %>% filter(Station == "HHHR2", Size_class == "Small_PN")

cor.test(sh_alex$Abundance, sh_alex$Alex, method="kendall")

KendallTauB(sh_alex$abundance,sh_alex$Alex)


lu_alex <- coocur %>% filter(Station == "UNH Pier", Size_class == "Large_PN")

cor.test(lu_alex$Abundance, lu_alex$Alex, method="kendall")

KendallTauB(lu_alex$abundance,lu_alex$Alex)


su_alex <- coocur %>% filter(Station == "UNH Pier", Size_class == "Small_PN")

cor.test(su_alex$Abundance, su_alex$Alex, method="kendall")

KendallTauB(su_alex$abundance,su_alex$Alex)


#Finding percentage of sum that the max takes up and creating graph NO TABLE DID NOT USE
maxch <- read.csv("maxch.csv", stringsAsFactors = TRUE)
sumch <- read.csv("sumch.csv", stringsAsFactors = TRUE)

smch <- cbind(sumch, maxch)

smch <- smch[, c('Year', 'Station', 'Species', 'Sum', 'Max')]

perch <- group_by(smch, Year, Station, Species) %>% mutate(Percent = Max/Sum)

write.csv(perch,'percentage_maxsum.csv', row.names = FALSE)

perch <- read.csv("percentage_maxsum.csv" , stringsAsFactors = TRUE)

ggplot(data = subset(perch, !is.na(Percent)), aes(x= Year, y = Percent)) + 
  geom_point(size = 3, stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  labs(x = "Year", y = "Max Abundance of the Annual Sum")+
  facet_grid(rows = vars(Species), cols = vars(Station))


#How many times does they appear vs how many total samples there were
##Loading in basic dataframes
hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

#column name change
colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", "Temp", "Salinity")

#Keeping specific columns
cmlc <- cml[ , c("Year", "Station", "Alex", "Large_PN", "Small_PN")]

hamptonc <- hampton[ , c("Year", "Station", "Alex", "Large_PN", "Small_PN")]

#Turning into long data
cmlc <- gather(cmlc, Species, Abundance, Alex, Large_PN, Small_PN)

hamptonc <- gather(hamptonc, Species, Abundance, Alex, Large_PN, Small_PN)

#Dropping NA values
cmlc <- cmlc %>% drop_na()

hamptonc <- hamptonc %>% drop_na()

#Counting total samplings
c_total <- cmlc %>% count(Year, Station, Species)

h_total <- hamptonc %>% count(Year, Station, Species)

#Renaming columns
colnames(c_total)<- c("Year", "Species", "Station", "Total_obs")

colnames(h_total)<- c("Year", "Species", "Station", "Total_obs")

#Counting only >0 observations
c_obs <- cmlc %>% count(Year, Station, Species, Abundance > 0)

h_obs <- hamptonc %>% count(Year, Station, Species, Abundance > 0)

#Deleting false rows
c_obs <- c_obs[c_obs$`Abundance > 0` != "FALSE", ]

h_obs <- h_obs[h_obs$`Abundance > 0` != "FALSE", ]

#Keeping Specific columns
c_obs <- c_obs[ , c("Year", "Station", "Species", "n")]

h_obs <- h_obs[ , c("Year", "Station", "Species", "n")]

#Renaming n column
colnames(c_obs)<- c("Year", "Station", "Species", "Actual_obs")

colnames(h_obs)<- c("Year", "Station", "Species", "Actual_obs")

#Joining the dataframes
tobsch <- rbind(c_total, h_total)
tobsch <- tobsch[-c(16), ]

aobsch <- rbind(c_obs, h_obs)

taobs <- cbind(tobsch, aobsch)

#Keeping specific columns and renaming
taobs <- taobs[ , c("Year", "Species", "Station", "Total_obs", "Actual_obs")]

colnames(taobs) <- c("Year", "Station", "Species", "Total_obs", "Actual_obs")

write.csv(taobs, "to_ac_obs.csv", row.names = FALSE)

taobs <- read.csv("to_ac_obs.csv" , stringsAsFactors = TRUE)

ptaobs <- group_by(taobs, Year, Station, Species) %>% mutate(Percent = Actual_obs/Total_obs)

write.csv(ptaobs, "percent_ta_obs.csv", row.names = FALSE)

ggplot(data = ptaobs, aes(x= Year, y = Percent)) + 
  geom_point(size = 3, stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  labs(x = "Year", y = "Percentage times observed of total samples")+
  facet_grid(rows = vars(Species), cols = vars(Station))












#######################Nutrient Graph Begins Here#######################

#Data frame corrections and manipulations
nutlong <- read.csv("CML_Nut_Long.csv" , stringsAsFactors = TRUE)

nutlong <- subset( nutlong, select = -c(species.sum, abundance.sum ) )

nutlong <- subset( nutlong, select = -c(silica ) )

nutlong1 <- nutlong %>%
  distinct(.keep_all = TRUE)

nutlong$species.avg <- as.character(nutlong$species.avg)

nutlong[nutlong == "Alex.avg."] <- "Alex"

nutlong[nutlong == "Large_PN.avg."] <- "Large_PN"

nutlong[nutlong == "Small_PN.avg."] <- "Small_PN"

nutlong <- nutlong[nutlong$Year != 2022, ]

nutlong <- nutlong %>%
  separate(fct_inorder.MY., sep="-", into = c("month", "Year"))

nutlong <- transform(nutlong,
                     Year = as.numeric(Year))

write.csv(nutlong,'CML_Nut_Long.csv', row.names = FALSE)

#Graphing avg against temp
alab <- expression('Average Temperature ('*degree*C*')')

ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = temperature, y = abundance.avg)) +
  geom_point(size = 2.5, shape = 21, aes(fill = Year)) +
  scale_fill_viridis() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(breaks=seq(0,24,2)) +
  theme_bw() +
  xlab(alab)+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))

#Graphing average abundance against salinity

nutlong_sal <- nutlong %>% filter(salinity >= 16)

ggplot(nutlong_sal[which(nutlong_sal$abundance.avg>0),], aes(x = salinity, y = abundance.avg)) +
  geom_point(size = 2.5, shape = 21, aes(fill = Year)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_fill_viridis() +
  scale_x_continuous(breaks=seq(12,36,2)) +
  theme_bw() +
  xlab('Average Salinity (ppt)')+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))

#Graphing against nitrogen
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = nitrogen, y = abundance.avg)) +
  geom_point(size = 2.5, shape = 21, aes(fill = Year)) +
  scale_x_continuous(limits = c(0, 0.16), breaks = seq(0, 0.16, 0.02)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_fill_viridis() +
  theme_bw() +
  xlab(bquote('Avaerage Nitrogen ' ~(mg~L^-1)))+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))

#Graphin average abundance against phosphorus
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = phosphorus, y = abundance.avg)) +
  geom_point(size = 2.5, shape = 21, aes(fill = Year)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_fill_viridis() +
  scale_x_continuous(breaks = seq(0,0.04, 0.005)) +
  theme_bw() +
  xlab(bquote('Average Phosphorus ' ~(mg~L^-1)))+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))

##Poster Graph Nitrogen:Phosphorus
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = N.P, y = abundance.avg)) +
  geom_point(size = 2.5, shape = 21, aes(fill = Year)) +
  scale_x_continuous(limits = c(0, 8), breaks = seq(0,8,1)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_fill_viridis() +
  theme_bw() +
  xlab('Nitrogen:Phosphorus')+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))

#Graphing against tss
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = tss, y = abundance.avg)) +
  geom_point(size = 2.5, shape = 21, aes(fill = Year)) +
  scale_x_continuous(limits = c(10, 24), breaks = seq(10,24,2)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_fill_viridis() +
  theme_bw() +
  xlab(bquote('Total suspended solids ' ~(mg~L^-1)))+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))

#Graphing against chla
blab <- expression('Average Chlorophyll a (' *mu*g/l*')')

ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = chla, y = abundance.avg)) +
  geom_point(size = 2.5, shape =21, aes(fill = Year)) +
  scale_x_continuous(limits = c(0,20), breaks = seq(0,20,2)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_fill_viridis() +
  theme_bw() +
  xlab(bquote('Average Chlorophyll ' ~(mu*L^-1)))+
  ylab(bquote('Log average abundance ' ~(cells~L^-1))) +
  facet_grid(rows = vars(species.avg))



#############################PLS Analysis###############################


library(pls)
library (plsVarSel)

#Testing variables for normality for PLS analysis
hist(nutlong$phosphorus) #positive skew
hist(nutlong$tss) #no/positive skew, very little data
hist(nutlong$chla) #positive skew/outlier
hist(nutlong$nitrogen) #positive skew/outlier
hist(nutlong$salinity) #negative skew
hist(nutlong$temperature) #negative skew
hist(nutlong$N.P) #normal to positive skew
hist(nutlong$abundance.avg) #outliers making no skew

#Separating into a file for each species
nuts <- read.csv("CML_Nut_Long.csv" , stringsAsFactors = TRUE)

#Alex
alexnut <- subset(nuts, species.avg == 'Alex',
                             select=c(phosphorus, chla, nitrogen, salinity, temperature,
                                      N.P, abundance.avg))

alexnut <- alexnut[-c(58,59), ] #removed since NA for abundance data

#Small_PN
smallnut <- subset(nuts, species.avg == 'Small_PN',
                  select=c(phosphorus, chla, nitrogen, salinity, temperature,
                           N.P, abundance.avg))

smallnut <- smallnut[-c(58,59),]

#Large_PN
largenut <- subset(nuts, species.avg == 'Large_PN',
                   select=c(phosphorus, chla, nitrogen, salinity, temperature,
                            N.P, abundance.avg))

largenut <- largenut[-c(58,59),]


#Taking the log of data and checking skews for Alex
alexlog <- log1p(alexnut)

hist(alexlog$phosphorus) 
hist(alexlog$chla) 
hist(alexlog$nitrogen) 
hist(alexlog$salinity) 
hist(alexlog$temperature) 
hist(alexlog$N.P) 
hist(alexlog$abundance.avg) 


#Taking the log of data and checking skews for small PN
smalllog <- log1p(smallnut)

hist(smalllog$phosphorus) 
hist(smalllog$chla) 
hist(smalllog$nitrogen) 
hist(smalllog$salinity) 
hist(smalllog$temperature) 
hist(smalllog$N.P) 
hist(smalllog$abundance.avg) 

#Taking the log of the data and checking skews for large PN
largelog <- log1p(largenut)

hist(largelog$phosphorus) 
hist(largelog$chla) 
hist(largelog$nitrogen) 
hist(largelog$salinity) 
hist(largelog$temperature) 
hist(largelog$N.P) 
hist(largelog$abundance.avg) 


#Performing the PLS & VIP for Alex
set.seed(10)

plsalex <- plsr(abundance.avg~temperature+salinity+nitrogen+phosphorus+N.P+chla, 
             na.action = na.omit, data=alexlog, scale=TRUE, validation="LOO")

summary(plsalex)

vipalex <- VIP(plsalex, opt.comp = 3)


vipalex

#Performing the PLS & VIP for Small_PN
set.seed(10)

plsSmall <- plsr(abundance.avg~temperature+salinity+nitrogen+phosphorus+N.P+chla, 
                na.action = na.omit, data=smalllog, scale=TRUE, validation="LOO")

summary(plsSmall)

vipsmall <- VIP(plsSmall, opt.comp = 3)

vipsmall

#Performing the PLS & VIP for Large_PN
set.seed(10)

plslarge <- plsr(abundance.avg~temperature+salinity+nitrogen+phosphorus+N.P+chla, 
                 na.action = na.omit, data=largelog, scale=TRUE, validation="LOO")

summary(plslarge)

viplarge <- VIP(plslarge, opt.comp = 3)

viplarge


#Follow-up regressions for abundance (of each) vs phosphorus
  #Using (species)nut dataframes created from pls analysis above

#Taking the log1p and adding new column
alexReg <- alexnut %>% 
  mutate(abunAL = log1p(abundance.avg))

smallReg <- smallnut %>% 
  mutate(abunSL = log1p(abundance.avg))

largeReg <- largenut %>% 
  mutate(abunLL = log1p(abundance.avg))

#Alex vs phosphorus
cor.test(alexReg$phosphorus, alexReg$abunAL, method="kendall")

cor.test(smallReg$phosphorus, smallReg$abunSL, method = "kendall")

cor.test(largeReg$phosphorus, largeReg$abunLL, method = "kendall")




################Generalized Linear Model Analysis##########################

help(glm)
