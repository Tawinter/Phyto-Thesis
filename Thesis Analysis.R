#Thesis Analysis

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


############General Trend Graphs Beings Here#########################

#Q.1 Seasonal Pattern Graphs

#Refer to Month Sums.R file for data manipulation
totalch <- read.csv("totalch.csv", stringsAsFactors = TRUE)

colnames(totalch)<- c("Station", "Year", "Month", "Species", "Sum")

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

write.csv(totalch,'totalch.csv', row.names = FALSE)

#Dot plot of individual years

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

#Box and whisker of month sum, years combined using totalch.csv from previous graph
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

ggplot(chmo, aes(x = MoAb, y = Abundance)) + 
  geom_boxplot() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_discrete(limits = month.abb) +
  theme_bw() + 
  xlab('Month')+
  ylab(bquote('Log abundance of combined years '(cells~L^-1))) +
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


#Two panel graph with sum of all species and years on a single graph, locations separate
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
  geom_point(aes(fill = factor(Species)), size = 3, shape = 21) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  labs(fill = "Species") +
  xlab('Year')+
  ylab(bquote('Log max abundance '(cells~L^-1))) +
  facet_grid(cols = vars(Station))


#Are they co-occurring?

combch <- read.csv("combinedch.csv" , stringsAsFactors = TRUE)

colnames(combch)<- c("Week", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

comb <- gather(combch, size_class, abundance, Large_PN, Small_PN)

comb <- 
  comb %>%
  mutate_at("Station", str_replace, "CML", "UNH Pier")

comb <- 
  comb %>%
  mutate_at("Station", str_replace, "Hampton", "HHHR2")

write.csv(comb, "combinedch.csv", row.names = FALSE)

ggplot(combch, aes(x = abundance, y = Alex))  + 
  geom_point(size = 3) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  xlab(expression(paste("Log ", italic("Pseudo-nitzschia "), "abundance ", (cells~L^-1))))+
  ylab(expression(paste("Log ", italic("Alexandrium "), "abundance ", (cells~L^-1))))+
  facet_grid(rows = vars(size_class), cols = vars(Station))

#Filtering out zeros to get count for co occurance table
coocur <- combch %>% filter(Alex > 0, abundance > 0)

table(cooccur$Station, cooccur$size_class, cooccur$Year)

#Regression of co ocurrance

#Spearman rank correlation as the data is not normally distributed
  #Attempted a spearman but there were ties and the p-value could not be calculated correctly

#Kendall test running all the data
cor.test(coocur$abundance,coocur$Alex, method="kendall")


#Finding percentage of sum that the max takes up and creating graph NO TABLE
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
  labs(x = "Year", y = "Percentage max of sum")+
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

#Counting total observations
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


##Poster Graph Nitrogen:Phosphorus
nutlong <- read.csv("CML_Nut_Long.csv" , stringsAsFactors = TRUE)

nutlong <- nutlong %>%
  separate(fct_inorder.MY., sep="-", into = c("month", "year"))

nutlong <- transform(nutlong,
                     year = as.numeric(year))

ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = phosphorus, y = abundance.avg)) +
  geom_point(aes(fill = factor(year)), shape = 21, size = 3) +
  scale_x_continuous(limits = c(0.005, 0.045)) +
  scale_fill_manual(values = c("#440154FF", "#39568CFF", "#1F968BFF", 
                               "#3CBB75FF", "#95D840FF", "#FDE725FF")) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() +
  labs (x = "Phosphorus (mg/l)", y = "Average Abundance (Cells/l)", fill = "Year") +
  facet_grid(rows = vars(species.avg))
