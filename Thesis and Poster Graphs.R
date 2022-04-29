#Thesis/Poster Graphs

library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)
library(formattable)
library(forcats)
library(tidyverse)
library(viridis)
library(ggtext)
library(hexbin)

install.packages("hexbin")

#What is the seasonal pattern?
totalch <- read.csv("totalch.csv", stringsAsFactors = TRUE)

ggplot(totalch, aes(x = Month, y = sum)) + 
  geom_point(aes(fill = factor(Year)), size = 2, shape = 21) +
  scale_fill_manual(values = c("#440154FF", "#39568CFF", "#1F968BFF", 
                      "#3CBB75FF", "#95D840FF", "#FDE725FF")) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_bw() + 
  labs(x = "Month", y = "Log Sum Abundance (Cells/l)", fill = "Year") +
  facet_grid(rows = vars(species), cols = vars(Location))


#How are the populations changing over time?
hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", "Temp", "Salinity")

cml_sum <- cml %>% 
  group_by(Year) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

hampton_sum <- hampton %>% 
  group_by(Year) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

write.csv(cml_sum,'CML_Sum.csv', row.names = FALSE)
write.csv(hampton_sum,'Hampton_Sum.csv', row.names = FALSE)

sumch <- read.csv("sumch.csv", stringsAsFactors = TRUE)

sumch_1 <- gather(sumch, species, sum, Alex, Large_PN, Small_PN)

ggplot(sumch_1, aes(x = Year, y = sum))  + 
  geom_point(aes(fill = factor(species)), size = 3, shape = 21) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  labs(x = "Year", y = "Log Annual Sum (Cells/l)", fill = "Species") +
  facet_grid(cols = vars(Location))

#Are they co-occurring?
install.packages("ggtext")

combch <- read.csv("combinedch.csv" , stringsAsFactors = TRUE)

colnames(combch)<- c("Week", "Month", "Day", "Year", "Location", "Alex", "Large_PN", "Small_PN")

comb <- gather(combch, size_class, abundance, Large_PN, Small_PN)

ggplot(comb, aes(x = abundance, y = Alex))  + 
  geom_point(size = 2) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + 
  labs(x = "Log *Pseudo-nitzschia* Abundance (Cells/l)", y = "Log *Alexandrium* Abundance (Cells/l)") +
  theme(axis.title.x = ggtext::element_markdown()) +
  theme(axis.title.y = ggtext::element_markdown()) +
  facet_grid(rows = vars(size_class), cols = vars(Location))


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
