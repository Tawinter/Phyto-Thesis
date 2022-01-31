library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)


hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Chlorophyl", "PAR", "Temp", "Salinity")


cml$Alex <- as.numeric(cml$Alex)
cml$Large_PN <- as.numeric(cml$Large_PN)
cml$Small_PN <- as.numeric(cml$Small_PN)

hampton$Alex <- as.numeric(hampton$Alex)
hampton$Large_PN <- as.numeric(hampton$Large_PN)
hampton$Small_PN <- as.numeric(hampton$Small_PN)

cols <- c("2017"="red", "2018"="blue", "2019"="purple", "2020"="green", "2021"="black")

#Grouping (by two variables), filtering, and creating a new table with multiple columns and set parameters
cml_month <- cml %>% 
  group_by(Year, Month) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

hampton_month <- hampton %>% 
  group_by(Year, Month) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

#Graphs 6 total 3 per site and 1 per species, all years on one graph

ggplot(cml_month, mapping = aes(x = Month, y = Alex, color = Year)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = 1:12,
                     labels = "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D") +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")
