library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)

install.packages("viridis")
library(viridis)

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)


hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", "Temp", "Salinity")

cols <- c("2017"="red", "2018"="blue", "2019"="purple", "2020"="green", "2021"="black", "2022" = "orange")

#Grouping (by two variables), filtering, and creating a new table with multiple columns and set parameters
cml_month <- cml %>% 
  group_by(Year, Month, Station) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

hampton_month <- hampton %>% 
  group_by(Year, Month, Station) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

write.csv(hampton_month,'hampton_month.csv', row.names = FALSE)

hampton_month <- gather(hampton_month, Species, Sum, Alex:Small_PN, factor_key=TRUE)

totalch <- cml_month %>% full_join(hampton_month)

write.csv(totalch,'totalch.csv', row.names = FALSE)

#Changing following 6 graphs into a panel of 6 graphs
totalch <- read.csv("totalch.csv", stringsAsFactors = TRUE)

ggplot(totalch, aes(x = Month, y = sum, color = factor(Year)))  + 
  geom_point(size = 1.5) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_bw() + 
  labs(x = "Month", y = "Log Sum Abundance (Cells/l)", color = "Year") +
  facet_grid(rows = vars(species), cols = vars(Location))

#Graphs 6 total 3 per site and 1 per species, all years on one graph

ggplot(cml_month, mapping = aes(x = Month, y = Alex, color = Year)) + 
  geom_point(aes(color = factor(Year)), size = 3) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")

ggplot(cml_month, mapping = aes(x = Month, y = Large_PN, color = Year)) + 
  geom_point(aes(color = factor(Year)), size = 3) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")

ggplot(cml_month, mapping = aes(x = Month, y = Small_PN, color = Year)) + 
  geom_point(aes(color = factor(Year)), size = 3) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")

ggplot(hampton_month, mapping = aes(x = Month, y = Alex, color = Year)) + 
  geom_point(aes(color = factor(Year)), size = 3) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")

ggplot(hampton_month, mapping = aes(x = Month, y = Large_PN, color = Year)) + 
  geom_point(aes(color = factor(Year)), size = 3) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")

ggplot(hampton_month, mapping = aes(x = Month, y = Small_PN, color = Year)) + 
  geom_point(aes(color = factor(Year)), size = 3) +
  scale_color_manual(values = cols) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() + 
  labs(x = "Month", y = "Log Total Abundance (Cells/l)")
