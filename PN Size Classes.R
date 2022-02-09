library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)


hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Chlorophyl", "PAR", "Temp", "Salinity")

#Create new dataframe

size_cml <- cml[ , c("Week", "Date", "Month", "Day", "Year", "Station", "Large_PN", "Small_PN")]

size_hampton <- hampton[ , c("Week", "Date", "Month", "Day", "Year", "Station", "Large_PN", "Small_PN")]

#Get the average per month ad create new column
size_cml_1 <- size_cml %>% 
  group_by(Year, Month) %>% 
  summarize_at(c("Large_PN", "Small_PN"), sum, na.rm = TRUE)

size_hampton_1 <- size_hampton %>% 
  group_by(Year, Month) %>% 
  summarize_at(c("Large_PN", "Small_PN"), sum, na.rm = TRUE)

#Turn into long data
size_cml_1 <- gather(size_cml_1, Class, Sum_Abundance, Large_PN:Small_PN, factor_key=TRUE)

size_hampton_1 <- gather(size_hampton_1, Class, Sum_Abundance, Large_PN:Small_PN, factor_key=TRUE)


#Graphs: using weekly data, area chart, plot totals and then each size class
ggplot(size_cml_1, aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_hampton_1, aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2017),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2018),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2019),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2020),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2021),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2017),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2018),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2019),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2020),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")

ggplot(size_cml_1[which(size_cml_1$Year== 2021),], aes(x = Month, y = Sum_Abundance))+
  geom_col(aes(fill = Class)) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme_classic() +
  labs(x = "Month", y = "Total Abundance (Cells/l)")
