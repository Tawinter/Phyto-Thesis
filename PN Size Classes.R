library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)
library(formattable)
library(forcats)

install.packages("forcats")

setwd("D:/R/phyto-thesis")

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Chlorophyl", "PAR", "Temp", "Salinity")

#Create new dataframe

size_cml <- cml[ , c("Week", "Date", "Month", "Day", "Year", "Station", "Large_PN", 
                     "Small_PN")]

size_hampton <- hampton[ , c("Week", "Date", "Month", "Day", "Year", "Station", 
                             "Large_PN", "Small_PN")]

#Get the sum per month and create new column
size_cml_1 <- size_cml %>% 
  group_by(Year, Month) %>% 
  summarize_at(c("Large_PN", "Small_PN"), sum, na.rm = TRUE)

size_hampton_1 <- size_hampton %>% 
  group_by(Year, Month) %>% 
  summarize_at(c("Large_PN", "Small_PN"), sum, na.rm = TRUE)

#Turn into long data
size_cml_1 <- gather(size_cml_1, Class, Sum_Abundance, Large_PN:Small_PN, factor_key=TRUE)

size_hampton_1 <- gather(size_hampton_1, Class, Sum_Abundance, Large_PN:Small_PN, 
                         factor_key=TRUE)

#Convert month number to month 3 letter abbreviation
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

size_cml_1$MoAb <- mymonths[ size_cml_1$Month ]

size_hampton_1$MoAb <- mymonths[ size_hampton_1$Month ]

#Combining month and year

size_cml_1$MY <- paste(size_cml_1$MoAb, "-", size_cml_1$Year)

size_hampton_1$MY <- paste(size_hampton_1$MoAb, "-", size_hampton_1$Year)

#Remove rows that have a zero
size_cml_1 <- filter (size_cml_1, Sum_Abundance > 0.0)

size_hampton_1 <- filter (size_hampton_1, Sum_Abundance > 0.0)

#Graphing each month in each year separately on one graph

ggplot(size_cml_1, aes(x = fct_inorder(MY), y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_1, aes(x = fct_inorder(MY), y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Month", y = "Percentage of Abundance")







#(RETIRED) Adding in percentages
size_cml_pct <- size_cml_1 %>%
  group_by(Year, Month) %>%
  mutate(freq = formattable::percent(Sum_Abundance / sum(Sum_Abundance)))

size_hampton_pct <- size_hampton_1 %>%
  group_by(Year, Month) %>%
  mutate(freq = formattable::percent(Sum_Abundance / sum(Sum_Abundance)))

#(RETIRED) Fill Graphs of all
ggplot(size_cml_1, aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_1, aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

#(RETIRED) Graphs: Years on separate graphs with percentages

ggplot(size_cml_pct[which(size_cml_pct$Year== 2017),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_cml_pct[which(size_cml_pct$Year== 2018),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_cml_pct[which(size_cml_pct$Year== 2019),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_cml_pct[which(size_cml_pct$Year== 2020),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_cml_pct[which(size_cml_pct$Year== 2021),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_pct[which(size_cml_pct$Year== 2017),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_pct[which(size_cml_pct$Year== 2018),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_pct[which(size_cml_pct$Year== 2019),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_pct[which(size_cml_pct$Year== 2020),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")

ggplot(size_hampton_pct[which(size_cml_pct$Year== 2021),], aes(x = Month, y = Sum_Abundance, fill = Class)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  labs(x = "Month", y = "Percentage of Abundance")
