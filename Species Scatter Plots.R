library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)

setwd("D:/R/phyto-thesis")

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Chlorophyl", "PAR", "Temp", "Salinity")

#Create new dataframe
cml_APN <- cml[ , c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", 
                     "Small_PN")]

hampton_APN <- hampton[ , c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", 
                             "Large_PN", "Small_PN")]

#Turn into long data
cml_1 <- gather(cml_APN, Class, Abundance, Large_PN:Small_PN, factor_key=TRUE)

hampton_1 <- gather(hampton_APN, Class, Abundance, Large_PN:Small_PN, 
                         factor_key=TRUE)

#Filtering out species that have zero in the row
cml_PN <- filter (cml_1, Alex > 0, Abundance > 0)

hampton_PN <- filter (hampton_1, Alex > 0, Abundance > 0)

#New scatter plots with 2 colored points
ggplot(cml_PN, mapping = aes(x = Abundance, y = Alex, color = Class)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() + 
  labs(x = "Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(hampton_PN, mapping = aes(x = Abundance, y = Alex, color = Class)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() + 
  labs(x = "Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")






#Scatter plots
ggplot(cml_LPN, mapping = aes(x = Large_PN, y = Alex)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Large Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(cml_SPN, mapping = aes(x = Small_PN, y = Alex)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Small Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(cml_LPN, mapping = aes(x = Large_PN, y = Alex)) + 
  geom_point() +
  scale_x_log10() +
  theme_classic() + 
  labs(x = "Large Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(cml_SPN, mapping = aes(x = Small_PN, y = Alex)) + 
  geom_point() +
  scale_x_log10() +
  theme_classic() + 
  labs(x = "Small Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(hampton_LPN, mapping = aes(x = Large_PN, y = Alex)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Large Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(hampton_SPN, mapping = aes(x = Small_PN, y = Alex)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Small Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(hampton_LPN, mapping = aes(x = Large_PN, y = Alex)) + 
  geom_point() +
  scale_x_log10() +
  theme_classic() + 
  labs(x = "Large Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(hampton_SPN, mapping = aes(x = Small_PN, y = Alex)) + 
  geom_point() +
  scale_x_log10() +
  theme_classic() + 
  labs(x = "Small Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")




#(RETIRED) Subsetting the data frame
cml_match <- subset(cml, Alex > 0 | Large_PN > 0,
                    select=c(Week, Month, Day, Year, Alex, Large_PN))

#(RETIRED) Sum the large and small PN values and create a new column in the same dataframe
cml_sum <- cml %>% rowwise() %>%
  mutate(PnTotal = sum(c_across(Large_PN:Small_PN)))

hampton_sum <- hampton %>% rowwise() %>%
  mutate(PnTotal = sum(c_across(Large_PN:Small_PN)))

#(RETIRED) Two graphs one for each site comparing species abundances
ggplot(cml, mapping = aes(x = Large_PN, y = Alex)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() + 
  labs(x = "Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")

ggplot(hampton, mapping = aes(x = PnTotal, y = Alex)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() + 
  labs(x = "Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")
