library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)


hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Chlorophyl", "PAR", "Temp", "Salinity")

#Sum the large and small PN values and create a new column in the same dataframe
cml <- cml %>% rowwise() %>%
  mutate(PnTotal = sum(c_across(Large_PN:Small_PN)))

hampton <- hampton %>% rowwise() %>%
  mutate(PnTotal = sum(c_across(Large_PN:Small_PN)))

#Plot monthly averages?


#Graphs: using weekly data, area chart, plot totals and then each size class
ggplot(cml, mapping = aes(x = PnTotal, y = Alex)) + 
  geom_area() +
  theme_classic() + 
  labs(x = "Pseudo-nitzschia Abundance (Cells/l)", y = "Alexandrium Abundance (Cells/l)")