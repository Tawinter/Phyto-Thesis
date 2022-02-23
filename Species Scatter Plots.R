library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)


hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Chlorophyl", "PAR", "Temp", "Salinity")

#Subsetting the data
cml_match <- subset(cml, age >= 20 | age < 10,
                  select=c(ID, Weight))

#Sum the large and small PN values and create a new column in the same dataframe
cml_sum <- cml %>% rowwise() %>%
  mutate(PnTotal = sum(c_across(Large_PN:Small_PN)))

hampton_sum <- hampton %>% rowwise() %>%
  mutate(PnTotal = sum(c_across(Large_PN:Small_PN)))

#Two graphs one for each site comparing species abundances
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
