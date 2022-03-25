library(dplyr)
library(tidyr)
library(formattable)
library(forcats)
library(tidyverse)

install.packages("tidyverse")

setwd("D:/R/phyto-thesis")

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                  "Temp", "Salinity")

#Create new dataframe

cml_reduced <- cml[ , c("Month", "Year", "Alex", "Large_PN", "Small_PN")]

hampton_reduced <- hampton[ , c("Month", "Year", "Alex", "Large_PN", "Small_PN")]

#Convert month number to month 3 letter abbreviation
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

cml_reduced$MoAb <- mymonths[ cml_reduced$Month ]

hampton_reduced$MoAb <- mymonths[ hampton_reduced$Month ]

#Concentrate Month and Year column with -
cml_reduced$MY <- paste(cml_reduced$MoAb, "-", cml_reduced$Year)

hampton_reduced$MY <- paste(hampton_reduced$MoAb, "-", hampton_reduced$Year)

#Get the sum per month and create new dataframes
cml_lpn_s <- cml_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Large_PN"), sum, na.rm = TRUE)

cml_a_s <- cml_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Alex"), sum, na.rm = TRUE)

cml_spn_s <- cml_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Small_PN"), sum, na.rm = TRUE)

hampton_lpn_s <- hampton_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Large_PN"), sum, na.rm = TRUE)

hampton_a_s <- hampton_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Alex"), sum, na.rm = TRUE)

hampton_spn_s <- hampton_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Small_PN"), sum, na.rm = TRUE)

#Join summary dataframes together
cmlnut <- inner_join(cml_a_s, cml_lpn_s, by = c('fct_inorder(MY)'))
cmlnut <- inner_join(cmlnut, cml_spn_s, by = c('fct_inorder(MY)'))

hamptonNut <- inner_join(hampton_a_s, hampton_lpn_s, by = c('fct_inorder(MY)'))
hamptonNut <- inner_join(hamptonNut, hampton_spn_s, by = c('fct_inorder(MY)'))

#Rename columns
colnames(hamptonNut)<- c("fct_inorder(MY)", "Alex(Sum)", "Large_PN(Sum)", "Small_PN(Sum)")

colnames(cmlnut)<- c("fct_inorder(MY)", "Alex(Sum)", "Large_PN(Sum)", "Small_PN(Sum)")

#Find the average per month and create new dataframes
cml_lpn_a <- cml_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Large_PN"), mean, na.rm = TRUE)

cml_a_a <- cml_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Alex"), mean, na.rm = TRUE)

cml_spn_a <- cml_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Small_PN"), mean, na.rm = TRUE)

hampton_lpn_a <- hampton_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Large_PN"), mean, na.rm = TRUE)

hampton_a_a <- hampton_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Alex"), mean, na.rm = TRUE)

hampton_spn_a <- hampton_reduced %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("Small_PN"), mean, na.rm = TRUE)

#Join averages with summaries
cmlnut <- inner_join(cmlnut, cml_a_a, by = c('fct_inorder(MY)'))
cmlnut <- inner_join(cmlnut, cml_lpn_a, by = c('fct_inorder(MY)'))
cmlnut <- inner_join(cmlnut, cml_spn_a, by = c('fct_inorder(MY)'))

hamptonNut <- inner_join(hamptonNut, hampton_a_a, by = c('fct_inorder(MY)'))
hamptonNut <- inner_join(hamptonNut, hampton_lpn_a, by = c('fct_inorder(MY)'))
hamptonNut <- inner_join(hamptonNut, hampton_spn_a, by = c('fct_inorder(MY)'))

#Rename columns
colnames(hamptonNut)<- c("fct_inorder(MY)", "Alex(Sum)", "Large_PN(Sum)", "Small_PN(Sum)", 
                         "Alex(avg)", "Large_PN(avg)", "Small_PN(avg)")

colnames(cmlnut)<- c("fct_inorder(MY)", "Alex(Sum)", "Large_PN(Sum)", "Small_PN(Sum)", 
                     "Alex(avg)", "Large_PN(avg)", "Small_PN(avg)")

#Write csv files
write.csv(cmlnut,'CML_Nut.csv', row.names = FALSE)
write.csv(hamptonNut,'Hampton_Nut.csv', row.names = FALSE)

#Read in new csv files and nutrient data
cmlnut <- read.csv("CML_Nut.csv" , stringsAsFactors = TRUE)
cml_chla <- read.csv("./PREP/cml_chla.csv", stringsAsFactors = TRUE)
cml_n <- read.csv("./PREP/cml_nitrate_nitrite.csv", stringsAsFactors = TRUE)
cml_p <- read.csv("./PREP/cml_phosphorus.csv", stringsAsFactors = TRUE)
cml_sal <- read.csv("./PREP/cml_salinity.csv", stringsAsFactors = TRUE)
cml_si <- read.csv("./PREP/cml_silica.csv", stringsAsFactors = TRUE)
cml_temp <- read.csv("./PREP/cml_temp.csv", stringsAsFactors = TRUE)
cml_tss <- read.csv("./PREP/cml_tss.csv", stringsAsFactors = TRUE)
ulb_chla <- read.csv("./PREP/grbulb_chla.csv", stringsAsFactors = TRUE)
ulb_n <- read.csv("./PREP/grbulb_Nitrate_Nitrite.csv", stringsAsFactors = TRUE)
ulb_p <- read.csv("./PREP/grbulb_phosphorus.csv", stringsAsFactors = TRUE)
ulb_sal <- read.csv("./PREP/grbulb_salinity.csv", stringsAsFactors = TRUE)
ulb_temp <- read.csv("./PREP/grbulb_temp.csv", stringsAsFactors = TRUE)

#Create new dataframe pulling out only wanted years
cfil_chla <- cml_chla %>% filter(year >= 2017)
cfil_n <- cml_n %>% filter(year >= 2017)
cfil_p <- cml_p %>% filter(year >= 2017)
cfil_sal <- cml_sal %>% filter(Year >= 2017)
cfil_si <- cml_si %>% filter(year >= 2017)
cfil_temp <- cml_temp %>% filter(Year >= 2017)
cfil_tss <- cml_tss %>% filter(year >= 2017)

#Create new dataframe keeping only wanted columns
cfil_chla <- cfil_chla[ , c("Month", "datavalue", "year")]
cfil_n <- cfil_n[ , c("Month", "datavalue", "year")]
cfil_p <- cfil_p[ , c("Month", "datavalue", "year")]
cfil_sal <- cfil_sal[ , c("Month", "datavalue", "Year")]
cfil_si <- cfil_si[ , c("Month", "datavalue", "year")]
cfil_temp <- cfil_temp[ , c("Month", "datavalue", "Year")]
cfil_tss <- cfil_tss[ , c("Month", "datavalue", "year")]
ufil_chla <- ulb_chla[ , c("Month", "datavalue", "Year")]
ufil_n <- ulb_n[ , c("Month", "datavalue", "Year")]
ufil_p <- ulb_p[ , c("Month", "datavalue", "Year")]
ufil_sal <- ulb_sal[ , c("Month", "datavalue", "Year")]
ufil_temp <- ulb_temp[ , c("Month", "datavalue", "Year")]

#Insert month abv. for month number
cfil_chla$MoAb <- mymonths[ cfil_chla$Month ]
cfil_n$MoAb <- mymonths[ cfil_n$Month ]
cfil_p$MoAb <- mymonths[ cfil_p$Month ]
cfil_sal$MoAb <- mymonths[ cfil_sal$Month ]
cfil_si$MoAb <- mymonths[ cfil_si$Month ]
cfil_temp$MoAb <- mymonths[ cfil_temp$Month ]
cfil_tss$MoAb <- mymonths[ cfil_tss$Month ]
ufil_chla$MoAb <- mymonths[ ufil_chla$Month ]
ufil_n$MoAb <- mymonths[ ufil_n$Month ]
ufil_p$MoAb <- mymonths[ ufil_p$Month ]
ufil_sal$MoAb <- mymonths[ ufil_sal$Month ]
ufil_temp$MoAb <- mymonths[ ufil_temp$Month ]

#Concentrate month abv. and year with "-"
cfil_chla$MY <- paste(cfil_chla$MoAb, "-", cfil_chla$year)
cfil_n$MY <- paste(cfil_n$MoAb, "-", cfil_n$year)
cfil_p$MY <- paste(cfil_p$MoAb, "-", cfil_p$year)
cfil_sal$MY <- paste(cfil_sal$MoAb, "-", cfil_sal$Year)
cfil_si$MY <- paste(cfil_si$MoAb, "-", cfil_si$year)
cfil_temp$MY <- paste(cfil_temp$MoAb, "-", cfil_temp$Year)
cfil_tss$MY <- paste(cfil_tss$MoAb, "-", cfil_tss$year)
ufil_chla$MY <- paste(ufil_chla$MoAb, "-", ufil_chla$Year)
ufil_n$MY <- paste(ufil_n$MoAb, "-", ufil_n$Year)
ufil_p$MY <- paste(ufil_p$MoAb, "-", ufil_p$Year)
ufil_sal$MY <- paste(ufil_sal$MoAb, "-", ufil_sal$Year)
ufil_temp$MY <- paste(ufil_temp$MoAb, "-", ufil_temp$Year)

#Combining chlorophyll a dataframes
cavg_chla <- cfil_chla %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_chla <- ufil_chla %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_chla)<- c("fct_inorder.MY.", "chla")
colnames(uavg_chla)<- c("fct_inorder.MY.", "chla")

chla <- full_join(cavg_chla, uavg_chla, by = c('fct_inorder.MY.'))

chla <- chla %>% mutate(chla = coalesce(chla.x,chla.y)) %>%
  select(`fct_inorder.MY.`, chla)

#Combining nitrogen dataframes
cavg_n <- cfil_n %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_n <- ufil_n %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_n)<- c("fct_inorder.MY.", "nitrogen")
colnames(uavg_n)<- c("fct_inorder.MY.", "nitrogen")

nitrogen <- full_join(cavg_n, uavg_n, by = c('fct_inorder.MY.'))

nitrogen <- nitrogen %>% mutate(nitrogen = coalesce(nitrogen.x,nitrogen.y)) %>%
  select(`fct_inorder.MY.`, nitrogen)

#Combining phosphorus dataframes
cavg_p <- cfil_p %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_p <- ufil_p %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_p)<- c("fct_inorder.MY.", "phosphorus")
colnames(uavg_p)<- c("fct_inorder.MY.", "phosphorus")

phosphorus <- full_join(cavg_p, uavg_p, by = c('fct_inorder.MY.'))

phosphorus <- phosphorus %>% mutate(phosphorus = coalesce(phosphorus.x,phosphorus.y)) %>%
  select(`fct_inorder.MY.`, phosphorus)

#Combining salinity dataframes
cavg_sal <- cfil_sal %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_sal <- ufil_sal %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_sal)<- c("fct_inorder.MY.", "salinity")
colnames(uavg_sal)<- c("fct_inorder.MY.", "salinity")

salinity <- full_join(cavg_sal, uavg_sal, by = c('fct_inorder.MY.'))

salinity <- salinity %>% mutate(salinity = coalesce(salinity.x,salinity.y)) %>%
  select(`fct_inorder.MY.`, salinity)

#Combining temperature dataframes
cavg_temp <- cfil_temp %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_temp <- ufil_temp %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_temp)<- c("fct_inorder.MY.", "temperature")
colnames(uavg_temp)<- c("fct_inorder.MY.", "temperature")

temperature <- full_join(cavg_temp, uavg_temp, by = c('fct_inorder.MY.'))

temperature <- temperature %>% mutate(temperature = coalesce(temperature.x,temperature.y)) %>%
  select(`fct_inorder.MY.`, temperature)

#averaging silica and tss
silica <- cfil_si %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(silica)<- c("fct_inorder.MY.", "silica")

tss <- cfil_tss %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(tss)<- c("fct_inorder.MY.", "tss")

#combing nutrient dataframes with master dataframes
cmlnut <- full_join(cmlnut, chla, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, nitrogen, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, phosphorus, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, salinity, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, temperature, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, silica, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, tss, by = c('fct_inorder.MY.'))

#Write completed nutrient csv file
write.csv(cmlnut,'CML_Nut.csv', row.names = FALSE)
