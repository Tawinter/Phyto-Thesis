library(dplyr)
library(tidyr)
library(formattable)
library(forcats)
library(tidyverse)


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
ulb_tss <- read.csv("./PREP/grbulb_tss.csv", stringsAsFactors = TRUE)
sag_chla <- read.csv("./PREP/04sag_chla.csv", stringsAsFactors = TRUE)
sag_n <- read.csv("./PREP/04sag_nitrogen.csv", stringsAsFactors = TRUE)
bchn_sal<- read.csv("./PREP/bchn_salinity.csv", stringsAsFactors = TRUE)
bchn_temp <- read.csv("./PREP/bchn_temp.csv", stringsAsFactors = TRUE)
gb_n <- read.csv("./PREP/grbgb_nitrogen.csv", stringsAsFactors = TRUE)
or_chla <- read.csv("./PREP/grbor_chla.csv", stringsAsFactors = TRUE)

#Create new dataframe pulling out only wanted years
cfil_chla <- cml_chla %>% filter(year >= 2017)
cfil_n <- cml_n %>% filter(year >= 2017)
cfil_p <- cml_p %>% filter(year >= 2017)
cfil_sal <- cml_sal %>% filter(Year >= 2017)
cfil_si <- cml_si %>% filter(year >= 2017)
cfil_temp <- cml_temp %>% filter(Year >= 2017)
cfil_tss <- cml_tss %>% filter(year >= 2017)
bfil_sal <- bchn_sal %>% filter(Year >= 2017)
bfil_temp <- bchn_temp %>% filter(Year >= 2017)
gfil_n <- gb_n %>% filter(Year >= 2017)
ofil_chla <- or_chla %>% filter(Year >= 2017)

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
ufil_tss <- ulb_tss[ , c("Month", "datavalue", "Year")]
bfil_sal <- bfil_sal[ , c("Month", "datavalue", "Year")]
bfil_temp <- bfil_temp[ , c("Month", "datavalue", "Year")]
gfil_n <- gfil_n[ , c("Month", "datavalue", "Year")]
ofil_chla <- ofil_chla[ , c("Month", "datavalue", "Year")]
sfil_chla <- sag_chla[ , c("Month", "datavalue", "Year")]
sfil_n <- sag_n[ , c("Month", "datavalue", "Year")]

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
ufil_tss$MoAb <- mymonths[ ufil_tss$Month ]
sfil_chla$MoAb <- mymonths[ sfil_chla$Month ]
sfil_n$MoAb <- mymonths[ sfil_n$Month ]
bfil_sal$MoAb <- mymonths[ bfil_sal$Month ]
bfil_temp$MoAb <- mymonths[ bfil_temp$Month ]
gfil_n$MoAb <- mymonths[ gfil_n$Month ]
ofil_chla$MoAb <- mymonths[ofil_chla$Month ]

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
ufil_tss$MY <- paste(ufil_tss$MoAb, "-", ufil_tss$Year)
sfil_chla$MY <- paste(sfil_chla$MoAb, "-", sfil_chla$Year)
sfil_n$MY <- paste(sfil_n$MoAb, "-", sfil_n$Year)
bfil_sal$MY <- paste(bfil_sal$MoAb, "-", bfil_sal$Year)
bfil_temp$MY <- paste(bfil_temp$MoAb, "-", bfil_temp$Year)
gfil_n$MY <- paste(gfil_n$MoAb, "-", gfil_n$Year)
ofil_chla$MY <- paste(ofil_chla$MoAb, "-", ofil_chla$Year)

#Taking averages and combining chlorophyll a dataframes
cavg_chla <- cfil_chla %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_chla <- ufil_chla %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

savg_chla <- sfil_chla %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

oavg_chla <- ofil_chla %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_chla)<- c("fct_inorder.MY.", "chla")
colnames(uavg_chla)<- c("fct_inorder.MY.", "chla")
colnames(savg_chla)<- c("fct_inorder.MY.", "chla")
colnames(oavg_chla)<- c("fct_inorder.MY.", "chla")

uavg_chla <- uavg_chla[-c(3:7), ]
oavg_chla <- oavg_chla[-c(1:9), ]
oavg_chla <- oavg_chla[-c(2:9), ]

chla <- full_join(cavg_chla, savg_chla, by = c('fct_inorder.MY.'))

chla <- chla %>% mutate(chla = coalesce(chla.x,chla.y)) %>%
  select(`fct_inorder.MY.`, chla)

chla <- full_join(chla, uavg_chla, by = c('fct_inorder.MY.'))

chla <- chla %>% mutate(chla = coalesce(chla.x,chla.y)) %>%
  select(`fct_inorder.MY.`, chla)

chla <- full_join(chla, oavg_chla, by = c('fct_inorder.MY.'))

chla <- chla %>% mutate(chla = coalesce(chla.x,chla.y)) %>%
  select(`fct_inorder.MY.`, chla)

#Combining nitrogen dataframes
cavg_n <- cfil_n %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_n <- ufil_n %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

savg_n <- sfil_n %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

gavg_n <- gfil_n %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_n)<- c("fct_inorder.MY.", "nitrogen")
colnames(uavg_n)<- c("fct_inorder.MY.", "nitrogen")
colnames(savg_n)<- c("fct_inorder.MY.", "nitrogen")
colnames(gavg_n)<- c("fct_inorder.MY.", "nitrogen")

gavg_n <- gavg_n[-c(1:9), ]
gavg_n <- gavg_n[-c(4:9), ]

nitrogen <- full_join(cavg_n, savg_n, by = c('fct_inorder.MY.'))

nitrogen <- nitrogen %>% mutate(nitrogen = coalesce(nitrogen.x,nitrogen.y)) %>%
  select(`fct_inorder.MY.`, nitrogen)

nitrogen <- full_join(nitrogen, uavg_n, by = c('fct_inorder.MY.'))

nitrogen <- nitrogen %>% mutate(nitrogen = coalesce(nitrogen.x,nitrogen.y)) %>%
  select(`fct_inorder.MY.`, nitrogen)

nitrogen <- full_join(nitrogen, gavg_n, by = c('fct_inorder.MY.'))

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

bavg_sal <- bfil_sal %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_sal)<- c("fct_inorder.MY.", "salinity")
colnames(uavg_sal)<- c("fct_inorder.MY.", "salinity")
colnames(bavg_sal)<- c("fct_inorder.MY.", "salinity")

bavg_sal <- bavg_sal[-c(1:2), ]

uavg_sal <- uavg_sal[-c(2:5), ]
uavg_sal <- uavg_sal[-c(7:10), ]
uavg_sal <- uavg_sal[-c(13:15), ]

salinity <- full_join(cavg_sal, bavg_sal, by = c('fct_inorder.MY.'))

salinity <- salinity %>% mutate(salinity = coalesce(salinity.x,salinity.y)) %>%
  select(`fct_inorder.MY.`, salinity)

salinity <- full_join(salinity, uavg_sal, by = c('fct_inorder.MY.'))

salinity <- salinity %>% mutate(salinity = coalesce(salinity.x,salinity.y)) %>%
  select(`fct_inorder.MY.`, salinity)

max(cfil_sal$datavalue, na.rm = TRUE)
min(cfil_sal$datavalue, na.rm = TRUE)

cfil_sal <- cfil_sal %>% filter(Year == 2018)

fil_cml <- cfil_sal %>% filter(datavalue <= 15)

max(bfil_sal$datavalue, na.rm = TRUE)
min(bfil_sal$datavalue, na.rm = TRUE)

bfil_sal <- bfil_sal %>% filter(Year == 2018)

fil_bchn <- bfil_sal %>% filter(datavalue <= 15)

max(ufil_sal$datavalue, na.rm = TRUE)
min(ufil_sal$datavalue, na.rm = TRUE)

ufil_sal <- ufil_sal %>% filter(Year == 2018)

fil_gulb <- ufil_sal %>% filter(datavalue <= 15)

#Combining temperature dataframes
cavg_temp <- cfil_temp %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_temp <- ufil_temp %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

bavg_temp <- bfil_temp %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_temp)<- c("fct_inorder.MY.", "temperature")
colnames(uavg_temp)<- c("fct_inorder.MY.", "temperature")
colnames(bavg_temp)<- c("fct_inorder.MY.", "temperature")

bavg_temp <- bavg_temp[-c(1:4), ]

uavg_temp <- uavg_temp[-c(2:5), ]
uavg_temp <- uavg_temp[-c(7:9), ]
uavg_temp <- uavg_temp[-c(13:15), ]

temperature <- full_join(cavg_temp, bavg_temp, by = c('fct_inorder.MY.'))

temperature <- temperature %>% mutate(temperature = coalesce(temperature.x,temperature.y)) %>%
  select(`fct_inorder.MY.`, temperature)

temperature <- full_join(temperature, uavg_temp, by = c('fct_inorder.MY.'))

temperature <- temperature %>% mutate(temperature = coalesce(temperature.x,temperature.y)) %>%
  select(`fct_inorder.MY.`, temperature)

#Combining TSS dataframes
cavg_tss <- cfil_tss %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

uavg_tss <- ufil_tss %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(cavg_tss)<- c("fct_inorder.MY.", "tss")
colnames(uavg_tss)<- c("fct_inorder.MY.", "tss")

tss <- full_join(cavg_tss, uavg_tss, by = c('fct_inorder.MY.'))

tss <- tss %>% mutate(tss = coalesce(tss.x,tss.y)) %>%
  select(`fct_inorder.MY.`, tss)

#averaging silica
silica <- cfil_si %>% 
  group_by(fct_inorder(MY)) %>% 
  summarize_at(c("datavalue"), mean, na.rm = TRUE)

colnames(silica)<- c("fct_inorder.MY.", "silica")

#combing nutrient dataframes with master dataframes
cmlnut <- full_join(cmlnut, chla, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, nitrogen, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, phosphorus, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, salinity, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, temperature, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, silica, by = c('fct_inorder.MY.'))
cmlnut <- full_join(cmlnut, tss, by = c('fct_inorder.MY.'))

#Correcting cmlnut dataframe for new column additions
cmlnut <- subset( cmlnut, select = -tss.x )
cmlnut <- subset( cmlnut, select = -chla.x )
cmlnut <- subset( cmlnut, select = -nitrogen.x )
cmlnut <- subset( cmlnut, select = -salinity.x )
cmlnut <- subset( cmlnut, select = -temperature.x )

#Correcting column names from new column additions to cmlnut dataframe
colnames(cmlnut)<- c("fct_inorder.MY.", "Alex(Sum)", "Large_PN(Sum)", "Small_PN(Sum)", 
                     "Alex(avg)", "Large_PN(avg)", "Small_PN(avg)", "phosphorus", "silica", "tss", "chla",
                     "nitrogen", "salinity", "temperature")

#Creating N:P column
cmlnut <- read.csv("CML_Nut.csv", stringsAsFactors = TRUE)
cmlnut$'N:P' <- cmlnut$nitrogen / cmlnut$phosphorus

#Write completed nutrient csv file
write.csv(cmlnut,'CML_Nut.csv', row.names = FALSE)
