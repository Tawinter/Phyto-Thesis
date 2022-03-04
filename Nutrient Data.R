library(dplyr)
library(tidyr)
library(formattable)
library(forcats)

setwd("D:/R/phyto-thesis")

hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)
chlorophyll <- read.csv("PREP/portsmouthharbor_chlorophyll.csv" , stringsAsFactors = TRUE)
nitrogen <- read.csv("PREP/portsmouthharbor_nitrate_nitrite.csv" , stringsAsFactors = TRUE)
phosphorus <- read.csv("PREP/portsmouthharbor_phosphorus.csv" , stringsAsFactors = TRUE)
silica <- read.csv("PREP/portsmouthharbor_silica.csv" , stringsAsFactors = TRUE)
TSS <- read.csv("PREP/portsmouthharbor_TSS.csv" , stringsAsFactors = TRUE)

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

hamptonNut <- inner_join(hampton_lpn_s, hampton_a_s, by = c('fct_inorder(MY)'))
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
hamptonNut <- read.csv("Hampton_Nut.csv", stringsAsFactors = TRUE)
chlorophyll <- read.csv("PREP/portsmouthharbor_chlorophyll.csv" , stringsAsFactors = TRUE)
nitrogen <- read.csv("PREP/portsmouthharbor_nitrate_nitrite.csv" , stringsAsFactors = TRUE)
phosphorus <- read.csv("PREP/portsmouthharbor_phosphorus.csv" , stringsAsFactors = TRUE)
silica <- read.csv("PREP/portsmouthharbor_silica.csv" , stringsAsFactors = TRUE)
TSS <- read.csv("PREP/portsmouthharbor_TSS.csv" , stringsAsFactors = TRUE)

#Create new dataframe

chl <- cml[ , c("Month", "Year", "Alex", "Large_PN", "Small_PN")]

hampton_reduced <- hampton[ , c("Month", "Year", "Alex", "Large_PN", "Small_PN")]

#Average of chlorophyll