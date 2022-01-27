library(ggplot2)
library(tinytex)
library(dplyr)

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

##cml %>% filter(Year == "2021") %>% summarise(sum(Alex))

cml1 <- cml %>% 
  group_by(Year) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

