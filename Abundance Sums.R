library(ggplot2)
library(tinytex)
library(dplyr)
library(tidyr)


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


#Grouping, filtering, and creating a new table with multiple columns and set parameters
cml1 <- cml %>% 
  group_by(Year) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

hampton1 <- hampton %>% 
  group_by(Year) %>% 
  summarize_at(c("Alex", "Large_PN", "Small_PN"), sum, na.rm = TRUE)

#Changing cml1 and hampton1 to long data
cml1_long <- gather(cml1, Species, Total_Abundance, Alex:Small_PN, factor_key=TRUE)

hampton1_long <- gather(hampton1, Species, Total_Abundance, Alex:Small_PN, factor_key=TRUE)

#Graphing data
ggplot(cml1_long, mapping = aes(x = Year, y = Total_Abundance, shape = Species)) + 
  geom_point(size = 3) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Total Abundance (Cells/l)")

ggplot(hampton1_long, mapping = aes(x = Year, y = Total_Abundance, shape = Species)) + 
  geom_point(size = 3) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Total Abundance (Cells/l)")
