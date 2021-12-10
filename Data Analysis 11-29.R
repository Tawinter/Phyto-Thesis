

library(ggplot2)


#Load in data
hampton <- read.csv("R_HHHR2.csv" , stringsAsFactors = TRUE)
cml <- read.csv("R_UNH_Pier.csv", stringsAsFactors = TRUE)

#change column names
colnames(hampton)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN")

colnames(cml)<- c("Week", "Date", "Month", "Day", "Year", "Station", "Alex", "Large_PN", "Small_PN", 
                       "Chlorophyl", "PAR", "Temp", "Salinity")

#Not currently being used
#hampton["Alex"][hampton["Alex"] == "0"] <- "0.01"
#hampton["Large_PN"][hampton["Large_PN"] == "0"] <- "0.01"
#hampton["Small_PN"][hampton["Small_PN"] == "0"] <- "0.01"

#cml["Alex"][cml["Alex"] == "0"] <- "0.01"
#cml["Large_PN"][cml["Large_PN"] == "0"] <- "0.01"
#cml["Small_PN"][cml["Small_PN"] == "0"] <- "0.01"

#hampton["Alex"][hampton["Alex"] == "NA"] <- "0.01"
#hampton["Large_PN"][hampton["Large_PN"] == "NA"] <- "0.01"
#hampton["Small_PN"][hampton["Small_PN"] == "NA"] <- "0.01"

#cml["Alex"][cml["Alex"] == "NA"] <- "0.01"
#cml["Large_PN"][cml["Large_PN"] == "NA"] <- "0.01"
#cml["Small_PN"][cml["Small_PN"] == "NA"] <- "0.01" 

#change species from character to numeric
cml$Alex <- as.numeric(cml$Alex)
cml$Large_PN <- as.numeric(cml$Large_PN)
cml$Small_PN <- as.numeric(cml$Small_PN)

hampton$Alex <- as.numeric(hampton$Alex)
hampton$Large_PN <- as.numeric(hampton$Large_PN)
hampton$Small_PN <- as.numeric(hampton$Small_PN)


#melt species columns together (not currently being used)
#df <- melt(df ,  id.vars = 'index', variable.name = 'series')

#plot only greater than values example code
ggplot(df[which(df$x>0),],aes(x,y))+geom_point()


#Following six are looking at log abundance by year for hampton and cml
                    
ggplot(hampton[which(hampton$Alex>0),], 
       mapping = aes(x = Year, y = Alex)) + 
  geom_point() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Abundance (Cells/l)")


ggplot(hampton[which(hampton$Large_PN>0),], 
       mapping = aes(x = Year, y = Large_PN)) + 
  geom_point() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Abundance (Cells/l)")

ggplot(hampton[which(hampton$Small_PN>0),], 
       mapping = aes(x = Year, y = Small_PN)) + 
  geom_point() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Abundance (Cells/l)")

ggplot(cml[which(cml$Alex>0),], 
       mapping = aes(x = Year, y = Alex)) + 
  geom_point() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Abundance (Cells/l)")

ggplot(cml[which(cml$Large_PN>0),], 
       mapping = aes(x = Year, y = Large_PN)) + 
  geom_point() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Abundance (Cells/l)")

ggplot(cml[which(cml$Small_PN>0),], 
       mapping = aes(x = Year, y = Small_PN)) + 
  geom_point() +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() + 
  labs(x = "Year", y = "Abundance (Cells/l)")


#Following graphs are scales used highest value and divided all values by this number to create a scale of 0 to 1

install.packages("dplyr")
library(dplyr)


#Divding column by max value and creating new column using mutate without changing NAs
hampton_scale_1 <- hampton %>% 
  mutate(alex_scaled = Alex/max(hampton$Alex, na.rm = TRUE))

hampton_scale_2 <- hampton_scale_1 %>% 
  mutate(large_PN_scaled = Large_PN/max(hampton$Large_PN, na.rm = TRUE))

hampton_scale_final <- hampton_scale_2 %>% 
  mutate(small_PN_scaled = Small_PN/max(hampton$Small_PN, na.rm = TRUE))

cml_scale_1 <- cml %>% 
  mutate(alex_scaled = Alex/max(cml$Alex, na.rm = TRUE))

cml_scale_2 <- cml_scale_1 %>% 
  mutate(large_PN_scaled = Large_PN/max(cml$Large_PN, na.rm = TRUE))

cml_scale_final <- cml_scale_2 %>% 
  mutate(small_PN_scaled = Small_PN/max(cml$Small_PN, na.rm = TRUE))

#Creating own color values
cols <- c("2017"="red", "2018"="blue", "2019"="purple", "2020"="green", "2021"="black")

#Create plots of all years on one graph but one graph per species per site

ggplot(hampton_scale_final, mapping = aes(x = Week, y = alex_scaled)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  theme_classic() + 
  labs(x = "Week of the Year", y = "Scaled Abundance (Cells/l)")

ggplot(hampton_scale_final, mapping = aes(x = Week, y = large_PN_scaled)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  theme_classic() + 
  labs(x = "Week of the Year", y = "Scaled Abundance (Cells/l)")

ggplot(hampton_scale_final, mapping = aes(x = Week, y = small_PN_scaled)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  theme_classic() + 
  labs(x = "Week of the Year", y = "Scaled Abundance (Cells/l)")

ggplot(cml_scale_final, mapping = aes(x = Week, y = alex_scaled)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  theme_classic() + 
  labs(x = "Week of the Year", y = "Scaled Abundance (Cells/l)")

ggplot(cml_scale_final, mapping = aes(x = Week, y = large_PN_scaled)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  theme_classic() + 
  labs(x = "Week of the Year", y = "Scaled Abundance (Cells/l)")

ggplot(cml_scale_final, mapping = aes(x = Week, y = small_PN_scaled)) + 
  geom_point(aes(color = factor(Year))) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  theme_classic() + 
  labs(x = "Week of the Year", y = "Scaled Abundance (Cells/l)")
