library(dplyr)
library(tidyr)
library(formattable)
library(forcats)
library(tidyverse)
library(ggplot2)

nutrients <- read.csv("CML_Nut.csv" , stringsAsFactors = TRUE)
nutlong <- read.csv("CML_Nut_Long.csv" , stringsAsFactors = TRUE)

#Changing to long data
nutlong <- gather(nutrients, species.sum, abundance.sum, Alex.Sum., Large_PN.Sum., Small_PN.Sum.)
nutlong <- gather(nutlong, species.avg, abundance.avg, Alex.avg., Large_PN.avg., Small_PN.avg.)

write.csv(nutlong,'CML_Nut_Long.csv', row.names = FALSE)

#Separating month and year into two columns
nutlong <- nutlong %>%
  separate(fct_inorder.MY., sep="-", into = c("month", "year"))

#Changing character column to numeric column
sapply(nutlong, class)

nutlong <- transform(nutlong,
                             year = as.numeric(year))

#Graphing sum and average against temperature
alab <- expression('Temperature ('*~degree*C*')')

ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = temperature, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = alab, y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphing sum and abundance against salinity
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = salinity, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = "Salinity (ppt)", y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphin sum and abundance against phosphorus
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = phosphorus, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = "Phosphorus (mg/l)", y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphing against silica
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = silica, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic() +
  labs (x = "silica (mg/l)", y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphing against tss
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = tss, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = "Total Suspended Solids (mg/l)", y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphing against chla
ylab <- expression('Temperature ('*~degree*C*')')
blab <- expression('Chlorophyll a (' *~mu*g/l*')')

ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = chla, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(10, 20)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = blab, y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphing against nitrogen
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = nitrogen, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 0.16)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = "Nitrogen (mg/l)", y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))

#Graphing against N:P
ggplot(nutlong[which(nutlong$abundance.avg>0),], aes(x = N.P, y = abundance.avg, 
                                                     color = year)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_color_gradient(low="blue", high="red") +
  theme_classic() +
  labs (x = "Nitrogen:Phosphorus", y = "Average Abundance (Cells/l)") +
  facet_grid(rows = vars(species.avg))
