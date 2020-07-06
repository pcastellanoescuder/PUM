
load("C:/Users/mbofi/Dropbox/C5/Scripts/GitKraken/PUM/data/proc_data.RData") 

# # install.packages("HistData")
# library(HistData)
# Nightingale
# 
# Nightingale=data

library(HistData) # CRAN v0.8-6
library(tidyverse) # CRAN v1.3.0
library(lubridate) # CRAN v1.7.8
library(reshape2) # CRAN v1.4.4
install_github(wilkelab/ggtext)

#################################################################
# Using code from GitHub
# edwardgunning/FlorenceNightingale/Rose Diagram Code.R


# Full data 
Nighting_dffull <- Nightingale  %>%
  filter(Date>'1854-03-01') %>%
  mutate(Label=case_when(
    Date=='1855-01-01' ~ "JANUARY 1855",
    Date=='1855-03-01' ~ "MARCH 1855",
    Date=='1854-04-01' ~ "APRIL \n 1854",
    Date=='1856-01-01' ~ "JANUARY \n 1856", 
    TRUE ~ toupper(month.name[month(Date)])),
    Disease.rad = sqrt(Disease.rate*12/pi),
    Wounds.rad = sqrt(Wounds.rate*12/pi),
    Other.rad = sqrt(Other.rate*12/pi),
    bar1 = Other.rad,
    bar2 = ifelse(Wounds.rad>bar1, Wounds.rad-bar1, 0),
    bar3 = ifelse(Disease.rad>(bar1+bar2), Disease.rad-bar1-bar2, 0),
    labelpos = ifelse(bar1+bar2+bar3>15,bar1+bar2+bar3, 15))

Nighting_dffull$textangle = 90 - 360 * (c(1:12)-0.5) /12

Nighting_Plot <- melt(Nighting_dffull,measure.vars=c("bar1", "bar2", "bar3"), variable.name = "bar") %>%
  ggplot(aes(x=month(Date), y=value))+
  geom_bar(aes(fill=factor(bar, levels=c("bar3", "bar2", "bar1")),color=factor(bar, levels=c("bar3", "bar2", "bar1"))),stat="identity",width = 1, alpha=0.7)+
  coord_polar(start=radiansStart, clip = "off")+
  geom_text(aes(label=Label, y=labelpos, angle=textangle), color="black", size=1.8, position = "identity", vjust=-1)+
  theme_void()+ 
  labs(subtitle = " APRIL 1854 TO  MARCH 1856")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "black", hjust = 0.5, lineheight = 1.5, family="serif",margin = margin(t=0,b = -50), size=14),
        plot.margin = margin(b=-100, t=5))+
  scale_fill_manual(values=c("lightblue","tomato", "darkslategrey"))+
  scale_color_manual(values=c("lightblue","tomato", "darkslategrey"))
