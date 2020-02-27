# By Eka Tian
# 28/Feb/2020

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('geofacet')) install.packages('geofacet'); library('geofacet')
if (!require('ggtext')) install.packages('ggtext'); library('ggtext')
if (!require('hrbrthemes')) install.packages('hrbrthemes'); library('hrbrthemes')
if (!require('extrafont')) install.packages('extrafont'); library('extrafont')

#load libraries
library(tidyverse)
library(geofacet)
library(hrbrthemes)
library(extrafont)
library(skimr)
library (dplyr)



# Download the data
measles <- readr::read_csv('C:/Users/ETian/Documents/Kedro Projects/TidyTuesday/data/measles.csv')

measles_df3 <- measles %>%
  filter(mmr >0) %>% #removed schools with no reported value (-1)
  mutate(xrel = as.numeric(xrel))%>% # it is showing up as a logical instead of a value
  replace_na(list(xmed = 0, xrel = 0, xper = 0, enroll = 0, overall = 0, mmr = 0)) %>%# replace all NA with 0
  group_by(type,state) %>% # group for visualisation
  summarise(enrollments = sum(enroll), avg_mmr = mean(mmr) ) %>%
  filter(type %in% list('Private','Public'))

#bar-chart private vs public
glimpse(measles_df3)
ggplot(measles_df3,aes(x = type, y = avg_mmr, colour = state, fill = state, label=state))+
  geom_bar(position = "dodge", stat="identity" ) 