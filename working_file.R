# By Nyssa Silbiger
# 2/25/2020

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('geofacet')) install.packages('geofacet'); library('geofacet')
if (!require('ggtext')) install.packages('ggtext'); library('ggtext')
if (!require('hrbrthemes')) install.packages('hrbrthemes'); library('hrbrthemes')
if (!require('extrafont')) install.packages('extrafont'); library('extrafont')

#load libraries
library(tidyverse)
library(geofacet)
library(ggtext)
library(hrbrthemes)
library(extrafont)
library(skimr)

help(package = "tidyverse")

# Download the data
measles <- readr::read_csv('C:/Users/ETian/Documents/Kedro Projects/TidyTuesday/data/measles.csv')

skimr::skim(measles)
summary(measles)

## get percentage of students that did not get the mmr shot for religious, medical, or personal reasons accross all states
measles_df <- measles %>%
  filter(mmr >0) %>% #removed schools with no reported value (-1)
  mutate(xrel = as.numeric(xrel))%>% # it is showing up as a logical instead of a value
  replace_na(list(xmed = 0, xrel = 0, xper = 0, enroll = 0, overall = 0, mmr = 0))%>% # replace all NA with 0
  select(-lat, -lng, -index, -enroll, -overall) %>% #remove lat and long
  pivot_longer(cols = xrel:xper, names_to = "Reason", values_to = "percentage") %>% # have all the reasons together for the barplot below
  group_by(state, Reason) %>%
  summarise_if(is.numeric, list(~mean(.), ~sd(.)/sqrt(n()))) %>% # get means and SE
  rename(SE = `percentage_/`)

glimpse(measles_df)
# rename the SE column
ggplot(measles_df,aes(x = Reason, y = percentage_mean, color = Reason, fill = Reason))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin = percentage_mean - SE, ymax = percentage_mean+SE), color = "white", width = 0)+
  theme_ft_rc() + # black background theme
  labs(caption = "Plot by N. Silbiger \n@nsilbiger \nData by the Wallstreet Journal")+
  theme(axis.title.x=element_blank(),  # remove xlabels
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #plot.title = element_markdown(lineheight = 1.1)
        
        legend.position = "none")+
  xlab("")+ylab("")+
  labs(title = "Mean prcentage of students that refused vaccines due to  medical , personal , or  religious")+
  facet_geo(~ state)+ # facet wrap it by state
  ggsave('C:/Users/ETian/Documents/Kedro Projects/TidyTuesday/data/measlesplot.png', width = 18, height = 10)

measles_df2 <- measles %>%
  filter(mmr >0) %>% #removed schools with no reported value (-1)
  mutate(xrel = as.numeric(xrel))%>% # it is showing up as a logical instead of a value
  replace_na(list(xmed = 0, xrel = 0, xper = 0, enroll = 0, overall = 0, mmr = 0)) # replace all NA with 0
glimpse(measles_df2)
ggplot(measles_df2,aes(x = enroll, y = mmr, colour = state, fill = state))+
  geom_point( )


measles_df3 <- measles %>%
  filter(mmr >0) %>% #removed schools with no reported value (-1)
  mutate(xrel = as.numeric(xrel))%>% # it is showing up as a logical instead of a value
  replace_na(list(xmed = 0, xrel = 0, xper = 0, enroll = 0, overall = 0, mmr = 0)) %>%# replace all NA with 0
  group_by(type,state) %>% # group for visualisation
  summarise(enrollments = sum(enroll), avg_mmr = mean(mmr) ) %>%
  filter(type %in% list('Private','Public'))

glimpse(measles_df3)
ggplot(measles_df3,aes(x = enrollments, y = avg_mmr, colour = state, fill = state, label=state))+
  geom_point( ) +
  geom_text(angle = 45)
ggplot(measles_df3,aes(x = type, y = avg_mmr, colour = state, fill = state, label=state))+
  geom_bar(position = "dodge", stat="identity" ) #+
#    geom_text(angle = 45)