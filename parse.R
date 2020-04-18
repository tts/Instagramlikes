library(tidyverse)
library(jsonlite)

# Calendar Heatmap by Paul Bleicher 
# https://github.com/iascchen/VisHealth/blob/master/R/calendarHeat.R
#
# Modification: w2b color scale reversed
source("calendarHeat.R")

# From my requested Instagram data as of 2020-04-17
likes <- fromJSON("likes.json")

# Comment likes are included too
media_likes <- data.frame(likes$media_likes, stringsAsFactors = FALSE)
names(media_likes) <- c("timestamp","account")

# Whose photos have I liked the most?
media_stat <- media_likes %>% 
  group_by(account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Likes by date
media <- media_likes %>% 
  mutate(timestamp_tidy = str_replace(timestamp, "\\+00\\:00", ""),
         time = format(as.POSIXct(strptime(timestamp_tidy, format="%Y-%m-%dT%H:%M:%S")), format = "%H:%M"),
         date = as.Date(timestamp)) %>% 
  group_by(date) %>% 
  tally() %>% 
  ungroup()

# Making two sets to get only one plot at a time
older <- media %>% 
  filter(date <= "2015-12-31") %>% 
  mutate(date = as.character(date))

newer <- media %>% 
  filter(date > "2015-12-31") %>% 
  mutate(date = as.character(date))

# Exporting both as PNG 800x700 from the GUI
calendarHeat(dates = older$date, 
             values = older$n, 
             color="w2b", 
             varname="My Instagram likes")

calendarHeat(dates = newer$date, 
             values = newer$n, 
             color="w2b", 
             varname="My Instagram likes")


