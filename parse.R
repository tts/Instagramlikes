library(tidyverse)
library(jsonlite)

# Calendar Heatmap by Paul Bleicher 
# https://github.com/iascchen/VisHealth/blob/master/R/calendarHeat.R
#
# Modification: w2b color scale reversed
source("calendarHeat.R")

# From my requested Instagram data as of 2020-04-17
likes <- fromJSON("likes.json")
pics1 <- fromJSON("media.json")
pics2 <- fromJSON("media2.json")

# Images ---------------------------

pics1_df <- data.frame(pics1$photos, stringsAsFactors = FALSE)
pics2_df <- data.frame(pics2$photos, stringsAsFactors = FALSE)
pics <- rbind(pics1_df, pics2_df)

pics <- pics %>% 
  mutate(timestamp_tidy = str_replace(taken_at, "\\+00\\:00", ""),
         time = format(as.POSIXct(strptime(timestamp_tidy, format="%Y-%m-%dT%H:%M:%S")), format = "%H:%M"),
         date = as.Date(taken_at)) %>% 
  group_by(date) %>% 
  tally() %>% 
  ungroup()

# Making two sets to get only one plot at a time
older <- pics %>% 
  filter(date <= "2015-12-31") %>% 
  mutate(date = as.character(date))

newer <- pics %>% 
  filter(date > "2015-12-31") %>% 
  mutate(date = as.character(date))

# Exporting as PNG 800x700 from the GUI
calendarHeat(dates = older$date, 
             values = older$n, 
             color="w2b", 
             varname="My Instagram images")

calendarHeat(dates = newer$date, 
             values = newer$n, 
             color="w2b", 
             varname="My Instagram images")

# Likes ---------------------------

accounts <- fromJSON("connections.json")
accounts_df <- data.frame(accounts$following, stringsAsFactors = FALSE)

# Transpose
following <- data.frame(name = names(accounts_df), 
                        date = as.character(accounts_df[1, ]), 
                        stringsAsFactors = FALSE)

following <- following %>% 
  mutate(date_tidy = str_replace(date, "\\+00\\:00", ""),
         date = as.Date(date_tidy),
         daysfollowed = as.integer(Sys.Date() - date)) %>% 
  select(-date_tidy)

# Whose photos have I liked the most, in total
media_stat <- media_likes %>% 
  group_by(account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Totals related to how long I've followed them
media_stat_rel_time_followed <- left_join(media_stat, following, by = c("account"="name"))

media_stat_rel_time_followed <- media_stat_rel_time_followed %>% 
  mutate(prop = count/daysfollowed) %>% 
  arrange(desc(prop))

top10bytime <- head(media_stat_rel_time_followed, n=10)

# Likes by date
media <- media_likes %>% 
  mutate(timestamp_tidy = str_replace(timestamp, "\\+00\\:00", ""),
         time = format(as.POSIXct(strptime(timestamp_tidy, format="%Y-%m-%dT%H:%M:%S")), format = "%H:%M"),
         date = as.Date(timestamp)) %>% 
  group_by(date) %>% 
  tally() %>% 
  ungroup()

older <- media %>% 
  filter(date <= "2015-12-31") %>% 
  mutate(date = as.character(date))

newer <- media %>% 
  filter(date > "2015-12-31") %>% 
  mutate(date = as.character(date))

calendarHeat(dates = older$date, 
             values = older$n, 
             color="w2b", 
             varname="My Instagram likes")

calendarHeat(dates = newer$date, 
             values = newer$n, 
             color="w2b", 
             varname="My Instagram likes")
