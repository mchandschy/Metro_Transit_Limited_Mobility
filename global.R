# loading library here ----

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyURL)
#library(shinydashboard)
library(data.table)
library(leaflet)
#library(MetroTransitr)
library(dplyr)
library(leaflet)
library(DT)
library(lubridate)
library(ggplot2)
library(leaflet.extras)
library(plotly)
library(RColorBrewer)
library(shinyjs)
#library(fontawesome)
library(raster)

# import/load data here ----
wc_boards <- readRDS('data/wc_boards.RDS')
mob_card <- readRDS('data/mob_card.RDS')
site_list <- readRDS('data/site_list.RDS')
site_sp <- readRDS('data/site_sp.RDS')
tts <- readRDS('data/tts.RDS')
tts <- tts[TIME_TABLE_VERSION_ID != 551]
#site_activity <- readRDS('data/site_activity.RDS')

site_list <- merge(site_list, tts[, .(TIME_TABLE_VERSION_ID, ACTIVATION_DATE, DEACTIVATION_DATE)], by.x = 'tt_version_id', by.y = 'TIME_TABLE_VERSION_ID', all.x = TRUE)
site_list[, `:=` (ACTIVATION_DATE = ACTIVATION_DATE.y, DEACTIVATION_DATE = DEACTIVATION_DATE.y, ACTIVATION_DATE.x = NULL, DEACTIVATION_DATE.x = NULL, ACTIVATION_DATE.y = NULL, DEACTIVATION_DATE.y = NULL)]
site_list <- site_list[tt_version_id %in% tts[, TIME_TABLE_VERSION_ID]]
site_list[, ACTIVATION_DATE := as.Date(ACTIVATION_DATE)]
site_list[, DEACTIVATION_DATE := as.Date(DEACTIVATION_DATE)]

unique_sites <- unique(site_list, by = 'site_id')

#compute date range of data and subset so dates match
min_date <- max(wc_boards[, min(MsgDate)], mob_card[, min(TRANSACTION_DATE)])
max_date <- as.Date('2018-12-31')


min_date_str <- format(min_date, "%Y%m%d")
max_date_str <- format(max_date, "%Y%m%d")

wc_boards <- wc_boards[MsgDate >= min_date & MsgDate <= max_date]
mob_card <- mob_card[TRANSACTION_DATE >= min_date & TRANSACTION_DATE <= max_date]

wc_boards[, site_id := as.character(site_id)]
mob_card[, site_id := as.character(site_id)]

wc_boards[, LM_ROUTE_ABBR := as.integer(LM_ROUTE_ABBR)]

site_list[ACTIVATION_DATE < min_date, ACTIVATION_DATE := min_date]
site_list <- site_list[ACTIVATION_DATE <= max_date]
site_list[DEACTIVATION_DATE > max_date, DEACTIVATION_DATE := max_date]
site_list[, act_wkday := wday(ACTIVATION_DATE)]
site_list[, deact_wkday := wday(DEACTIVATION_DATE)]
site_list[, days_active := as.numeric(DEACTIVATION_DATE - ACTIVATION_DATE)]
site_list[days_active == min(days_active), days_active := days_active + 1]

unq_site <- unique(site_list, by = c('tt_version_id', 'site_id'))
unq_site[, cyc_num_days := sum(days_active), by = site_id]
unq_site[, tag_num_days := sum(days_active), by = site_id]
unq_site[, num_active_days := sum(days_active), by = site_id]
unq_site <- unique(unq_site, by = 'site_id')

site_list <- unq_site[, .(site_id, num_active_days)][site_list, on = 'site_id']


#num_wkday <- function(day, startDate, endDate) {length(which(wday(seq(from = startDate, to = endDate - 1, by = "days")) == day))}

site_list[, num_Sun := (days_active + ((act_wkday - 2) %% 7)) %/% 7]
site_list[, num_Mon := (days_active + ((act_wkday - 3) %% 7)) %/% 7]
site_list[, num_Tue := (days_active + ((act_wkday - 4) %% 7)) %/% 7]
site_list[, num_Wed := (days_active + ((act_wkday - 5) %% 7)) %/% 7]
site_list[, num_Thu := (days_active + ((act_wkday - 6) %% 7)) %/% 7]
site_list[, num_Fri := (days_active + ((act_wkday - 7) %% 7)) %/% 7]
site_list[, num_Sat := (days_active + ((act_wkday - 8) %% 7)) %/% 7]
site_list[, num_Weekend := num_Sat + num_Sun]
site_list[, num_Weekday := num_Mon + num_Tue + num_Wed + num_Thu + num_Fri]

route_at_site <- site_list[, unique(route), by = site_id]
route_at_site <- route_at_site[, .(route_list = paste0(sort(unique(V1)), collapse = ", ")), by = site_id]
route_at_site[, route_list_2 := as.list(strsplit(route_list, ", "))]

site_list <- route_at_site[site_list, on = 'site_id']

remove(route_at_site)

unq_site <- unique(site_list, by = c('tt_version_id', 'site_id'))
unq_site[, cyc_num_days := sum(days_active), by = site_id]
unq_site[, tag_num_days := sum(days_active), by = site_id]
unq_site <- unique(unq_site, by = 'site_id')

#Adjust lat/long for these sites. They are not, in fact, in the Gulf of Guinea. Need to either automate this
#or figure out why stuff is going wrong
wc_boards[site_id == 56608, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 56609, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 56611, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 56610, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 46784, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 43699, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 40167, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 56679, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]
wc_boards[site_id == 56704, `:=` (site_lat = site_lat*(10^7), site_lon = site_lon*(10^7))]

wc_boards <- unq_site[, .(site_id, cyc_num_days, route_list, route_list_2)][wc_boards, on = 'site_id']
mob_card <- unq_site[, .(site_id, tag_num_days, route_list, route_list_2)][mob_card, on = 'site_id']

is_in <- function(x,y) {
  return (x %in% y)
}

wc_boards[, is_valid := mapply(is_in, LM_ROUTE_ABBR, route_list_2)]
wc_boards <- wc_boards[is_valid == TRUE]

mob_card[, is_valid := mapply(is_in, ROUTE, route_list_2)]
mob_card <- mob_card[is_valid == TRUE]

wc_boards[, total_cycles := .N, by = site_id]
mob_card[, total_tags := .N, by = site_id]

wc_boards[, cycs_per_day := round(total_cycles/cyc_num_days, 2)]
wc_boards[, activity_per_day := total_activity/cyc_num_days]

mob_card[, tags_per_day := round(total_tags/tag_num_days, 2)]


# temp <- unique(wc_boards, by = 'site_id')
# temp <- temp[order(-cycs_per_day)]  
# temp <- temp[1:100]
# top_100_tot <- wc_boards[site_id %in% temp$site_id]
# #top_100_tot <- top_100_tot[temp, mult = "first", on = "site_id", nomatch = 0L]
# top_100_tot <- unique(top_100_tot, by = 'site_id')
# 
# temp <- mob_card[, .(mob_tags = .N), by = site_id]
# temp <- temp[order(-mob_tags)] 
# temp <- temp[1:100]
# 
# top_100_tags <- mob_card[site_id %in% temp$site_id]
# top_100_tags <- top_100_tags[temp, mult = "first", on = "site_id", nomatch = 0L]
# 
# remove(temp)

# dat2 <- wc_boards[, .N, by = .(monthYear, TOD)]
# dat2[, Date := monthYear]
# dat2[, monthYear := as.Date(monthYear)]
# dat2[, Day := weekdays(monthYear)]
# dat2[, TOD := factor(TOD, levels = c("Early", "AM Peak", "Midday", "PM Peak", "Evening", "Owl"), ordered = T)]
# 
# dat_tag <- mob_card[, .N, by = TRANSACTION_DATE]
# dat_tag[, Date := as.character(TRANSACTION_DATE)]
# dat_tag[, Day := weekdays(TRANSACTION_DATE)]
# 
# dat_tag2 <- mob_card[, .N, by = .(TRANSACTION_DATE, tod)]
# dat_tag2[, Date := as.character(TRANSACTION_DATE)]
# dat_tag2[, Day := weekdays(TRANSACTION_DATE)]


# supporting functions here ----
get_popup <- function(input_data) {
  return(paste0("<strong> Stop ID: </strong>", input_data$site_id,
                "<br><strong> Route List: </strong>", input_data$route_list,
                "<br><strong> Site On: </strong>", input_data$site_on,
                "<br><strong> Site At: </strong>", input_data$site_at,
                "<br><strong> Number of Days: </strong>", input_data$cyc_num_days,
                "<br><strong> Average Daily Cycles: </strong>", input_data$cycs_per_day
                #"<br><strong> Average Daily Activity: </strong>", round(input_data$activity_per_day, 2)
  )
  )
}

get_popup2 <- function(input_data) {
  return(paste0("<strong> Stop ID: </strong>", input_data$site_id,
                "<br><strong> Route List: </strong>", input_data$route_list,
                "<br><strong> Site On: </strong>", input_data$site_on,
                "<br><strong> Site At: </strong>", input_data$site_at,
                "<br><strong> Number of Days: </strong>", input_data$cyc_num_days,
                "<br><strong> Average Daily Cycles: </strong>", input_data$cycs_per_day,
                #"<br><strong> Average Daily Activity: </strong>", round(input_data$activity_per_day, 2),
                "<br><strong> Number of Days (with filters applied): </strong>", input_data$cyc_num_days_sel,
                "<br><strong> Average Daily Cycles (with filters applied): </strong>", input_data$cycs_per_day_sel,
                "<br><strong> Percent of Cycles: </strong>", round((input_data$cyc_pct)*100, 2), "%"))
  #"<br><strong> Average Daily Activity (with filters applied): </strong>", round(input_data$activity_per_day_sel, 2)))
}

get_popup_tag <- function(input_data) {
  return(paste0("<strong> Stop ID: </strong>", input_data$site_id,
                "<br><strong> Route List: </strong>", input_data$route_list,
                "<br><strong> Site On: </strong>", input_data$site_on,
                "<br><strong> Site At: </strong>", input_data$site_at,
                "<br><strong> Number of Days: </strong>", input_data$tag_num_days,
                "<br><strong> Average Daily Card Tags: </strong>", input_data$tags_per_day, 2))
}

get_popup_tag2 <- function(input_data) {
  return(paste0("<strong> Stop ID: </strong>", input_data$site_id,
                "<br><strong> Route List: </strong>", input_data$route_list,
                "<br><strong> Site On: </strong>", input_data$site_on,
                "<br><strong> Site At: </strong>", input_data$site_at,
                "<br><strong> Number of Days: </strong>", input_data$tag_num_days,
                "<br><strong> Average Daily Card Tags: </strong>", input_data$tags_per_day,
                "<br><strong> Number of Days (with filters applied): </strong>", input_data$tag_num_days_sel,
                "<br><strong> Average Daily Card Tags (with filters applied) </strong>", input_data$tags_per_day_sel,
                "<br><strong> Percent of Tags: </strong>", round((input_data$tag_pct)*100, 2), "%"))
}

dummy <- data.table(Source = c('Ramp Cycles', 'Card Tags'))
pal <- colorFactor(palette = c('#ff6666', '#6699ff'), levels = c('Ramp Cycles', 'Card Tags'))
