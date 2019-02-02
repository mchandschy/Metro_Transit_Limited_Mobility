library(data.table)
library(MetroTransitr)
library(RODBC)
library(BAMMtools)

#get average boardings and alightings
ch_apc <- odbcConnect('db_prod_apc_poc', uid = getOption('odbc.uid'), pwd = getOption('odbc.pwd'))
cols_apc <- describeDB(ch_apc)
site_activity <- sqlQuery(ch_apc, "select * from t_apc_pick_summary where pick_start_date >= '2017-06-17' and service_id = 'wk'")
setDT(site_activity)
odbcClose(ch_apc)
num_picks <- site_activity[, uniqueN(pick_start_date)]
site_activity[, site_id := as.character(site_id)]
site_activity[, sum_on := sum(on_avg), by = 'site_id']
site_activity[, sum_off := sum(off_avg), by = 'site_id']
site_activity[, avg_boards := sum_on/num_picks]
site_activity[, avg_alights := sum_off/num_picks]
site_activity <- site_activity[, .(site_id, avg_boards, avg_alights)]
site_activity <- unique(site_activity, by = 'site_id')
saveRDS(site_activity, 'data/site_activity.RDS')

#pull list of active shelters
ch_as <- odbcConnect('db_prod_allstop', uid = "sdreport", pwd = "tdr3port")
cols_as <- describeDB(ch_as)

active_shelters <- sqlQuery(ch_as, 'select * from active_shelter')
setDT(active_shelters)

#v_gis_shelt <- sqlQuery(ch_as, 'select * from v_gis_shelter')
#setDT(v_gis_shelt)

#site <- sqlQuery(ch_as, 'select * from site')
#setDT(site)
#site[, site_id := as.character(site_id)]
close(ch_as)

#May need to pull active shelter info from a different source
#Pull 'active_stops.csv' from the mobility app. Select all stops, NOT top 100
site_data <- read.csv('active_stops.csv', stringsAsFactors = FALSE)
setDT(site_data)
setnames(site_data, old = c("V8", "V11"), new = c('cyc_pct', 'tag_pct'))

site_activity <- site_data[, .(site_id, cycs_per_day, tags_per_day)][site_activity, on = 'site_id']
rm(site_data)

prioritize <- site_activity[!(site_id %in% active_shelters[, site_id])]
rm(site_activity)
prioritize[is.na(cycs_per_day), cycs_per_day := 0]
prioritize[is.na(tags_per_day), tags_per_day := 0]

#getJenksBreaks - choose k = 4, i.e. 5 clusters - see plots of sum of squared errors below
boards_Jenks <- getJenksBreaks(prioritize[, avg_boards], k = 5)
alights_Jenks <- getJenksBreaks(prioritize[, avg_alights], k = 5)
cycs_Jenks <- getJenksBreaks(prioritize[, cycs_per_day], k=5)
tags_Jenks <- getJenksBreaks(prioritize[, tags_per_day], k=5)

prioritize[avg_boards >= min(avg_boards) & avg_boards <= boards_Jenks[2], board_score := 0]
prioritize[avg_boards > boards_Jenks[2] & avg_boards <= boards_Jenks[3], board_score := 1]
prioritize[avg_boards > boards_Jenks[3] & avg_boards <= boards_Jenks[4], board_score := 2]
prioritize[avg_boards > boards_Jenks[4] & avg_boards <= max(avg_boards), board_score := 3]

prioritize[avg_alights >= min(avg_alights) & avg_alights <= alights_Jenks[2], alight_score := 0]
prioritize[avg_alights > alights_Jenks[2] & avg_alights <= alights_Jenks[3], alight_score := 1]
prioritize[avg_alights > alights_Jenks[3] & avg_alights <= alights_Jenks[4], alight_score := 2]
prioritize[avg_alights > alights_Jenks[4] & avg_alights <= max(avg_alights), alight_score := 3]

prioritize[cycs_per_day >= min(cycs_per_day) & cycs_per_day <= boards_Jenks[2], cyc_score := 0]
prioritize[cycs_per_day > cycs_Jenks[2] & cycs_per_day <= cycs_Jenks[3], cyc_score := 1]
prioritize[cycs_per_day > cycs_Jenks[3] & cycs_per_day <= cycs_Jenks[4], cyc_score := 2]
prioritize[cycs_per_day > cycs_Jenks[4] & cycs_per_day <= max(cycs_per_day), cyc_score := 3]

prioritize[tags_per_day >= min(tags_per_day) & tags_per_day <= tags_Jenks[2], tag_score := 0]
prioritize[tags_per_day > tags_Jenks[2] & tags_per_day <= tags_Jenks[3], tag_score := 1]
prioritize[tags_per_day > tags_Jenks[3] & tags_per_day <= tags_Jenks[4], tag_score := 2]
prioritize[tags_per_day > tags_Jenks[4] & tags_per_day <= max(tags_per_day), tag_score := 3]

prioritize[, snow_score := board_score + alight_score + 2*cyc_score + 2*tag_score]


sse_list <- list()
sse <- 0

#elbow plot
for (i in 2:20) {
  print(i)
  jenks <- getJenksBreaks(prioritize[, avg_boards], k = i)
  for (j in 1:i-1) {
    mean <- prioritize[avg_boards >= jenks[j] & avg_boards <= jenks[j+1], mean(avg_boards)]
    sse <- sse + prioritize[avg_boards >= jenks[j] & avg_boards <= jenks[j+1], sum((avg_boards - mean)^2)]
  }
  sse_list[i-1] <- sse
  sse <- 0
}

plot(c(2:20), sse_list)

sse_list <- list()
sse <- 0

#elbow plot
for (i in 2:20) {
  print(i)
  jenks <- getJenksBreaks(prioritize[, avg_alights], k = i)
  for (j in 1:i-1) {
    mean <- prioritize[avg_alights >= jenks[j] & avg_alights <= jenks[j+1], mean(avg_alights)]
    sse <- sse + prioritize[avg_alights >= jenks[j] & avg_alights <= jenks[j+1], sum((avg_alights - mean)^2)]
  }
  sse_list[i-1] <- sse
  sse <- 0
}

plot(c(2:20), sse_list)

sse_list <- list()
sse <- 0

#elbow plot
for (i in 2:20) {
  print(i)
  jenks <- getJenksBreaks(prioritize[, cycs_per_day], k = i)
  for (j in 1:i+1) {
    mean <- prioritize[cycs_per_day >= jenks[j] & cycs_per_day <= jenks[j+1], mean(cycs_per_day)]
    sse <- sse + prioritize[cycs_per_day >= jenks[j] & cycs_per_day <= jenks[j+1], sum((cycs_per_day - mean)^2)]
  }
  sse_list[i-1] <- sse
  sse <- 0
}

plot(c(2:20), sse_list)

sse_list <- list()
sse <- 0

#elbow plot
for (i in 2:20) {
  print(i)
  jenks <- getJenksBreaks(prioritize[, tags_per_day], k = i)
  for (j in 1:i+1) {
    mean <- prioritize[tags_per_day >= jenks[j] & tags_per_day <= jenks[j+1], mean(tags_per_day)]
    sse <- sse + prioritize[tags_per_day >= jenks[j] & tags_per_day <= jenks[j+1], sum((tags_per_day - mean)^2)]
  }
  sse_list[i-1] <- sse
  sse <- 0
}

plot(c(2:20), sse_list)