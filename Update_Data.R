library(RODBC)
library(data.table)
library(MetroTransitr)
library(RANN)
library(sp)

#Dates ----

curr_site_list <- readRDS('data/site_list.RDS')

#Select the last date present in the table at the minimum date, user selects maximum date
curr_wc_boards <- readRDS('data/wc_boards.RDS')
prev_date_wd <- curr_wc_boards[, max(MsgDate)]
#User may change Date to the desired maximum date
Date = Sys.Date()

prev_date <- as.character(prev_date_wd)
curr_date <- as.character(Date)

prevCalID <- format(prev_date_wd, "1%Y%m%d")
currCalID <- format(Date, "1%Y%m%d")

prev_ITime <- as.ITime(curr_wc_boards[MsgDate == prev_date_wd, max(Time)])
prev_time <- paste(prev_date, as.character(prev_ITime), sep = " ")

#wc_boards ----
#Pull and format ramp messages between previous pull and date input
ch_etl <- RODBC::odbcConnect('ETL_test',  uid = getOption('odbc.uid'), pwd = getOption('odbc.pwd'))

q_all <- paste0("SELECT * FROM MT_LOGGEDMESSAGES_VW WHERE MESSAGE_TYPE_ID = 49 AND CALENDAR_ID BETWEEN ",
                prevCalID, " AND ", currCalID)

wc_all <- sqlQuery(ch_etl, q_all, stringsAsFactors = F)
setDT(wc_all)
wc_all <- wc_all[(CALENDAR_ID > prevCalID) | (CALENDAR_ID == prevCalID & REVISED_LOCAL_TIMESTAMP > prev_time)]
wc_all[, `:=`(lat = LATITUDE*1e-7, long = LONGITUDE*1e-7)]
setkey(wc_all, CALENDAR_ID, LM_ROUTE_ABBR, LM_BLOCK_ABBR, DIRECTION, STOP_OFFSET)


#Pull site information and merge with ramp cycle data
ch_tmdm <- odbcConnect('TMDataMart', uid = getOption('odbc.uid'), pwd = getOption('odbc.pwd'))

tts <- sqlQuery(ch_tmdm, 'select * from time_table_version where time_table_version_id >= 522', stringsAsFactors = FALSE)
setDT(tts)
saveRDS(tts, 'data/tts.RDS')

q_sch <- paste0(
  "select DISTINCT s.TIME_TABLE_VERSION_ID as tt_version_id, s.SERVICE_TYPE_ID as service_id, ",
  "block_abbr as block, block_stop_order as stop_offset, ",
  "s.REVENUE_ID as revenue, r.ROUTE_ABBR as route, ",
  "geo_node_abbr as site_id, g.latitude as site_lat, g.longitude as site_lon, ",
  "t.ACTIVATION_DATE, t.DEACTIVATION_DATE ",
  "from schedule s with (nolock) ",
  "inner join route r with (nolock) on s.ROUTE_ID = r.ROUTE_ID ",
  "inner join geo_node g with (nolock) on s.geo_node_id = g.geo_node_id ",
  "inner join block b with (nolock) on s.block_id = b.block_id ",
  "inner join TIME_TABLE_VERSION t with (nolock) on s.time_table_version_id = t.time_table_version_id ",
  "where calendar_id  between ", prevCalID, " and ", currCalID
)

site_sch <- sqlQuery(ch_tmdm, q_sch, stringsAsFactors = F)
setDT(site_sch)
site_sch[, ACTIVATION_DATE := as.Date(ACTIVATION_DATE)]
site_sch[, DEACTIVATION_DATE := as.Date(DEACTIVATION_DATE)]
site_sch[, site_lon := as.numeric(site_lon)]
setkey(site_sch, tt_version_id, service_id, block, stop_offset)

temp_site_list <- unique(site_sch, by = c('site_id', 'route', 'tt_version_id', 'ACTIVATION_DATE', 'DEACTIVATION_DATE'))
temp_site_list[, site_lat := site_lat*10^(-7)]
temp_site_list[, site_lon := site_lon*10^(-7)]

#Pull site ADA comments and site on/at information
ch_as <- odbcConnect("db_prod_allstop", uid = "sdreport", pwd = "tdr3port")

site_desc <- sqlQuery(ch_as, "select site_id, site_ada_accessible, site_ada_5x8_pad, site_ada_comments,
                      site_latitude, site_longitude, site_on, site_at from site", stringsAsFactors = F)
setDT(site_desc)
site_desc[, site_id := as.character(site_id)]

#Fill in for site_list
missing_geo <- temp_site_list[is.na(site_lat) | is.na(site_lon), .(site_id)]
missing_geo <- missing_geo[!(duplicated(missing_geo$site_id))]
missing_geo <- site_desc[missing_geo, .(site_id, site_latitude, site_longitude), on = 'site_id']
temp_site_list <- missing_geo[temp_site_list, on = 'site_id']
temp_site_list[is.na(site_lat), site_lat := site_latitude]
temp_site_list[is.na(site_lon), site_lon := site_longitude]
temp_site_list[, `:=` (site_latitude = NULL, site_longitude = NULL)]
temp_site_list <- temp_site_list[!(is.na(site_lat)) & !is.na(site_lon)]

site_list <- rbind(curr_site_list, temp_site_list)

site_list <- unique(site_list)

saveRDS(site_list, 'data/site_list.RDS')

site_sp <- SpatialPointsDataFrame(coords = site_list[, .(site_lon, site_lat)], data = site_list[, .(site_id, site_lon, site_lat)])

#get rid of 'incorrect' time table from most recent pick
tt_list <- unique(site_list, by = c('route', 'tt_version_id', 'ACTIVATION_DATE', 'DEACTIVATION_DATE'))
tt_list <- subset(tt_list, tt_version_id %in% tts[, TIME_TABLE_VERSION_ID])
tt_list[, num_tt := .N, by = .(tt_version_id, route)]
tt_list[, idx := .I]

tt_list <- tt_list[, .(route = route, tt_version_id = tt_version_id, num_tt, ACTIVATION_DATE, DEACTIVATION_DATE, date = seq(ACTIVATION_DATE, DEACTIVATION_DATE, by = 'day')), by = 1:nrow(tt_list)]
tt_list[, num_days := .N, by = nrow]
tt_list[num_days > 1, num_days := num_days - 1L]
tt_list[, max_date := max(date), by = nrow]
tt_list <- tt_list[num_days == 1 | (num_days > 1 & date < max_date)]


setnames(tt_list, old = c('route', 'date'), new = c('ROUTE', 'TRANSACTION_DATE'))



#Fill in missing lat long for stops
missing_geo <- site_sch[is.na(site_lat)|is.na(site_lon), .(site_id)]
missing_geo <- missing_geo[!(duplicated(missing_geo$site_id))]
missing_geo <- site_desc[missing_geo, .(site_id, site_latitude, site_longitude), on = 'site_id']
site_sch <- missing_geo[site_sch, on = 'site_id']
site_data <- missing_geo[site_sch, on = 'site_id']
site_sch[is.na(site_lat), site_lat := site_latitude]
site_sch[is.na(site_lon), site_lon := site_longitude]
site_data[is.na(site_lat), site_lat := site_latitude]
site_data[is.na(site_lon), site_lon := site_longitude]
site_data[, `:=` (site_latitude = NULL, site_longitude = NULL)]
site_data <- site_data[!(is.na(site_lat)) & !is.na(site_lon)]



#Clean stop data for associated dates
site_data <- site_data[(route < 900 | route >= 921) & !(route == 888)]
site_data[, `:=` (site_lat = site_lat*1e-7, site_lon = site_lon*1e-7)]

wc_all_id <- site_sch[wc_all[STOP_OFFSET > 1 & DIRECTION > 0], on = .(tt_version_id = TIME_TABLE_VERSION_ID,
                                                                      service_id = SERVICE_TYPE_ID, 
                                                                      block = LM_BLOCK_ABBR,
                                                                      stop_offset = STOP_OFFSET
)]

wc_all_id <- site_desc[wc_all_id, on = 'site_id']

wc_all_id[, date_key := sub("1", "", CALENDAR_ID)]
setnames(wc_all_id, c("block", "SCH_TRIP_SERIAL_NO"), c("block_number", "trip_number"))
setkey(wc_all_id, date_key, site_id, block_number, trip_number)

wc_all_id[, `:=`(site_lat = site_lat*1e-7, site_lon = site_lon*1e-7)]


#Get ridership numbers by site
channel <- odbcConnect("db_SDWarehouse", uid = getOption('odbc.uid'), pwd = getOption('odbc.pwd'))

query <- paste0("SELECT ",
                "d.site_id, ",
                "d.stopsk, ",
                "d.site_latitude, ",
                "d.site_longitude, ",
                "b.service_id, ",
                "b.block_number, ",
                "b.trip_number, ",
                "b.line_id, ",
                "b.line_direction_number, ",
                "d.start_effective_date, ",
                "a.board, ",
                "a.alight, ",
                "c.date_key, ",
                "a.sched_time ",
                "FROM ",
                "FACT_APC a ",
                "INNER JOIN DIM_TRIP_TM b on a.trip_tmsk = b.trip_tmsk ",
                "INNER JOIN DIM_DATE c on a.date_key = c.date_key ",
                "INNER JOIN DIM_STOP d on a.stopsk = d.stopsk ",
                "INNER JOIN DIM_FLAG_APC e on a.flagsk = e.flagsk ",
                "WHERE ",
                "c.date_key between ", substr(prevCalID, 2, 9), " AND ",
                substr(currCalID, 2, 9), " AND ",
                "e.flg_bad_bus != 1 AND ",
                "e.flg_badtrip != 1")

stopBoardings <- data.table(sqlQuery(channel, query, stringsAsFactors = FALSE))

setkey(stopBoardings, date_key, site_id, block_number, trip_number)

stopBoardings[, site_id := as.character(site_id)]
stopBoardings[, date_key := as.character(date_key)]

#merge boardings and alightings with ramp cycles
temp_wc_boards <- stopBoardings[wc_all_id, on = .(date_key, site_id, block_number, trip_number)]


#filter valid lat/lon points
temp_wc_boards[, c('FOM', 'dGPS', 'ValidOdo', 'ValidAdh', 'ValidLoc') := validityToFlags(validity = VALIDITY)]
temp_wc_boards <- temp_wc_boards[FOM < 10 & ValidLoc == T]

temp_wc_boards <- temp_wc_boards[(board > 0 | alight > 0)]

#Create TOD and other time variables
temp_wc_boards[, c('MsgDate', 'Time') := IDateTime(REVISED_LOCAL_TIMESTAMP)]
temp_wc_boards <- addTOD(temp_wc_boards, TODFactor = T)

temp_wc_boards[, weekday := weekdays(MsgDate)]
temp_wc_boards[weekday == "Sunday" | weekday == "Saturday", bus_or_wkend := "weekend"]
temp_wc_boards[!(weekday == "Sunday" | weekday == "Saturday"), bus_or_wkend := "weekday"]

temp_wc_boards[, monthYear := substr(REVISED_LOCAL_TIMESTAMP, 1, nchar(as.character(REVISED_LOCAL_TIMESTAMP))-17)]
temp_wc_boards[, month := month.name[month(MsgDate)]]

#Pull node sequence for each route
ch_prtr <- odbcConnect("db_prod_trapeze_current", uid = "sdreport", pwd = "tdr3port")

line_node_sequence <- sqlQuery(ch_prtr, "SELECT * FROM line_node_sequence", stringsAsFactors = F)
setDT(line_node_sequence)

line_node_sequence[, node_id := gsub(" ", "", node_id)]

line_node_sequence <- line_node_sequence[, line_dir_abbr := substr(hastus_line_direction, 1, 1)]
keycols = c("line_id", "line_direction_number", "node_sequence_number")
setkeyv(line_node_sequence, keycols)

#Create variables to identify if a node is the first/last on a line
line_node_sequence[, is_first := ifelse(node_sequence_number == 1, 1, 0)]
line_node_sequence[, is_last := node_sequence_number == max(node_sequence_number), by = .(line_id, line_direction_number)]
line_node_sequence[, is_last := ifelse(is_last == TRUE, 1, 0)]

temp_wc_boards <- merge(temp_wc_boards, line_node_sequence[, .(node_id, line_id, line_dir_abbr, is_first, is_last)], 
                        by.x = c("LM_ROUTE_ABBR", "LM_TIMEPOINT_ABBR", "LM_ROUTE_DIRECTION_ABBR"), 
                        by.y = c("line_id", "node_id", "line_dir_abbr"),
                        all.x = TRUE)


#Compute updated statistics
temp_wc_boards[, `:=` (total_cycles = NA, total_board = NA, total_alight = NA, total_activity = NA, daily_activity = NA, median_daily_cycles = NA,
                       median_daily_activity = NA, daily_cycles = NA, source = NA,
                       daily_board = NA, daily_alight = NA)]


#Remove unneeded columns
temp_wc_boards[, comments := site_ada_comments]
temp_wc_boards <- temp_wc_boards[, .(LM_ROUTE_ABBR, MsgDate, Time, TOD, monthYear, month, bus_or_wkend, weekday, LATITUDE, LONGITUDE, lat, long, site_id,
                                     site_on, site_at, site_lon, site_lat, site_ada_accessible, site_ada_5x8_pad, comments, board, alight, is_first, is_last,
                                     source, total_cycles, total_board, total_alight, total_activity, daily_cycles, daily_board, daily_alight,
                                     daily_activity, median_daily_cycles, median_daily_activity)]

new_wc_boards <- rbind(curr_wc_boards, temp_wc_boards)

new_wc_boards[, `:=` (total_cycles = NULL, total_board = NULL, total_alight = NULL, total_activity = NULL,
                      daily_cycles = NULL, daily_activity = NULL, median_daily_cycles = NULL, median_daily_activity = NULL)]

new_wc_boards[, total_cycles := .N, by = site_id]
new_wc_boards[, total_board := sum(board, na.rm = TRUE), by = site_id]
new_wc_boards[, total_alight := sum(alight, na.rm = TRUE), by = site_id]
new_wc_boards[, total_activity := total_board + total_alight]

new_wc_boards[, daily_cycles := .N, by = c('monthYear', 'site_id')]
new_wc_boards[, daily_cycles := as.numeric(daily_cycles)]
new_wc_boards[, daily_board := sum(board, na.rm = TRUE), by = c('monthYear', 'site_id')]
new_wc_boards[, daily_alight := sum(alight, na.rm = TRUE), by = c('monthYear', 'site_id')]
new_wc_boards[, daily_activity := daily_board + daily_alight]
new_wc_boards[, daily_activity := as.numeric(daily_activity)]

# new_wc_boards[, cyc_num_days := uniqueN(monthYear), by = site_id]
# new_wc_boards[, cycs_per_day :=total_cycles/cyc_num_days]
# new_wc_boards[, activity_per_day := total_activity/cyc_num_days]

new_wc_boards[, median_daily_cycles := median(daily_cycles), by = site_id]
new_wc_boards[, median_daily_activity := median(daily_activity), by = site_id]

new_wc_boards[, source := 'cyc']
#new_wc_boards[, site_id := as.character(site_id)]

saveRDS(new_wc_boards, "data/wc_boards.RDS")

#mob_card ----
#Pull mobility card data
curr_mob_card <- readRDS('data/mob_card.RDS')

prev_date_wd <- curr_mob_card[, max(TRANSACTION_DATE)]

ch_gtc <- odbcConnect('GotoCard_Usage', uid = getOption('odbc.uid'), pwd = getOption('odbc.pwd'))

q_mob <- paste0("SELECT * FROM USE_TRANSACTION WHERE TRANSACTION_DATE >= '", prev_date_wd, "' AND TRANSACTION_DATE <= '", Date,
                "' AND RIDER_CLASS = 6")
temp_mob_card <- sqlQuery(ch_gtc, q_mob, stringsAsFactors = F)
setDT(temp_mob_card)

#Remove entries with no lat/long info and overlapping entries
temp_mob_card <- temp_mob_card[!(is.na(LATITUDE)) & !(is.na(LONGITUDE))]
temp_mob_card <- temp_mob_card[TRANSACTION_DATETIME > curr_mob_card[, max(TRANSACTION_DATETIME)]]

#Remove Blue, Green, Red, Northstar
temp_mob_card[, ROUTE := as.integer(ROUTE)]
temp_mob_card <- temp_mob_card[(ROUTE < 900 | ROUTE == 921) & !(ROUTE == 888)]

#pull time table version
temp_mob_card[, TRANSACTION_DATE := as.Date(TRANSACTION_DATE)]
temp_mob_card <- tt_list[, .(tt_version_id, ROUTE, TRANSACTION_DATE)][temp_mob_card, on = c('ROUTE', 'TRANSACTION_DATE')]

#group id's for each date/route combo - needed?
temp_mob_card[, grp_id := .GRP, by = .(ROUTE, tt_version_id)]


#Find Nearest Stop

for(i in 1:max(temp_mob_card[, grp_id])){
  rte <- as.numeric(temp_mob_card[grp_id == i, ROUTE][1])
  tt_vers <- as.numeric(temp_mob_card[grp_id == i, tt_version_id][1])
  stops_temp <- site_list[route == rte & tt_version_id == tt_vers]
  stops_temp <- stops_temp[!(duplicated(stops_temp$site_id))]
  if(dim(stops_temp)[1] > 0){
    nn <- nn2(stops_temp[, .(site_lat, site_lon)], temp_mob_card[grp_id == i, .(LATITUDE, LONGITUDE)], k=1)
    temp_mob_card[grp_id == i, nearest := stops_temp[nn$nn.idx[.I,1], site_id]]
  }
}

temp_mob_card <- temp_mob_card[!(is.na(nearest))]
temp <- site_list[!(duplicated(site_list$site_id))]
temp_mob_card <- merge(temp_mob_card, temp[, .(site_id, site_lat, site_lon)], by.x = 'nearest', by.y = 'site_id', all.x = T)
temp_mob_card[, site_id := nearest]
temp_mob_card[, nearest := NULL]

#new time vars
temp_mob_card[, weekday := weekdays(as.Date(TRANSACTION_DATE))]
temp_mob_card[weekday == "Sunday" | weekday == "Saturday", bus_or_wkend := "weekend"]
temp_mob_card[!(weekday == "Sunday" | weekday == "Saturday"), bus_or_wkend := "weekday"]
temp_mob_card[, month := month.name[month(as.Date(TRANSACTION_DATE))]]

#add ADA info
temp_mob_card <- site_desc[temp_mob_card, on = 'site_id']

temp_mob_card[, c('MsgDate', 'Time') := IDateTime(TRANSACTION_DATETIME)]
temp_mob_card <- addTOD(temp_mob_card, TODFactor = T)

#TEMPORARILY remove A line
temp_mob_card <- temp_mob_card[ROUTE != 921]

temp_mob_card[, `:=` (total_tags = NA, median_daily_tags = NA, daily_tags = NA, source = NA)]

#remove unneeded variables
temp_mob_card <- temp_mob_card[, .(site_id, site_ada_accessible, site_ada_comments, site_on, site_at,
                                   ROUTE, TRANSACTION_DATETIME, TRANSACTION_DATE, TRANSACTION_TIME, LATITUDE, LONGITUDE, site_lat, site_lon,
                                   TOD, bus_or_wkend, weekday, month, total_tags, median_daily_tags, daily_tags, source)]
setnames(temp_mob_card, c('TOD', 'site_ada_comments'), c('tod', 'comments'))
#temp_mob_card[, TRANSACTION_DATE := as.Date(TRANSACTION_DATE)]

new_mob_card <- rbind(curr_mob_card, temp_mob_card)

new_mob_card[, `:=` (total_tags = NULL, median_daily_tags = NULL, daily_tags = NULL, source = NULL)]

new_mob_card[, total_tags := .N, by = site_id]
new_mob_card[, daily_tags := .N, by = c('TRANSACTION_DATE', 'site_id')]
new_mob_card[, daily_tags := as.numeric(daily_tags)]
new_mob_card[, median_daily_tags := median(daily_tags), by = site_id]

# new_mob_card[, tag_num_days := uniqueN(TRANSACTION_DATE), by = site_id]
# new_mob_card[, tags_per_day := total_tags/tag_num_days]

new_mob_card[, source := 'tag']

saveRDS(new_mob_card, 'data/mob_card.RDS')

odbcCloseAll()

rm(list = ls())


