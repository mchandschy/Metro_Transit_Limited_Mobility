shinyServer(function(input, output, session) {
  # user_info <- readRDS("data/user_info.RDS")
  # startTime <- Sys.time()
  # user <- ""
  # 
  # observe({
  #   user <<- input$email
  # })
  # 
  # toggleModal(session, "startupModal", toggle = "open")
  # 
  # session$onSessionEnded(function() {
  #   newdata <- data.table(user = user, start_time = as.character(startTime), end_time = as.character(Sys.time()))
  #   user_info <<- rbind(user_info, newdata)
  #   saveRDS(user_info, "data/user_info.RDS")
  #   stopApp()
  # })
  
  ## KK1 - lines 2:17 should be removed because this feature is not being used in this app
  
  shinyURL.server(session)
  #Select Stops ----------------
  csv_up_stops <- reactive({
    inFile <- input$stops_csv
    
    if(is.null(inFile)) {return(NULL)}
    
    tmp <- read.csv(inFile$datapath, header = FALSE)
    tmp <- setDT(tmp)
    tmp[, V1 := as.character(V1)]
    
    tmp
  })
  
  output$up_stops <- renderTable({
    csv_up_stops()
  })
  
  rv <- reactiveValues(bbox = NULL, sp_box = NULL)
  
  output$geo_stops <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
               under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
               Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
               <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>%
      setView(-93.2125, 44.9675, zoom = 11) %>%
      addCircleMarkers(data = unique(site_list, by = 'site_id'), lng = ~site_lon, lat = ~site_lat, radius = 3, stroke = FALSE, fillOpacity = .9) %>%
      addDrawToolbar(polylineOptions = F, circleOptions = F, rectangleOptions = F, markerOptions = F, 
                     circleMarkerOptions =F, 
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  })
  
  observeEvent(input$geo_stops_draw_new_feature, {
    newfeature <- input$geo_stops_draw_new_feature
    box_coords <- unlist(newfeature$geometry$coordinates)
    rv$bbox <- box_coords
  }, ignoreInit = TRUE)
  
  observeEvent(input$geo_stops_draw_edited_features, {
    editedfeature <- input$geo_stops_draw_edited_features$features[[1]]
    if (is.null(editedfeature)) return()
    box_coords <- unlist(editedfeature$geometry$coordinates)
    rv$bbox <- box_coords
  }, ignoreInit = TRUE)
  
  observe({
    req(!is.null(rv$bbox))
    bbox_mat <- matrix(rv$bbox, ncol = 2, byrow = TRUE)
    sp_box <- SpatialPolygons(list(Polygons(list(sp::Polygon(bbox_mat)), ID = "a")))
    proj4string(sp_box) <- CRS('+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')
    rv$sp_box <- sp_box
  })
  
  geo_sel_stops <- reactive({
    req(rv$sp_box)
    sp_box <- rv$sp_box
    site_in_box <- site_sp[sp_box,]
    site_in_box <- data.table(V1 = site_in_box$site_id)
    site_in_box[, V1 := as.character(V1)]
    site_in_box
  })
  
  output$geo_stops_list <- renderTable({
    geo_sel_stops()[order(V1)][, unique(V1)]
  })
  
  
  outRoute <- reactive({
    if ('list_stops_sel' %in% input$selector_type & !is.null(input$stops_csv)) {
      subset_route_list <- subset(site_list, site_id %in% csv_up_stops()[, V1])[order(route)][, unique(route)]
      subset_route_list
    } else if ('geo_stops_sel' %in% input$selector_type & !is.null(input$geo_stops_draw_new_feature)) {
      subset_route_list <- subset(site_list, site_id %in% geo_sel_stops()[, V1])[order(route)][, unique(route)]
      subset_route_list
    } else {
      subset_route_list <- site_list[order(route)][, unique(route)]
      subset_route_list
    }
  })
  
  observe({
    updateSelectInput(session, 'route', choices = c("All Routes" = 'all_routes', outRoute()), selected = 'all_routes')
  })
  
  observe({
    updateSelectInput(session, 'route_trend', choices = c("All Routes" = 'all_routes_trend', outRoute()), selected = 'all_routes_trend')
  })
  
  observe({
    updateSelectInput(session, 'route_activity', choices = c("All Routes" = 'all_routes_activity', outRoute()), selected = 'all_routes_activity')
  })
  
  av_lat <- reactive({
    if('all_stops_sel' %in% input$selector_type) {
      sites_wc <- wc_boards[, unique(site_id)]
      sites_mob <- unique(mob_card[, unique(site_id)])
    } else if ('list_stops_sel' %in% input$selector_type) {
      sites_wc <- subset(wc_boards, site_id %in% csv_up_stops()[, V1])[, unique(site_id)]
      sites_mob <- subset(mob_card, site_id %in% csv_up_stops()[, V1])[, unique(site_id)]
    } else if ('geo_stops_sel' %in% input$selector_type) {
      sites_wc <- subset(wc_boards, site_id %in% geo_sel_stops()[, V1])[, unique(site_id)]
      sites_mob <- subset(mob_card, site_id %in% geo_sel_stops()[, V1])[, unique(site_id)]
    }
    sites <- subset(unique_sites[, .(site_id, site_lat)], site_id %in% sites_wc | site_id %in% sites_mob)
    sites[, mean(site_lat)]
  })
  
  av_lng <- reactive({
    if('all_stops_sel' %in% input$selector_type) {
      sites_wc <- wc_boards[, unique(site_id)]
      sites_mob <- unique(mob_card[, unique(site_id)])
    } else if ('list_stops_sel' %in% input$selector_type) {
      sites_wc <- subset(wc_boards, site_id %in% csv_up_stops()[, V1])[, unique(site_id)]
      sites_mob <- subset(mob_card, site_id %in% csv_up_stops()[, V1])[, unique(site_id)]
    } else if ('geo_stops_sel' %in% input$selector_type) {
      sites_wc <- subset(wc_boards, site_id %in% geo_sel_stops()[, V1])[, unique(site_id)]
      sites_mob <- subset(mob_card, site_id %in% geo_sel_stops()[, V1])[, unique(site_id)]
    }
    sites <- subset(unique_sites[, .(site_id, site_lon)], site_id %in% sites_wc | site_id %in% sites_mob)
    sites[, mean(site_lon)]
  })
  
  # Raw Data Maps -------------------------------------------------------------
  
  # Creates reactivity user-selected choices including: route number, time of day, date range
  
  map_data <- reactive({
    site_input <- input$selector_type
    route_input <- input$route
    time_input <- input$msg_time
    day_input <- input$day
    month_input <- input$month
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    if('all_stops_sel' %in% site_input) {
      site_list_wc <- unique_sites[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      site_list_wc <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      site_list_wc <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id]
    }
    
    if('all_routes' %in% route_input){
      route_list_wc <- wc_boards[, unique(LM_ROUTE_ABBR)]
    } else {
      route_list_wc <- route_input
    }
    
    if('all_times' %in% time_input){
      time_list <- wc_boards[, unique(TOD)]
    } else {
      time_list <- time_input
    }
    
    if('all_days' %in% day_input){
      day_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    } else {
      day_list <- day_input
    }
    
    if('all_months' %in% month_input){
      month_list <- wc_boards[, unique(month)]
    } else {
      month_list <- month_input
    }
    
    subset(wc_boards, site_id %in% site_list_wc & LM_ROUTE_ABBR %in% route_list_wc & TOD %in% time_list 
           & MsgDate >= start_date & MsgDate <= end_date & weekday %in% day_list
           & month %in% month_list)
  })
  
  map_data_tag <- reactive({
    site_input <- input$selector_type
    route_input <- input$route
    time_input <- input$msg_time
    day_input <- input$day
    month_input <- input$month
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    if('all_stops_sel' %in% site_input) {
      site_list_tag <- unique_sites[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      site_list_tag <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      site_list_tag <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id]
    }
    
    if('all_routes' %in% route_input){
      route_list_tag <- mob_card[, unique(ROUTE)]
    } else {
      route_list_tag <- route_input
    }
    
    if('all_times' %in% time_input){
      time_list_tag <- mob_card[, unique(tod)]
    } else {
      time_list_tag <- time_input
    }
    
    if('all_days' %in% day_input){
      day_list_tag <- mob_card[, unique(weekday)]
    } else if('weekend' %in% day_input){
      day_list_tag <- mob_card[bus_or_wkend == 'weekend', unique(weekday)]
    } else if('weekday' %in% day_input){
      day_list_tag <- mob_card[bus_or_wkend == 'weekday', unique(weekday)]
    } else {
      day_list_tag <- day_input
    }
    
    if('all_months' %in% month_input){
      month_list_tag <- mob_card[, unique(month)]
    } else {
      month_list_tag <- month_input
    }
    
    subset(mob_card, site_id %in% site_list_tag & ROUTE %in% route_list_tag & tod %in% time_list_tag 
           & TRANSACTION_DATE >= start_date & TRANSACTION_DATE <= end_date & weekday %in% day_list_tag
           & month %in% month_list_tag)
  })
  
  
  output$raw_map <- renderLeaflet({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    leaflet() %>%
      addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
               under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
               Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
               <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>%
      setView(av_lng(), av_lat(), zoom = 11) %>%
      addHeatmap(data = map_data(), lng = ~long, lat = ~lat, intensity = NULL, radius = 3, blur = 5, group = "all")
  })
  
  observeEvent(c(input$seletor$type, input$plot_type, input$data_source, input$route, input$msg_time, input$date_range, input$day, input$month), {
    
    
    raw_proxy <- leafletProxy('raw_map')
    
    if (input$plot_type == 'circ' & input$data_source == 'acc_ramp_cyc') {
      raw_proxy %>% clearGroup(group = 'all') %>%
        addCircleMarkers(data = map_data(), lng = ~long, lat = ~lat, radius = 6,
                         clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 1.2,
                                                               maxClusterRadius = 40), group = 'all')
    } else if (input$plot_type == 'circ' & input$data_source == 'mob_card_tags') {
      raw_proxy %>% clearGroup(group = 'all') %>%
        addCircleMarkers(data = map_data_tag(), lng = ~LONGITUDE, lat = ~LATITUDE, radius = 6,
                         clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 1.2,
                                                               maxClusterRadius = 40), group = 'all')
    } else if (input$plot_type == 'heat' & input$data_source == 'acc_ramp_cyc') {
      raw_proxy %>% clearGroup(group = 'all') %>%
        addHeatmap(data = map_data(), lng = ~long, lat = ~lat, intensity = NULL, radius = 3, blur = 5, group = "all")
    } else {
      raw_proxy %>% clearGroup(group = 'all') %>%
        addHeatmap(data = map_data_tag(), lng = ~LONGITUDE, lat = ~LATITUDE, intensity = NULL, radius = 3, blur = 5, group = 'all')
    }
  })  
  
  # Changes the printed list of routes selected 
  output$routesSelected <- renderText({
    if ('all_routes' %in% input$route) {
      c('Route(s) Selected: All')
    } else {
      c('Route(s) Selected:', paste(input$route, collapse = ", "))
    }
  })
  
  #Trends Tab -----------------------------------------------------------------
  trend_data <- reactive({
    site_input <- input$selector_type
    trend_list <- input$trend_tod
    route_list <- input$route_trend
    
    if('all_stops_sel' %in% site_input) {
      site_list_wc <- unique_sites[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      site_list_wc <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      site_list_wc <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id]
    }
    
    temp <- wc_boards[site_id %in% site_list_wc, .N, by = .(monthYear, TOD, LM_ROUTE_ABBR)]
    temp[, Date := monthYear]
    temp[, monthYear := as.Date(monthYear)]
    temp[, Day := weekdays(monthYear)]
    
    if ('all_tod' %in% trend_list & 'all_routes_trend' %in% route_list) {
      temp[, N := sum(N), by = monthYear]
      temp <- unique(temp, by = 'monthYear')
      temp
    } else if ('all_tod' %in% trend_list) {
      temp <- temp[LM_ROUTE_ABBR %in% route_list]
      temp[, N := sum(N), by = .(monthYear)]
      temp <- unique(temp, by = 'monthYear')
      temp
    } else if ('all_routes_trend' %in% route_list) {
      temp <- temp[TOD %in% trend_list]
      temp[, N := sum(N), by = .(monthYear)]
      temp <- unique(temp, by = 'monthYear')
      temp
    } else {
      temp <- temp[TOD %in% trend_list & LM_ROUTE_ABBR %in% route_list]
      temp[, N := sum(N), by = .(monthYear)]
      temp <- unique(temp, by = 'monthYear')
      temp
    }
  })
  
  mob_trend_data <- reactive({
    site_input <- input$selector_type
    trend_list <- input$trend_tod
    route_list <- input$route_trend
    
    if('all_stops_sel' %in% site_input) {
      site_list_mob <- unique_sites[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      site_list_mob <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      site_list_mob <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id] 
    }
    
    temp <- mob_card[site_id %in% site_list_mob, .N, by = .(TRANSACTION_DATE, tod, ROUTE)]
    temp[, Date := as.character(TRANSACTION_DATE)]
    temp[, Day := weekdays(TRANSACTION_DATE)]
    
    if ('all_tod' %in% trend_list & 'all_routes_trend' %in% route_list) {
      temp[, N := sum(N), by = TRANSACTION_DATE]
      temp <- unique(temp, by = 'TRANSACTION_DATE')
      temp
    } else if ('all_tod' %in% trend_list) {
      temp <- temp[ROUTE %in% route_list]
      temp[, N := sum(N), by = .(TRANSACTION_DATE)]
      temp <- unique(temp, by = 'TRANSACTION_DATE')
      temp
    } else if ('all_routes_trend' %in% route_list) {
      temp <- temp[tod %in% trend_list]
      temp[, N := sum(N), by = .(TRANSACTION_DATE)]
      temp <- unique(temp, by = 'TRANSACTION_DATE')
      temp
    } else {
      temp <- temp[tod %in% trend_list & ROUTE %in% route_list]
      temp[, N := sum(N), by = .(TRANSACTION_DATE)]
      temp <- unique(temp, by = 'TRANSACTION_DATE')
      temp
    }
  })
  
  dat2 <- reactive({
    site_input <- input$selector_type
    route_list <- input$route_trend
    
    if('all_stops_sel' %in% site_input) {
      site_list_wc <- unique_sites[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      site_list_wc <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      site_list_wc <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id]
    }
    
    temp <- wc_boards[site_id %in% site_list_wc, .N, by = .(monthYear, TOD, LM_ROUTE_ABBR)]
    temp[, Date := monthYear]
    temp[, monthYear := as.Date(monthYear)]
    temp[, Day := weekdays(monthYear)]
    temp[, TOD := factor(TOD, levels = c("Early", "AM Peak", "Midday", "PM Peak", "Evening", "Owl"), ordered = T)]
    
    if ('all_routes_trend' %in% route_list) {
      temp[, N := sum(N), by = .(monthYear, TOD)]
      temp <- unique(temp, by = c('monthYear', 'TOD'))
      temp
    } else {
      temp <- temp[LM_ROUTE_ABBR %in% route_list]
      temp[, N := sum(N), by = .(monthYear, TOD)]
      temp <- unique(temp, by = c('monthYear', 'TOD'))
      temp
    }
  })
  
  dat_tag2 <- reactive({
    site_input <- input$selector_type
    route_list <- input$route_trend
    
    if('all_stops_sel' %in% site_input) {
      site_list_mob <- unique_sites[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      site_list_mob <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      site_list_mob <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id]
    }
    
    temp <- mob_card[site_id %in% site_list_mob, .N, by = .(TRANSACTION_DATE, tod, ROUTE)]
    temp[, Date := as.character(TRANSACTION_DATE)]
    temp[, Day := weekdays(TRANSACTION_DATE)]
    temp[, tod := factor(tod, levels = c("Early", "AM Peak", "Midday", "PM Peak", "Evening", "Owl"), ordered = T)]
    
    if ('all_routes_trend' %in% route_list) {
      temp[, N := sum(N), by = .(TRANSACTION_DATE, tod)]
      temp <- unique(temp, by = c('TRANSACTION_DATE', 'tod'))
      temp
    } else {
      temp <- temp[ROUTE %in% route_list]
      temp[, N := sum(N), by = .(TRANSACTION_DATE, tod)]
      temp <- unique(temp, by = c('TRANSACTION_DATE', 'tod'))
      temp
    }
  })
  
  
  output$wc_trends <- renderPlotly({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }  
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    validate(need(dim(trend_data())[1] > 0, message = 'No wheelchair lift cycles for this route/time of day combination.'))
    p <- ggplot(trend_data(), aes(x = monthYear, y = N)) +
      geom_line() +
      geom_point(size = 1, aes(label2 = Date, label = N, label3 = Day)) +
      theme_bw() +
      ggtitle("Cycles per Day") +
      geom_smooth() +
      xlab("Date") +
      ylab("Number of Ramp Cycles") +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    ggplotly(p, tooltip = c('label', 'label2', 'label3'))
  })
  
  
  output$mob_trends <- renderPlotly({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    validate(need(dim(mob_trend_data())[1] > 0, message = 'No limited mobility card tags for this route/time of day combination.'))
    p2 <- ggplot(mob_trend_data(), aes(x = TRANSACTION_DATE, y = N)) +
      geom_line() +
      geom_point(size = 1, aes(label = Date, label2 = N, label3 = Day)) +
      theme_bw() +
      ggtitle("Tags per Day") +
      geom_smooth() +
      xlab("Date") +
      ylab("N = Number of Mobility Card Tags") +
      theme(plot.margin = unit(c(1,1,1,1), 'cm'))
    ggplotly(p2, tooltip = c('label', 'label2', 'label3'))
  })
  
  output$route_trendSelected <- renderText({
    if ('all_routes_trend' %in% input$route_trend) {
      c('Route(s) Selected: All')
    } else {
      c('Route(s) Selected:', paste(input$route_trend, collapse = ", "))
    }
  })
  
  
  output$trend_static <- renderPlot({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    validate(need(dim(trend_data())[1] > 0, message = 'No wheelchair lift cycles for this route/time of day combination.'))
    
    ggplot(dat2(), aes(x = monthYear, y = N, label = Date)) +
      geom_line(aes(group = TOD, color = TOD)) +
      theme_bw() +
      ggtitle("Ramp Cycles by Time of Day") +
      scale_color_brewer(palette = 'Set2', name = "Time of Day") +
      xlab("Date") +
      ylab("N = Number of Ramp Cycles")
  })
  
  output$mob_trend_static <- renderPlot({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    validate(need(dim(mob_trend_data())[1] > 0, message = 'No limited mobility card tags for this route/time of day combination.'))
    
    ggplot(dat_tag2(), aes(x = TRANSACTION_DATE, y = N, label = Date)) + 
      geom_line(aes(group = tod, color = tod)) + 
      theme_bw() + 
      ggtitle("Mobility Card Tags by Time of Day") + 
      scale_color_brewer(palette = 'Set2', name = "Time of Day") + 
      xlab("Date") + 
      ylab("N = Number of Mobility Card Tags")
  })
  
  
  #Most Active Stops Tab ---------------------------------------
  
  observeEvent(input$button, {
    shinyjs::toggle("instructions")
  })
  
  plot_dat <- reactive({
      if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
        return(NULL)
      }

      if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
        return(NULL)
      }
    
    site_input <- input$selector_type
    route_input <- input$route_activity
    time_input <- input$msg_time_activity
    day_input <- input$day_activity
    month_input <- input$month_activity
    start_date <- input$date_range_activity[1]
    end_date <- input$date_range_activity[2]
    
    
    #subset site_list ----
    if('all_stops_sel' %in% site_input) {
      sel_sites <- unq_site[, site_id]
    } else if ('list_stops_sel' %in% site_input) {
      sel_sites <- subset(unique_sites, site_id %in% csv_up_stops()[, V1])[, site_id]
    } else if ('geo_stops_sel' %in% site_input) {
      sel_sites <- subset(unique_sites, site_id %in% geo_sel_stops()[, V1])[, site_id]
    }
    
    temp_site <- site_list[site_id %in% sel_sites]
    
    #subset based on route choices
    if (!('all_routes_activity' %in% route_input)) {
      temp_site <- subset(temp_site, route %in% route_input)
    }
    
    #subset based on start and end date chosen
    temp_site <- subset(temp_site, DEACTIVATION_DATE > as.Date(start_date))
    temp_site[ACTIVATION_DATE < as.Date(start_date), ACTIVATION_DATE == as.Date(start_date)]
    temp_site <- subset(temp_site, ACTIVATION_DATE <= as.Date(end_date))
    temp_site[DEACTIVATION_DATE > as.Date(end_date), DEACTIVATION_DATE == as.Date(end_date)]
    
    #subset based on month chosen
    if (!('all_months_activity' %in% month_input)) {
      temp_site[, has_Jan := ifelse((month(ACTIVATION_DATE) <=1 & month(DEACTIVATION_DATE - 1) >= 1), "January", "N")]
      temp_site[, has_Feb := ifelse((month(ACTIVATION_DATE) <= 2 & month(DEACTIVATION_DATE - 1) >= 2), "February", "N")]
      temp_site[, has_Mar := ifelse((month(ACTIVATION_DATE) <= 3 & month(DEACTIVATION_DATE - 1) >= 3), "March", "N")]
      temp_site[, has_Apr := ifelse((month(ACTIVATION_DATE) <= 4 & month(DEACTIVATION_DATE - 1) >= 4), "April", "N")]
      temp_site[, has_May := ifelse((month(ACTIVATION_DATE) <= 5 & month(DEACTIVATION_DATE - 1) >= 5), "May", "N")]
      temp_site[, has_Jun := ifelse((month(ACTIVATION_DATE) <= 6 & month(DEACTIVATION_DATE - 1) >= 6), "June", "N")]
      temp_site[, has_Jul := ifelse((month(ACTIVATION_DATE) <= 7 & month(DEACTIVATION_DATE - 1) >= 7), "July", "N")]
      temp_site[, has_Aug := ifelse((month(ACTIVATION_DATE) <= 8 & month(DEACTIVATION_DATE - 1) >= 8), "August", "N")]
      temp_site[, has_Sep := ifelse((month(ACTIVATION_DATE) <= 9 & month(DEACTIVATION_DATE - 1) >= 9), "September", "N")]
      temp_site[, has_Oct := ifelse((month(ACTIVATION_DATE) <= 10 & month(DEACTIVATION_DATE - 1) >= 10), "October", "N")]
      temp_site[, has_Nov := ifelse((month(ACTIVATION_DATE) <= 11 & month(DEACTIVATION_DATE - 1) >= 11), "November", "N")]
      temp_site[, has_Dec := ifelse((month(ACTIVATION_DATE) <= 12 & month(DEACTIVATION_DATE - 1) >= 12), "December", "N")]
      
      temp_site <- subset(temp_site, has_Jan %in% month_input | 
                            has_Feb %in% month_input |
                            has_Mar %in% month_input |
                            has_Apr %in% month_input |
                            has_May %in% month_input |
                            has_Jun %in% month_input |
                            has_Jul %in% month_input |
                            has_Aug %in% month_input |
                            has_Sep %in% month_input |
                            has_Oct %in% month_input |
                            has_Nov %in% month_input |
                            has_Dec %in% month_input )
      
      num_month_input <- match(month_input, month.name)
      
      temp_site[month(ACTIVATION_DATE) < min(num_month_input), ACTIVATION_DATE := as.Date(paste0("2018-", min(num_month_input), "-01"))]
      temp_site[month(DEACTIVATION_DATE) > max(num_month_input), DEACTIVATION_DATE := as.Date(paste0("2018-", max(num_month_input)+1, "-01"))]
    }
    
    temp_site[, act_wkday := wday(ACTIVATION_DATE)]
    temp_site[, deact_wkday := wday(DEACTIVATION_DATE)]
    temp_site[, days_active := as.numeric(DEACTIVATION_DATE - ACTIVATION_DATE)]
    temp_site[days_active == min(days_active), days_active := days_active + 1]
    
    temp_site[, num_Sun := (days_active + ((act_wkday - 2) %% 7)) %/% 7]
    temp_site[, num_Mon := (days_active + ((act_wkday - 3) %% 7)) %/% 7]
    temp_site[, num_Tue := (days_active + ((act_wkday - 4) %% 7)) %/% 7]
    temp_site[, num_Wed := (days_active + ((act_wkday - 5) %% 7)) %/% 7]
    temp_site[, num_Thu := (days_active + ((act_wkday - 6) %% 7)) %/% 7]
    temp_site[, num_Fri := (days_active + ((act_wkday - 7) %% 7)) %/% 7]
    temp_site[, num_Sat := (days_active + ((act_wkday - 8) %% 7)) %/% 7]
    temp_site[, num_Weekend := num_Sat + num_Sun]
    temp_site[, num_Weekday := num_Mon + num_Tue + num_Wed + num_Thu + num_Fri]
    
    if ('all_days_activity' %in% day_input) {
      temp_site[, temp_days_active := days_active]
    } else if ('weekend_activity' %in% day_input) {
      temp_site[, temp_days_active := num_Weekend]
    } else if ('weekday_activity' %in% day_input) {
      temp_site[, temp_days_active := num_Weekday]
    } else if ('Sunday' %in% day_input) {
      temp_site[, temp_days_active := num_Sun]
    } else if ('Monday' %in% day_input) {
      temp_site[, temp_days_active := num_Mon]
    } else if ('Tuesday' %in% day_input) {
      temp_site[, temp_days_active := num_Tue]
    } else if ('Wednesday' %in% day_input) {
      temp_site[, temp_days_active := num_Wed]
    } else if ('Thursday' %in% day_input) {
      temp_site[, temp_days_active := num_Thu]
    } else if ('Friday' %in% day_input) {
      temp_site[, temp_days_active := num_Fri]
    } else if ('Saturday' %in% day_input) {
      temp_site[, temp_days_active := num_Sat]
    }
    
    temp_site <- unique(temp_site, by = c('tt_version_id', 'site_id'))
    temp_site[, cyc_num_days_sel := sum(temp_days_active), by = site_id]
    temp_site[, tag_num_days_sel := sum(temp_days_active), by = site_id]
    temp_site[, num_active_days_sel := sum(temp_days_active), by = site_id]
    temp_site <- unique(temp_site, by = 'site_id')
    
    #subset wc_boards ----
    
    if ('all_routes_activity' %in% route_input) {
      route_activity_list <- wc_boards[, unique(LM_ROUTE_ABBR)]
    } else {
      route_activity_list <- route_input
    }
    
    if ('all_times_activity' %in% time_input) {
      time_activity_list <- wc_boards[, unique(TOD)]
    } else {
      time_activity_list <- time_input
    }
    
    if ('all_days_activity' %in% day_input) {
      day_activity_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    } else {
      day_activity_list <- day_input
    }
    
    if ('all_months_activity' %in% month_input) {
      month_activity_list <- wc_boards[, unique(month)]
    } else {
      month_activity_list <- month_input
    }
    
    #Subset the data based on filter choices
    temp_boards <- subset(wc_boards, site_id %in% sel_sites & LM_ROUTE_ABBR %in% route_activity_list & TOD %in% time_activity_list 
                          & MsgDate >= start_date & MsgDate <= end_date & weekday %in% day_activity_list
                          & month %in% month_activity_list)
    
    temp_boards <- temp_site[, .(site_id, cyc_num_days_sel, num_active_days, num_active_days_sel)][temp_boards, on = 'site_id']    
    temp_boards[, cycles := .N, by = site_id]
    temp_boards[, cyc_pct := cycles/total_cycles]
    
    temp_boards[, totalBoard := sum(board), by = site_id]
    temp_boards[, totalAlight := sum(alight), by = site_id]
    temp_boards[, Activity := totalBoard + totalAlight]
    temp_boards[, cycs_per_day_sel :=  round(cycles/cyc_num_days_sel, 2)]
    temp_boards[, activity_per_day_sel := Activity/cyc_num_days_sel]
    
    if (input$first_last == FALSE) {
      temp_boards <- temp_boards[!(is_first == 1) & !(is_last == 1)]
    }
    
    if (input$number_stops == 'all_stops') {
      temp <- unique(temp_boards, by = 'site_id')
      temp_ramp <- temp[order(-cycs_per_day_sel)]
      #temp_ramp[, cycs_idx := .I]
    } else {
      
      #Compute the number of unique site ids appearing in the selected data
      num_stops <- temp_boards[, uniqueN(site_id)]
      
      #Compute the number of cycle activities per site id from the data selected by filters and order big -> little
      temp_num_cycles <- unique(temp_boards, by = "site_id")
      temp_num_cycles <- temp_num_cycles[order(-cycs_per_day_sel)]
      
      if (num_stops >= 100) {
        temp_num_cycles <- temp_num_cycles[1:100]
      }
      
      #Select only the stops in the top 100
      temp_boards <- temp_boards[site_id %in% temp_num_cycles$site_id]
      temp_boards <- temp_boards[temp_num_cycles, mult = "first", on = "site_id", nomatch = 0L]
      
      #Merge
      temp_ramp <- temp_num_cycles[temp_boards, on = .(site_id)]
      temp_ramp[, c("i.Activity", "i.totalBoard", "i.totalAlight", "i.cycles") := NULL]
      temp_ramp <- temp_ramp[order(-cycs_per_day_sel)]
      #temp_ramp[, cycs_idx := .I]
      #temp_ramp <- temp_ramp[order(-activity_per_day_sel)]
      #temp_ramp[, site_id := as.character(site_id)]
      #temp_ramp[, act_idx := .I]
    }
    
    temp_ramp <- temp_ramp[order(cycs_per_day_sel)]
    temp_ramp[, cycs_idx := .I]
    
    #subset mob_card ----
    if ('all_routes_activity' %in% route_input) {
      route_mob_list <- mob_card[, unique(ROUTE)]
    } else {
      route_mob_list <- route_input
    }
    
    if ('all_times_activity' %in% time_input) {
      time_mob_list <- mob_card[, unique(tod)]
    } else {
      time_mob_list <- time_input
    }
    
    if ('all_days_activity' %in% day_input) {
      day_mob_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    } else {
      day_mob_list <- day_input
    }
    
    if ('all_months_activity' %in% month_input) {
      month_mob_list <- mob_card[, unique(month)]
    } else {
      month_mob_list <- month_input
    }
    
    #Subset the data based on filter choices
    temp_card <- subset(mob_card, site_id %in% sel_sites & ROUTE %in% route_mob_list & tod %in% time_mob_list 
                        & TRANSACTION_DATE >= start_date & TRANSACTION_DATE <= end_date
                        & weekday %in% day_mob_list
                        & month %in% month_mob_list)
    
    temp_card <- temp_site[, .(site_id, tag_num_days_sel, num_active_days, num_active_days_sel)][temp_card, on = 'site_id']
    
    temp_card[, tags := .N, by = site_id]
    temp_card[, tag_pct := tags/total_tags]
    temp_card[, tags_per_day_sel := round(tags/tag_num_days_sel, 2)]
    
    #Compute the number of unique site ids appearing in the selected data and order by number of tags
    num_stops <- temp_card[, uniqueN(site_id)]
    temp_num_tag <- unique(temp_card, by = 'site_id')
    temp_num_tag <- temp_num_tag[order(-tags_per_day_sel)]
    
    if(input$number_stops == 'all_stops') {
      temp_tag <- unique(temp_card, by = 'site_id')
      temp_tag[, num_tags := tags]
      temp_tag <- temp_tag[order(-tags_per_day_sel)]
      #temp_tag[, tag_idx := .I]
    } else {
      if (num_stops >= 100) {
        temp_num_tag <- temp_num_tag[1:100]
      }
      
      #Select only the stops in the top 100
      temp_card <- temp_card[site_id %in% temp_num_tag$site_id]
      temp_card <- temp_card[temp_num_tag, mult = "first", on = "site_id", nomatch = 0L]
      
      #Merge
      temp_tag <- temp_num_tag[temp_card, on = .(site_id)]
      temp_tag[, i.tags := NULL]
      temp_tag[, num_tags := tags]
      
      temp_tag <- temp_tag[order(-tags_per_day_sel)]
      #temp_tag[, tag_idx := .I]
    }
    
    temp_tag <- temp_tag[order(tags_per_day_sel)]
    temp_tag[, tag_idx := .I]
    
    all_sites <- temp_ramp[, site_id := as.character(site_id)]
    all_sites <- all_sites[, unique(site_id)]
    all_sites <- c(all_sites, temp_tag[, unique(site_id)])
    all_sites <- unique(all_sites)
    all_sites <- data.table(site_id = all_sites)
    
    all_dat <- temp_ramp[, .(route_list, site_id, site_on, site_at, cyc_num_days, cycs_per_day, cyc_num_days_sel, cycs_per_day_sel, cyc_pct, source, cycs_idx, site_lon, site_lat, num_active_days, num_active_days_sel)][all_sites, on = 'site_id']
    all_dat <- temp_tag[, .(route_list, site_id, site_on, site_at, tag_num_days, tags_per_day, tag_num_days_sel, tags_per_day_sel, tag_pct, source, tag_idx, LATITUDE, LONGITUDE, num_active_days, num_active_days_sel)][all_dat, on = 'site_id']
    all_dat[is.na(site_on), site_on := i.site_on]
    all_dat[is.na(site_at), site_at := i.site_at]
    all_dat[, `:=` (i.site_on = NULL, i.site_at = NULL)]
    all_dat[is.na(site_lat), site_lat := LATITUDE]
    all_dat[is.na(site_lon), site_lon := LONGITUDE]
    all_dat[, `:=` (LATITUDE = NULL, LONGITUDE = NULL)]
    all_dat[is.na(route_list), route_list := i.route_list]
    all_dat[, i.route_list := NULL]
    all_dat[is.na(num_active_days), num_active_days := i.num_active_days]
    all_dat[is.na(num_active_days_sel), num_active_days_sel := i.num_active_days_sel]
    all_dat[, `:=` (i.num_active_days = NULL, i.num_active_days_sel = NULL)]
    
    #all_dat <- unq_site[, .(site_id, num_active_days)][all_dat, on = 'site_id']
    
    all_dat[is.na(cyc_num_days), cyc_num_days := 0]
    all_dat[is.na(cyc_num_days_sel), cyc_num_days_sel := 0]
    
    all_dat[is.na(tag_num_days), tag_num_days := 0]
    all_dat[is.na(tag_num_days_sel), tag_num_days_sel := 0]
    
    
    all_dat[is.na(cycs_idx), cycs_idx := 0L]
    all_dat[is.na(tag_idx), tag_idx := 0L]
    
    num_stops_cyc <- all_dat[, max(cycs_idx)]
    
    num_stops_tag <- all_dat[, max(tag_idx)]
    
      if (input$cycle_quartile == 'bot25cyc') {
       cyc_high <-  num_stops_cyc/4
      } else if (input$cycle_quartile == 'bot50cyc') {
        cyc_high <- num_stops_cyc/2
      } else {
        cyc_high <- num_stops_cyc
      }
    
      if (input$cycle_quartile == 'top25cyc') {
        cyc_low <- (3/4)*num_stops_cyc
      } else if (input$cycle_quartile == 'top50cyc') {
        cyc_low <- num_stops_cyc/2 
      } else if (input$cycle_quartile == 'bot25cyc' | input$cycle_quartile == 'bot50cyc') {
        cyc_low <- 1
      } else {
        cyc_low <- 0
      }
    
      if (input$mob_quartile == 'bot25mob') {
        mob_high <- num_stops_tag/4
      } else if (input$mob_quartile == 'bot50mob') {
        mob_high <- num_stops_tag/2
      } else {
        mob_high <- num_stops_tag
      }
    
      if (input$mob_quartile == 'top25mob') {
        mob_low <- (3/4)*num_stops_tag
      } else if (input$mob_quartile == 'top50mob') {
        mob_low <- num_stops_tag/2
      } else if (input$mob_quartile == 'bot25mob' | input$mob_quartile == 'bot50mob') {
        mob_low <- 1
      } else {
        mob_low <- 0
      }
    
    all_dat <- subset(all_dat, cycs_idx >= cyc_low & cycs_idx <= cyc_high & tag_idx >= mob_low & tag_idx <= mob_high)
    #all_dat <- site_activity[all_dat, on = 'site_id']
    #all_dat[, `:=` (cyc_prop = cycs_per_day_sel/avg_boards, tag_prop = tags_per_day_sel/avg_boards)]
    all_dat[, idx := .I]

    all_dat
    
  }) 

  
  max_dat <- reactive({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    plot_dat()[!is.na(cycs_per_day_sel), max(cycs_per_day_sel)]
  })
  
  min_dat <- reactive({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    plot_dat()[!is.na(cycs_per_day_sel), min(cycs_per_day_sel)]
  })
  
  max_tag_dat <- reactive({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    plot_dat()[!is.na(tags_per_day_sel), max(tags_per_day_sel)]
  })
  
  min_tag_dat <- reactive({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    plot_dat()[!is.na(tags_per_day_sel), min(tags_per_day_sel)]
  })
  
  slope_dat <- reactive({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    (15-3)/(max_dat()-min_dat())
  })
  
  slope_tag_dat <- reactive({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    (15-3)/(max_tag_dat()-min_tag_dat())
  })
  
  output$top100 <- renderLeaflet({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
                   req(plot_dat())
                   
                   validate(need(dim(plot_dat())[1] > 0, message = 'No data for these selections.'))
                   
                   #probably could write a function instead of a bunch of 'if else'
                   if (input$data_source_activity == 'both') {
                     if (input$bring_to_front == 'tag_front' & input$size_by == 'avg'){
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
               under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
               Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
               <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1,
                                          color = '#ff6666', radius = ~slope_dat()*(cycs_per_day_sel-min_dat()) + 3, group = 'all', 
                                          popup = get_popup2(plot_dat()[i.source == 'cyc'])) %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1, 
                                          color = '#6699ff', radius = ~slope_tag_dat()*(tags_per_day_sel-min_tag_dat()) + 3, group = 'all',
                                          popup = get_popup_tag2(plot_dat()[source == 'tag'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     } else if (input$bring_to_front == 'ramp_front' & input$size_by == 'avg') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1, 
                                          color = '#6699ff', radius = ~slope_tag_dat()*(tags_per_day_sel-min_tag_dat()) + 3, group = 'all',
                                          popup = get_popup_tag2(plot_dat()[source == 'tag'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1,
                                          color = '#ff6666', radius = ~slope_dat()*(cycs_per_day_sel-min_dat()) + 3, group = 'all', 
                                          popup = get_popup2(plot_dat()[i.source == 'cyc'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     } else if (input$bring_to_front == 'tag_front' & input$size_by == 'pct') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1,
                                          color = '#ff6666', radius = ~12*cyc_pct + 3, group = 'all', 
                                          popup = get_popup2(plot_dat()[i.source == 'cyc'])) %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1, 
                                          color = '#6699ff', radius = ~12*tag_pct + 3, group = 'all',
                                          popup = get_popup_tag2(plot_dat()[source == 'tag'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     } else if (input$bring_to_front == 'ramp_front' & input$size_by == 'pct') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1, 
                                          color = '#6699ff', radius = ~12*tag_pct + 3, group = 'all',
                                          popup = get_popup_tag2(plot_dat()[source == 'tag'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1,
                                          color = '#ff6666', radius = ~12*cyc_pct + 3, group = 'all', 
                                          popup = get_popup2(plot_dat()[i.source == 'cyc'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     }
                   } else if (input$data_source_activity == 'acc_ramp_cyc_activity') {
                     if (input$size_by == 'avg') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1,
                                          color = '#ff6666', radius = ~slope_dat()*(cycs_per_day_sel-min_dat()) + 3, group = 'all', 
                                          popup = get_popup2(plot_dat()[i.source == 'cyc'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     } else if (input$size_by == 'pct') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1,
                                          color = '#ff6666', radius = ~12*(cyc_pct) + 3, group = 'all', 
                                          popup = get_popup2(plot_dat()[i.source == 'cyc'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     }
                   } else if (input$data_source_activity == 'mob_card_tags_activity') {
                     if (input$size_by == 'avg') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1, 
                                          color = '#6699ff', radius = ~slope_tag_dat()*(tags_per_day_sel-min_tag_dat()) + 3, group = 'all',
                                          popup = get_popup_tag2(plot_dat()[source == 'tag'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     } else if (input$size_by == 'pct') {
                       leaflet() %>%
                         addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
                                  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
                                  under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. 
                                  Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under
                                  <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', group = "toner") %>% 
                         setView(av_lng(), av_lat(), zoom=12) %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'], lng = ~site_lon, lat = ~site_lat, fillOpacity = .3, stroke = TRUE, weight = 1, 
                                          color = '#6699ff', radius = ~12*tag_pct + 3, group = 'all',
                                          popup = get_popup_tag2(plot_dat()[source == 'tag'])) %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[i.source == 'cyc'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 9, weight = 4, color = "#000000", opacity = 1, group = 'all') %>%
                         addCircleMarkers(data = plot_dat()[source == 'tag'][idx %in% input$table_rows_selected], lng = ~site_lon, lat = ~site_lat, 
                                          fill = FALSE, radius = 13, weight = 4, color = "#F7FF00", opacity = 1, group = 'all') %>%
                         addLegend(data = dummy, position = "bottomright", pal = pal, values = ~Source)
                     }
                   }
                 })
  
  output$route_activitysSelected <- renderText({
    if ('all_routes_activity' %in% input$route_activity) {
      c('Route(s) Selected: All')
    } else {
      c('Route(s) Selected:', paste(input$route_activity, collapse = ", "))
    }
  })
  
  table_dat <- reactive({
    temp <- copy(plot_dat())
    temp[, tags_per_day := as.character(tags_per_day)]
    temp[, cycs_per_day := as.character(cycs_per_day)]
    temp[, tags_per_day_sel := as.character(tags_per_day_sel)]
    temp[, cycs_per_day_sel := as.character(cycs_per_day_sel)]
    # temp[is.na(tags_per_day), tags_per_day := 'Not in top 100 tag sites']
    # temp[is.na(cycs_per_day), cycs_per_day := 'Not in top 100 ramp sites']
    # temp[is.na(tags_per_day_sel), tags_per_day_sel := 'Not in top 100 tag sites']
    # temp[is.na(cycs_per_day_sel), cycs_per_day_sel := 'Not in top 100 ramp sites']
    temp
  })
  
  output$table <- renderDataTable({
    if ('list_stops_sel' %in% input$selector_type & is.null(csv_up_stops())) {
      return(NULL)
    }
    
    if ('geo_stops_sel' %in% input$selector_type & is.null(input$geo_stops_draw_new_feature)) {
      return(NULL)
    }
    
    datatable(data = table_dat()[, .(site_id, site_on, site_at, num_active_days, num_active_days_sel, cycs_per_day, cycs_per_day_sel, round(cyc_pct*100, 2), tags_per_day, tags_per_day_sel, round(tag_pct*100, 2))], 
              class = 'compact', colnames = c('Site ID' = 2, "Site On" = 3, "Site At" = 4, 
                                              "Days Site Active" = 5, "Days Site Active (with Filters)" = 6, "Average Cycles per Day" = 7, "Average Cycles per Day (with Filters)" = 8, 
                                              "Percent of Cycles in Selection" = 9, "Average Tags per Day" = 10, "Average Tags per Day (with Filters)" = 11,
                                              "Percent of Tags in Selection" = 12),
              options = list(columnDefs = list(list(
                className = 'dt-right', targets = '_all'
              )),  order = JS("[6, 'desc']"))
    )
  })
  
  output$download <- downloadHandler(
    filename = 'active_stops.csv',
    content = function(fname) {
      write.csv(table_dat()[order(as.numeric(site_id)), .(site_id, site_on, site_at, num_active_days, num_active_days_sel, cycs_per_day, cycs_per_day_sel, round(cyc_pct*100, 2), tags_per_day, tags_per_day_sel, round(tag_pct*100, 2))], fname)
    }
  ) 
  
  # feedback tab --------------------------------------------------------------
  # CHANGE EMAIL TO APP AUTHOR'S EMAIL AND CHANGE THE EMAIL SUBJECT TEXT 
  panelText <- eventReactive(input$feedbackSubmit, {
    ## Process feedback form and send via sendmailR
    success <- tryCatch({
      sendmail('madeline.handschy@metrotransit.org', 'madeline.handschy@metrotransit.org', 
               subject = 'Mobility App Feedback', 
               msg = paste0(input$feedback, '\n\n', 'email:', input$email, '\n', 'rating:', input$rating), 
               control = list(smtpServer = 'thor.metc.state.mn.us'))
      1
    }, error = function(e) return(0))
    div(HTML("Thank you for your feedback!"))
  })
  
  observeEvent(input$feedbackSubmit, {
    updateTextAreaInput(session, "feedback", value = paste(""))
    updateTextInput(session, "email", value = paste(""))
  })
  
  output$feedbackResponse <- renderUI(panelText())
})

