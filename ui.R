fillPage(
  tags$img(src = "MT_Bar_2.png", width = '100%'), 
  tags$head(
    tags$style(HTML(".sidebar {
                    height: 97vh; overflow-y: auto;
                    }"
               ) # make sidebar scrollable
    )),
  
  dashboardPage(
    skin = 'black', 
    dashboardHeader(title = span(tagList(img(src = "MetroTransit_T.png", height = '35px'), "Stop-Level Limited Mobility Activity")), titleWidth = 375), 
    dashboardSidebar(width = 350, div(style="overflow-y: scroll"),
                     #sidebar items ----
                     sidebarMenu(id = 'menu',
                                 menuItem(text = "About", icon = icon("list-alt"), tabName = 'abt'),
                                 menuItem(text = "Select Stops", icon = icon("list-ul"), tabName = 'sel_stops'),
                                 menuItem(text = "Heat Maps", icon = icon("map"), tabName = 'raw'),
                                 menuItem(text = "Trends", icon = icon("line-chart"), tabName = 'trend'),
                                 menuItem(text = "Stop-Level Maps", icon = icon("wheelchair"), tabName = 'top'),
                                 menuItem(text = "Feedback", icon = icon("envelope-open"), tabName = 'fb')
                     ), 
                     #tags$hr(), 
                     #box(shinyURL.ui(), width = 12, background = "blue"),
                     tags$hr(),
                     #stop selection menu ----
                     conditionalPanel(
                       condition = 'input.menu == "sel_stops"',
                       box(width = 12, background = 'blue',
                           fluidRow(column(12,(radioButtons("selector_type", label = "Selection Method", choices = list("All Stops" = 'all_stops_sel',
                                                                                                                        "Input List" = 'list_stops_sel',
                                                                                                                        "Create Bounding Box" = 'geo_stops_sel'),
                                                            selected = 'all_stops_sel'))
                           )
                           )
                       )
                     ),
                     #raw maps menu ----
                     conditionalPanel(
                       condition = 'input.menu == "raw"',
                       box(width = 12, background = 'blue',
                           fluidRow(column(6, (selectizeInput('plot_type', "Plot Type", choices = c("Heat Map" = 'heat', "Frequency Map" = 'circ')))
                           ),
                           column(6, (selectizeInput('data_source', "Data Source", choices = c('Ramp Cycles' = 'acc_ramp_cyc', 'Limited Mobility Card Tags' = 'mob_card_tags')))
                           )
                           )
                       ),
                       box(width = 12, background = 'blue', title = "Filter by:",
                           selectInput('route', 'Route (may select multiple):', choices = c("All Routes" = 'all_routes', wc_boards[order(LM_ROUTE_ABBR)][, unique(LM_ROUTE_ABBR)]),
                                       multiple = TRUE, selectize = FALSE, selected = 'all_routes'),
                           h4(textOutput('routesSelected'), style = "color:#000000"),
                           fluidRow(column(6, selectInput('msg_time', "Time of Day (may select multiple)", choices = c("All" = "all_times", wc_boards[, levels(TOD)]), multiple = TRUE, selectize = FALSE, selected = 'all_times')
                           ),
                           column(6, selectInput('day', "Day of the Week (may select multiple)", choices = c("All" = 'all_days', "Monday" = 'Monday', "Tuesday" = 'Tuesday', "Wednesday" = 'Wednesday', "Thursday" = 'Thursday', "Friday" = 'Friday', "Saturday" = 'Saturday', "Sunday" = 'Sunday'), multiple = TRUE, selectize = FALSE, selected = 'all_days')
                           )
                           ),
                           fluidRow(column(6, selectInput('month', "Month (may select multiple)", choices = c("All" = 'all_months', wc_boards[,unique(month)]), selected = 'all_months', multiple = TRUE, selectize = FALSE)
                           )
                           ),
                           fluidRow(column(12, dateRangeInput('date_range', 'Date Range', start = min_date, end = max_date, min = min_date, max = max_date, format = 'mm/dd/yy', startview = 'month', separator = '-')
                           )
                           )
                       )
                     ),
                     
                     #active stops menu ----
                     conditionalPanel(
                       condition = 'input.menu == "top"',
                       #  box(width = 12, background = 'blue',
                       #     checkboxGroupInput('data', label = "Data", choices = c("Ramp Cycles (Red)" = 'wc', "Mobility Card Tags (Blue)" = 'mob'), selected = c('wc', 'mob'))
                       #     ),
                       box(width = 12, background = 'blue', title = "Filter by:",
                           selectInput('route_activity', "Route (may select multiple):", choices = c("All Routes" = 'all_routes_activity', sort(unique(wc_boards$LM_ROUTE_ABBR))),
                                       multiple = TRUE, selectize = FALSE, selected = 'all_routes_activity'),
                           h4(textOutput('route_activitysSelected'), style = "color:#000000"),
                           fluidRow(column(12, selectInput('data_source_activity', "Data Source", choices = c('Cycles & Tags' = 'both', 'Cycles Only' = 'acc_ramp_cyc_activity', 'Tags Only' = 'mob_card_tags_activity'), selected = 'both')
                           )
                           ),
                           fluidRow(column(12, selectInput('number_stops', "Number of Stops", choices = c('Top 100' = 'hundred', 'All' = 'all_stops'), selected = 'hundred')
                           )
                           ),
                           fluidRow(column(6, selectInput('bring_to_front', 'Bring to Front', choices = c('Ramp Cycles' = 'ramp_front', 'Card Tags' = 'tag_front'), selected = 'tag_front')
                           ),
                           column(6, selectInput('size_by', "Size by", choices = c("Daily Average" = 'avg', "Percent in Selection" = 'pct'), selected = 'avg')
                           )
                           ),
                           fluidRow(column(6, selectInput('msg_time_activity', "Time of Day (may select multiple)", choices = c("All" = 'all_times_activity', wc_boards[, levels(TOD)]), multiple = TRUE, selectize = FALSE, selected = 'all_times_activity')
                           ),
                           column(6, selectInput('day_activity', "Day of Week (may select multiple)", choices = c("All" = 'all_days_activity', "Monday" = 'Monday', "Tuesday" = 'Tuesday', "Wednesday" = 'Wednesday', "Thursday" = 'Thursday', "Friday" = 'Friday', "Saturday" = 'Saturday', "Sunday" = 'Sunday'), multiple = TRUE, selectize = FALSE, selected = 'all_days_activity')
                           )
                           ),
                           fluidRow(column(6, selectInput('month_activity', "Month (may select multiple)", choices = c("All" = 'all_months_activity', wc_boards[, unique(month)]), multiple = TRUE, selectize = FALSE, selected = 'all_months_activity')
                           ),
                           column(6, checkboxInput('first_last', "Include first/last nodes (Ramp Only)", value = TRUE)
                           )
                           ),
                           fluidRow(column(12, dateRangeInput('date_range_activity', 'Date Range', start = min_date, end = max_date, min = min_date, max = max_date, format = 'mm/dd/yy', startview = 'month', separator = '-')
                           )
                           ),
                           fluidRow(column(12, selectInput('cycle_quartile', "Ramp Activity", choices = c("All" = 'allcyc', "Top 25" = 'top25cyc', "Top 50" = 'top50cyc',
                                                                                                          "Bottom 25" = 'bot25cyc', "Bottom 50" = 'bot50cyc'), selected = 'allcyc', selectize = FALSE, size = 2)
                           )
                           ),
                           # fluidRow(column(12, selectInput('activity_quartile', "Boarding and Alighting", choices = c("All" = 'alltot', "Top 25" = 'top25tot', "Top 50" = 'top50tot',
                           #                                                                                             "Bottom 25" = 'bot25tot', "Bottom 50" = 'bot50tot'), selected = 'alltot')
                           #                 )
                           #          ),
                           fluidRow(column(12, selectInput('mob_quartile', "Mobility Card Tags", choices = c("All" = 'allmob', "Top 25" = 'top25mob', "Top 50" = 'top50mob',
                                                                                                             "Bottom 25" = 'bot25mob', "Bottom 50" = 'bot50mob'), selected = 'allmob', selectize = FALSE, size = 2)
                           )
                           )
                       )
                     ),
                     
                     #trends menu ---- 
                     conditionalPanel(condition = 'input.menu == "trend"',
                                      box(width = 12, solidHeader = TRUE, background = 'blue', status = 'primary',
                                          fluidRow(column(12, selectInput('trend_tod', "Time of Day", choices = c("All" = 'all_tod', wc_boards[, levels(TOD)]), selected = 'all_tod')
                                          )
                                          ),
                                          fluidRow(column(12, selectInput('route_trend', "Route (may select multiple):", choices = c("All Routes" = 'all_routes_trend', sort(unique(wc_boards$LM_ROUTE_ABBR))),
                                                                          multiple = TRUE, selectize = FALSE, selected = 'all_routes_trend')
                                          )
                                          ),
                                          h4(textOutput('route_trendSelected'), style = 'color:#000000')
                                      )
                     )
    ),
    dashboardBody(style = "max-height: 90vh; overflow-y: scroll;",
                  tabItems(
                    tabItem(
                      'abt', 
                      box(width = 6, solidHeader = TRUE, status = 'primary', title = "What this app does", 
                          h5(strong("Heatmaps"), style = "color:#0053A0"), 
                          tags$ul(
                            tags$li(p("Heatmaps give an overview of where accessibilty ramps are deployed and where limited mobility fare Go-To Cards are tagged")),
                            tags$li(p("Users can filter by route, date range, day of the week, month, and time of day"))
                          ),
                          h5(strong("Trends"), style = "color:#0053A0"),
                          tags$ul(
                            tags$li(p("View the trends over time of wheelchair lift deployment and card tags from ", format(min_date, "%B %d, %Y"), " to ", format(max_date, "%B %d, %Y"))),
                            tags$li(p("Users can filter by time of day and route"))
                          ),
                          h5(strong("Stop-Level Maps"), style = 'color:#0053A0'),
                          tags$ul(
                            tags$li(p("View a map of the most active stops in terms of ramp deployments and card tags")),
                            tags$li(p("Users can filter by route, date range, day of the week, time of day"))
                          ),
                          tags$hr(), 
                          p(em(paste0("This app was last updated on ", max(file.info('ui.R')$mtime, file.info('server.R')$mtime, file.info('global.R')$mtime), " by Madeline Handschy (madeline.handschy@metrotransit.org).")))
                      ), 
                      box(width = 6, solidHeader = TRUE, status = 'success', title = "New in December 2018", 
                          tags$ul(
                            tags$li(p("'Select Stops' tab allows user to upload a CSV of stop IDs or select stops from a map and view only those stops.")),
                            tags$li(p("If you find a bug in this feature, please let me know! (madeline.handschy@metrotransit.org)"))
                          )
                      ), 
                      box(width = 6, solidHeader = TRUE, status = "primary", title = "Methodology", 
                          tags$ul(
                            tags$li(p("Accessibility ramp cycles are associated to a stop based on the date and stop offset of the message.")),
                            tags$li(p("Limitid Mobility Go-To Card tags are associated to the nearest stop serving the route."))
                          )
                      ),
                      box(width = 6, solidHeader = TRUE, status = "primary", title = "Data Sources", 
                          tags$ul(
                            tags$li(p("Information on accessibility ramp cycles is from the ETL Logged Messages Database")),
                            tags$li(p("Information on Limited Mobility Go-To Card tags is from the Go-To Card Usage Database."))
                          )
                      )
                      #  box(width = 6, solidHeader = TRUE, status = 'primary', title = "Related Apps", 
                      #      tags$ul(
                      #        tags$li(p("Bullet points with links to other Shiny apps that are similar/related to this one."))
                      #      )
                      #  )
                    ),
                    
                    #stop selection body ----
                    tabItem(
                      'sel_stops',
                      conditionalPanel(condition = 'input.selector_type == "all_stops_sel"',
                                       box(width = 12, solideHeader = FALSE, status = 'primary', p("You have selected all stops."))),
                      conditionalPanel(condition = 'input.selector_type == "list_stops_sel"',
                                       box(width = 12, solidHeader = TRUE, status = 'primary', title = 'Instructions',
                                           tags$ul(
                                             tags$li(p("Upload a CSV (file extension .csv) file of a list of stops. File should ONLY include the list of stop IDs
                                                       and no other information."))
                                             )),
                                       box(width = 12, solidHeader = TRUE, status = 'primary', title = "Upload CSV",
                                           fileInput("stops_csv", "Choose CSV File", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                       box(width = 12, solidHeader = TRUE, status = 'primary', title = "Uploaded Stops", tableOutput("up_stops"))),
                      conditionalPanel(condition = 'input.selector_type == "geo_stops_sel"',
                                       box(width = 12, solidHeader = TRUE, status = 'primary', title = 'Instructions',
                                           tags$ul(
                                             tags$li(p("Map may take a few seconds to load")),
                                             tags$li(p("Select the pentagon icon on the map")),
                                             tags$li(p("Click on the map to draw a box around desired stops"))
                                           )),
                                       box(width = 12, solidHeader = TRUE, status = 'primary', title = "Draw Bounding Box",
                                           leafletOutput("geo_stops")),
                                       box(width = 12, solidHeader = TRUE, status = 'primary', title = "Selected Stops", tableOutput("geo_stops_list")))
                                       ),
                    
                    #raw maps body ----
                    tabItem(
                      'raw',
                      tags$style(type = "text/css", "#raw_map {height: calc(100vh - 200px) !important;}"),
                      box(width = 12, solidHeader = TRUE, status = 'primary', title = 'Details', 
                          tags$ul(
                            tags$li("Use these maps to get an overview of where the accessibility ramps are deployed and where limited mobility Go-To Cards are tagged.
                                    The locations are obtained from raw latitude and longitude data and are NOT associated to stops."), 
                            tags$li("Data is from ", format(min_date, "%B %d, %Y"), " to ", format(max_date, "%B %d, %Y")),
                            tags$li("Select the data source (ramp cycles or card tags) you would like to view."),
                            tags$li("Select 'Frequency Map' for a map with the total number of ramp cycles or card tags associated to an area. Click on a colored circle to zoom in to that area."),
                            tags$li(p("If no map displays, please navigate to the 'Select Stops' tab and update your list of stops.", style = "color:red; font-size = 14em;"))
                            )
                      ),
                      leafletOutput("raw_map")
                      
                      #box(width = 12, title = "Limited Mobility Go-To Card Tags",
                      #    leafletOutput("raw_tags")
                      #    )
                    ),
                    
                    #trend body ----
                    tabItem(
                      'trend',
                      box(width = 12, solidHeader = TRUE, status = 'primary', title = 'Details', 
                          tags$ul(
                            tags$li('Hover over a point on the top row of plots to see the date and total activity (either ramp cycles or card tags) on that date for the selected
                                    time of day.'),
                            tags$li('Hover over the plot for a menu of zoom options.'),
                            tags$li('Select "Zoom" (magnifying glass icon) and then click and drag to select an area of the plot to magnify.'),
                            tags$li('Selet "Reset Axes" (home icon) to reset to default view.'),
                            tags$li(p("If no plots display, please navigate to the 'Select Stops' tab and update your list of stops.", style = "color:red; font-size = 14em;"))
                            )
                      ),
                      fluidRow(column(6, box(width = 12, height = NULL, plotlyOutput('wc_trends'))
                      ),
                      column(6, box(width = 12, height = NULL, plotlyOutput('mob_trends'))
                      )
                      ),
                      fluidRow(column(6, box(width = 12, height = NULL, plotOutput('trend_static'))
                      ),
                      column(6, box(width = 12, height = NULL, plotOutput('mob_trend_static'))
                      )
                      )
                    ),
                    
                    #active stops body ----
                    tabItem(
                      'top',
                      useShinyjs(),
                      box(id = "instructions", title = tags$div(tags$p("Instructions"), tags$p(actionButton(inputId = "button", label = "Show / Hide", style = "color:#ffffff"))), width = 12, background = 'blue',
                          tags$ul(
                            tags$li("Red circles denote stops where accessibility ramps are deployed. Blue circles denote stops where limited mobility Go-To Cards are tagged."),
                            tags$li("If 'Top 100 Stops' is selected, the map and table display the 100 stops with the highest daily average ramp cycles for your selections and 100 stops with the highest daily average card tags for your selections. Some stops fall into both categories.  There may be fewer than 100 total sites for certain filter choices."),
                            tags$li("Click on a circle to view the following information about that stop:"),
                            tags$ul(
                              tags$li('Site Id'),
                              tags$li('Site On & Site At'),
                              tags$li('Number of Days: Total number of days the stop was active'),
                              tags$li('Average Daily Cycles/Tags: Total cycles/tags at that site divided by total number of days active'),
                              tags$li('Number of Days (with filters applied): Total number of days the stop was active during user-selected date range.'),
                              tags$li('Average Daily Cycles/Tags (with filters applied): Total cycles/tags at that site for user selections divided by total number of days stop was active during user-selected date range'),
                              tags$li('Percent: Percentage of total tags at that site that occured with user-defined filters.')
                            ),
                            tags$li("Toggle whether cycles (red) or tags (blue) are shown in front using the 'Bring to Front' option. Use this if you can see a stop, but cannot click because of circle size. "),
                            tags$li("Click on a row of the table to highlight the corresponding site on the map. You may need to zoom/navigate to find the highlighted site."),
                            tags$li("NOTE: mobility card tags for A-line (rte 921) are not currently included")
                          )
                      ),
                      box(width = 12, height = NULL,
                          fillPage(leafletOutput("top100"))
                      ),
                      box(width = 12, height = NULL,
                          dataTableOutput('table')
                      ),
                      downloadButton('download', 'Download Table as CSV')
                    ),
                    
                    
                    #feedback ----
                    tabItem(
                      'fb', 
                      p('Would you like to see a new feature? Have you found a problem with the app? Just want to tell us you like this app? Tell us all about it, someone from our team will get back to you within a week.'),
                      includeCSS('www/stars.css'),
                      HTML(
                        '<h4>Rate Us!</h4>
                        <div id="rating" class="form-group shiny-input-radiogroup shiny-input-container shiny-bound-input">
                        <div class="shiny-options-group star-rating">
                        <input type="radio" name = "rating" value="1"><i></i>
                        <input type="radio" name = "rating" value="2"><i></i>
                        <input type="radio" name = "rating" value="3"><i></i>
                        <input type="radio" name = "rating" value="4"><i></i>
                        <input type="radio" name = "rating" value="5"><i></i>
                        </div>
                        </div>'),
                      textInput('email', "Email address if you'd like a reply", value = ''),
                      tags$textarea(id = "feedback", cols = 80, rows = 10),
                      p(),
                      actionButton('feedbackSubmit', label = "Submit feedback"),
                      conditionalPanel(condition = "input.feedbackSubmit > 0", htmlOutput("feedbackResponse"))
                      )
                    )
    )
                  )
                    )