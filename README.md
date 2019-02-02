Metro Transit Limited Mobility App

What it is:
This repository contains code for a Shiny app created for internal use at Metro Transit. The app combines data about where buses deploy wheelchair lifts and where limited mobility fare cards are used. Users may either view all stops or select a subset of stops by uploading a .csv file of stop id numbers or selecting a geographic area on the map. After the initial stop selection, users are able to filter by route,  time of day, month. The app also shows the trends for the selected stop groups. Additionally, users may download a spreadsheet containing summary statistics about the total number of ramp deployments and limited mobility card tags for their selected stops.

This app and the resulting data product are currently being used by the cities of Coon Rapids, Minneapolis, and St. Paul to aid prioritization of bus stops for snow removal as well as by Metro Transit as part of a tool to prioritize the placement of new bus stop shelters.


Contents:
- Update_Data.R : Code to update data. Pulls, formats, and saves data from the date of the most recent record to the current data.
- global.R, server.R, ui.R : Code for Shiny app used to visualize limited mobility boardings and trends.
- top_100_stops.R : Code to generate a list of the top 100 bus stops to prioritize for snow removal. The list of candidate stops includes all bus stops that are not Park and Ride locations, do not have an existing shelter, and are not necessary to clear for ADA compliance (such stops are already prioritized for snow removal).
