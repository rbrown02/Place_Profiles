library(stringr)
library(fingertipsR)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(writexl)
library(shiny)
library(shinydashboard)
library(plotly)
library(readxl)
library(ggrepel)
library(DT)

source("Data_Pull.R")
source("Population_Chart.R")

ineq_title <- "Inequality in life expectancy at birth"
lifexp_title <- "Life expectancy at birth"
hlifexp_title <- "Healthy life expectancy at birth"
schread_title <- "School ready at end of Reception"
avgatt_title <- "Average Attainment 8 score"
neet_title <- "16-17 year old NEET"
ltu_title <- "Long-Term Unemployment per 1,000"
fuel_title <- "Households in Fuel Poverty"
childhomeless_title <- "Households with homeless children per 1,000"
homeless_title <- "Households classed as homeless per 1,000"
lowincome_title <- "Under 16s living in absolute low income families"
isolation_title <- "% social care users (65+) who lack social contact"
lonely_title <- "% adults who feel lonely often or always"
physact_title <- "Physically Active Adults"
physactcyp_title <- "Physically Active CYP"
recepow_title <- "Reception children overweight"
y6ow_title <- "Year 6 children overweight"
y6ob_title <- "Year 6 children obese"
recepob_title <- "Reception children obese"
alc_title <- "Alcohol admits per 100,000 (all ages)"
cyp_alc_title <- "Alcohol admits per 100,000 (< 18 yrs)"
adob_title <- "Adults Obese"
schneeds_title <- "Pupils with social, emotional and MH needs"
tooth_title <- "3 year olds with tooth decay"
smoke_title <- "Smoking at time of delivery"
dep_title <- "Deprivation score (IMD 2019)"
pop_title <- "Population"

source("Data_Pull.R")
source("Population_Chart.R")

server <- function(input, output, session) {
  observe({
    areas_selection <- combined_df_mapped %>%
      filter(Region == input$Region)
    filtered_areas <- unique(areas_selection$AreaName)
    updateSelectInput(session, "area", choices = filtered_areas)
  })
  
  output_data <- reactive({
    region_df %>%
      filter(Region == input$dataset) %>%
      select(-Code,-Region)
  })

  output$ineqfem <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "92901",
             Sex == "Female")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "92901",
               Sex == "Female")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "92901",
             Sex == "Female")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(unique(filtered_data$Value),nsmall=1))}
    value_comp <- as.character(format(unique(filtered_compare$Value),nsmall=1))
    if(input$compare == "National") {title_join <- paste(ineq_title,"<br>Female - ",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(ineq_title,"<br>Female - ",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")    
    valueBox(
      value, title, color = "purple", icon = icon("scale-unbalanced")
    )
  })
  
  output$ineqmal <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "92901",
             Sex == "Male")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "92901",
               Sex == "Male")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "92901",
             Sex == "Male")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(unique(filtered_data$Value),nsmall=1))}
    value_comp <- as.character(format(unique(filtered_compare$Value),nsmall=1))
    if(input$compare == "National") {title_join <- paste(ineq_title,"<br>Male - ",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(ineq_title,"<br>Male - ",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("scale-unbalanced") )
  })
  
  output$lifeexpfem <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "90366",
             Sex == "Female")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "90366",
               Sex == "Female")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "90366",
             Sex == "Female")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))}
    value_comp <- as.character(format(round(unique(filtered_compare$Value),1),nsmall=1))
    if(input$compare == "National") {title_join <- paste(lifexp_title,"<br>Female - ",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(lifexp_title,"<br>Female - ",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")   
    valueBox(
      value, title, color = "purple", icon = icon("calendar-days")
    )
  })
  
  output$lifeexpmal <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "90366",
             Sex == "Male")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "90366",
               Sex == "Male")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "90366",
             Sex == "Male")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))}
    value_comp <- as.character(format(round(unique(filtered_compare$Value),1),nsmall=1))
    if(input$compare == "National") {title_join <- paste(lifexp_title,"<br>Male - ",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(lifexp_title,"<br>Male - ",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("calendar-days"))
  })
  
  output$hlifeexpfem <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "90362",
             Sex == "Female")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "90362",
             Sex == "Female")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "90362",
             Sex == "Female")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))}
    value_comp <- as.character(format(round(unique(filtered_compare$Value),1),nsmall=1))
    if(input$compare == "National") {title_join <- paste(hlifexp_title,"<br>Female - ",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(hlifexp_title,"<br>Female - ",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")     
    valueBox(
      value, title, color = "purple", icon = icon("heart")
    )
  })
  
  output$hlifeexpmal <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "90362",
             Sex == "Male")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "90362",
             Sex == "Male")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "90362",
             Sex == "Male")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))}
    value_comp <- as.character(format(round(unique(filtered_compare$Value),1),nsmall=1))
    if(input$compare == "National") {title_join <- paste(hlifexp_title,"<br>Male - ",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(hlifexp_title,"<br>Male - ",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("heart"))
  })
  
  output$schoolread <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "90631",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "90631",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "90631",
             Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(schread_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(schread_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("graduation-cap") )
  })
  
  output$avgattainment <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93378",
             Sex == "Persons")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "93378",
               Sex == "Persons")}
    else {
      filtered_compare <- combined_df %>%
        filter(AreaName == input$Region,
               IndicatorID == "93378",
               Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(unique(filtered_data$Value),nsmall=1))}
    value_comp <- as.character(format(round(unique(filtered_compare$Value),1),nsmall=1))
    if(input$compare == "National") {title_join <- paste(avgatt_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(avgatt_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("ranking-star"))
  })
  
  output$NEET <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93203",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "93203",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "93203",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(neet_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(neet_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("person-circle-question") )
  })
  
  output$unemployment <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93098",
             Sex == "Persons")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "93098",
               Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "93098",
             Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- as.character(format(unique(filtered_data$Value),nsmall=1))}
    if(input$area %in% regions_list) {value = paste("N/a for regions")}
    else {value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))}
    value_comp <- as.character(format(round(unique(filtered_compare$Value),1),nsmall=1))
    if(input$compare == "National") {title_join <- paste(ltu_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(ltu_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ","N/A")}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("briefcase") )
  })
  
  output$fuelpov <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93759",
             Sex == "Not applicable")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "93759",
               Sex == "Not applicable")}
    else {
      filtered_compare <- combined_df %>%
        filter(AreaName == input$Region,
               IndicatorID == "93759",
               Sex == "Not applicable")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(fuel_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(fuel_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("gas-pump") )
  })
  
  output$chomeless <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93739",
             Sex == "Not applicable")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "93739",
               Sex == "Not applicable")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "93739",
             Sex == "Not applicable")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)))
    if(input$compare == "National") {title_join <- paste(childhomeless_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(childhomeless_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("house-circle-xmark") )
  })
  
  output$homeless <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93736",
             Sex == "Not applicable")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "93736",
             Sex == "Not applicable")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "93736",
             Sex == "Not applicable")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)))
    if(input$compare == "National") {title_join <- paste(homeless_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(homeless_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("house-circle-xmark") )
  })
  
  output$lowincome <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93701",
             Sex == "Persons")
    if(input$compare == "National") {
      filtered_compare <- combined_df %>%
        filter(AreaName == "England",
               IndicatorID == "93701",
               Sex == "Persons")}
    else {
      filtered_compare <- combined_df %>%
        filter(AreaName == input$Region,
               IndicatorID == "93701",
               Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(lowincome_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(lowincome_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("money-bills") )
  })
  
  output$isolation <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "90280",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "90280",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "90280",
             Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)))
    if(input$compare == "National") {title_join <- paste(isolation_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(isolation_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("person-cane") )
  })

  
  output$lonely <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "94175",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "94175",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "94175",
             Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(lonely_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(lonely_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("heart-crack") )
  })
  
  output$adultsactive <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93014",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "93014",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "93014",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(physact_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(physact_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("person-running") )
  })
  
  output$childrenactive <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "93570",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "93570",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "93570",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(physactcyp_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(physactcyp_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("person-running") )
  })
  
  output$recepover <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "20601",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "20601",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "20601",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(recepow_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(recepow_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("weight-scale") )
  })
  
  output$y6over <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "20602",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "20602",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "20602",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(y6ow_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(y6ow_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("weight-scale") )
  })
  
  output$y6obese <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "90323",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "90323",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "90323",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(y6ob_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(y6ob_title,unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("weight-scale") )
  })
  
  output$recepobese <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "90319",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "90319",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "90319",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(recepob_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(recepob_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regioal Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "purple",icon = icon("weight-scale") )
  })
  
  output$adultalc <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "93764",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "93764",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "93764",
             Sex == "Persons")}
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)))
    if(input$compare == "National") {title_join <- paste(alc_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(alc_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("wine-glass") )
  })
  
  output$childalc <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "92904",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "92904",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "92904",
             Sex == "Persons")}
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)))
    if(input$compare == "National") {title_join <- paste(cyp_alc_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(cyp_alc_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("wine-glass") )
  })
  
  output$adultobese <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "93881",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "93881",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "93881",
             Sex == "Persons")}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(adob_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(adob_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("weight-scale") )
  })
  
  output$schoolMH <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "91871",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "91871",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "91871",
             Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(schneeds_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(schneeds_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("hand-holding-heart") )
  })
  
  output$toothdecay <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "92500",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "92500",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "92500",
             Sex == "Persons")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(tooth_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(tooth_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("tooth") )
  })
  
  output$smoke <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorID == "93085",
             Sex == "Female")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",             
             IndicatorID == "93085",
             Sex == "Female")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,             
             IndicatorID == "93085",
             Sex == "Female")}
    if(nrow(filtered_data) == 0) {value = paste("No data available")}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)),"%")
    if(input$compare == "National") {title_join <- paste(smoke_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benchmark: ",value_comp)}
    else {title_join <- paste(smoke_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benchmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "light-blue",icon = icon("smoking") )
  })
  
  output$deprived <- renderValueBox({
    selected_area <- input$area
    filtered_data  <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorID == "93553",
             Sex == "Persons")
    if(input$compare == "National") {filtered_compare <- combined_df %>%
      filter(AreaName == "England",
             IndicatorID == "93553",
             Sex == "Persons")}
    else {filtered_compare <- combined_df %>%
      filter(AreaName == input$Region,
             IndicatorID == "93553",
             Sex == "Persons")}
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    value_comp <- paste(as.character(format(round(unique(filtered_compare$Value),1),nsmall=1)))
    if(input$compare == "National") {title_join <- paste(dep_title,"<br>",unique(filtered_data$Timeperiod),"<br>National Benckmark: ",value_comp)}
    else {title_join <- paste(dep_title,"<br>",unique(filtered_data$Timeperiod),"<br>Regional Benckmark: ",value_comp)}
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      value, title, color = "blue",icon = icon("scale-unbalanced-flip") )
  })
  
  output$pop <- renderValueBox({
    selected_area <- input$area
    filtered_pop <- pop_chart_data_reg %>%
      filter(AreaName == selected_area) %>%
      filter(Sex == "Persons") %>%
      filter(Age == "All ages") 
    formatted_value <- scales::comma(unique(filtered_pop$Value))
    title_join <- paste(" ","<br>",pop_title,"<br>", "2021")
    #change font size in the valuebox directly using the tags$p function
      #ideally would set this across all valueboxes in the css, but can't get it to work
    title <- tags$p(HTML(title_join), style = "font-size: 18px")
    valueBox(
      formatted_value, title, color = "blue", icon = icon("users-line")
    )
  })

  pop_data <- fingertips_data(IndicatorID = 92708,
                              AreaTypeID = 502)
  pop_data_regions <- fingertips_data(IndicatorID = 92708,
                                      AreaTypeID = 6)
  
  pop_data <- rbind(pop_data,pop_data_regions)
  
  pop_data <- pop_data %>% 
    filter(TimeperiodSortable == max(TimeperiodSortable))
  
  year <- unique(pop_data$Timeperiod)
  
  pop_region_map <- read_csv("./Mappings.csv")
  
  pop_region_map <- pop_region_map %>%
    select(-Region)
  
  pop_data <- pop_data %>%
    mutate(AreaName = str_remove_all(AreaName, " \\(statistical\\)"))
  
  pop_chart_data_reg <- merge(x=pop_data, y=pop_region_map, by="AreaName", all.x=TRUE)
  
  pop_chart_data_reg <- pop_chart_data_reg %>%
    distinct()
    

  output$popPlot <- renderPlot({
    selected_area <- input$area
    
    selected_region <- input$Region
    
    filtered_pop_data <- pop_chart_data_reg %>%
      filter(Age != "All ages",
             Sex %in% c("Male", "Female"),
             Timeperiod == year,
             AreaName %in% c("England", selected_region, selected_area)) %>% 
      mutate(Age = factor(Age, 
                          levels = c("0-4 yrs", "5-9 yrs", "10-14 yrs", 
                                     "15-19 yrs", "20-24 yrs", "25-29 yrs",
                                     "30-34 yrs", "35-39 yrs", "40-44 yrs",
                                     "45-49 yrs", "50-54 yrs", "55-59 yrs",
                                     "60-64 yrs", "65-69 yrs", "70-74 yrs",
                                     "75-79 yrs", "80-84 yrs", "85-89 yrs", 
                                     "90+ yrs"))) 

    ggplot_chart <- generate_ggplot_chart(
      data = filtered_pop_data,
      value = Value,
      sex = Sex,
      age = Age,
      area = AreaName,
      area_name = selected_area,
      comparator_1 = selected_region,
      comparator_2 = "England",
      title = paste("Population in", as.character(year)),
      subtitle = " ",
      xlab = "% of total population"
    )
    print(ggplot_chart) 
  })
  
  custom_colors <- c("#006AB4", "#ED8B00", "#960051", "#78BE20", "#605CA8")
  
  output$pieplot <- renderPlot({
    selected_area <- input$area
    
    ethnicities_filtered <- ethnicities %>%
      filter(Upper_Tier == selected_area)
    
    pie_plot <- ggplot(ethnicities_filtered, aes(x = reorder(Ethnicity,Prop), y = Prop, fill = Ethnicity)) +
      geom_col() +
      theme_void() +
      geom_text(
        aes(label = paste0(round(Prop * 100, 4),"%")),
        color = ifelse(ethnicities_filtered$Prop > 0.04, "white", "black"), 
        hjust = ifelse(ethnicities_filtered$Prop > 0.04, 1.15, -0.25), 
        size = 7.5
      ) +
      labs(title = "Ethnicity Breakdown") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 24)) +
      theme(legend.position = "none") +
      theme(plot.margin = margin(l = 30))
    
    pie_plot <- pie_plot + scale_fill_manual(values = custom_colors)
    pie_plot + coord_flip()
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, " Place Profile Data.csv", sep = "")
    },
    content = function(file) {
      write.csv(output_data(), file, row.names = FALSE)
    }    )
  
  output$metadata <- renderDataTable({
    datatable(metadata, options = list(pageLength = 10), escape = FALSE)
  })
  
  output$LA_mappings <- renderDataTable({
    datatable(LA_mappings, options = list(pageLength = 10))
  })
  }
