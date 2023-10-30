library(dplyr)
library(fingertipsR)
library(forcats)
library(tidyverse)
library(writexl)
library(shiny)
library(shinydashboard)
library(plotly)
library(fingertipscharts)
library(readxl)
library(ggrepel)

# SELECT AREA TYPE FROM: 'Counties & UAs'; 'Districts & UAs','NHS Regions','Region','Sub-ICBs','ICBs'
input_area_name <- "Counties & UAs"

# Adjust area type
area_type <- (if (input_area_name == "Counties & UAs") { 402 }
              else if (input_area_name == "Districts & UAs") { 401 }
              else if (input_area_name == "NHS regions") { 223 }
              else if (input_area_name == "Region") { 6 }
              else if (input_area_name == "Sub-ICBs") { 66 }
              else if (input_area_name == "ICBs") { 221 }
              else { "" })

regions_list <- c("East Midlands region", "East of England region", "London region",
                  "North East region", "North West region", "South East region",
                  "South West region", "West Midlands region","Yorkshire and the Humber region")

# Input indicator ID and the corresponding y-label depending on data type
input_indicators <- data.frame(Indicator_ID = c(90366, 92901, 90631, 93378, 93203, 93098, 1730, 90362, 
                                                93553, 93759, 93739, 93103, 93758, 93701, 93736, 93014, 
                                                93570, 20601, 20602, 90319, 90323, 92904, 93764, 93881, 
                                                91871, 92500, 93085))

indicator_ids <- input_indicators$Indicator_ID
new_indicator_ids <- indicator_ids

data_frames_list <- list()

i <- 1
while (i <= length(new_indicator_ids)) {
  indicator_id <- new_indicator_ids[i]
  
  data_frame <- fingertips_data(
    IndicatorID = indicator_id,
    AreaTypeID = area_type
  )
  
  if (length(unique(data_frame$Sex)) == 1) {
    sex <- unique(data_frame$Sex)
  } else if ("Persons" %in% unique(data_frame$Sex)) {
    sex <- "Persons"
  } else if ("Not applicable" %in% unique(data_frame$Sex)) {
    sex <- "Not applicable"
  } else if (sum(new_indicator_ids == indicator_id) > 1) {
    sex <- "Male"
  } else {
    new_indicator_ids <- c(new_indicator_ids, indicator_id)
    sex <- "Female"
  }
  i <- i + 1
  
  data_frame <- data_frame %>%
    filter(!is.na(TimeperiodSortable)) %>%
    group_by(Sex, AreaName) %>%
    filter(TimeperiodSortable == max(TimeperiodSortable),
           Sex == sex,
           !(indicator_id == 93758 & Age != "16+ yrs"),
           !(indicator_id == 93014 & Age != "19+ yrs"),
           !(indicator_id == 93881 & Age != "18+ yrs"),
           !(indicator_id == 91871 & Age != "School age")) %>%
    select("IndicatorName", "AreaName", "Sex", "Timeperiod", "Value", "ComparedtoEnglandvalueorpercentiles")
  
  data_frames_list[[i]] <- data_frame
  
  cat("Processed indicator ID:", indicator_id, "\n")
}

combined_df <- bind_rows(data_frames_list)

################################################################################
################################~POP DATA~######################################
################################################################################
generate_ggplot_chart <- function(data, value, sex, age, area, area_name, comparator_1, comparator_2, title, subtitle, xlab) {
  if (!missing(area_name) & !missing(comparator_1) & !missing(comparator_2)) {
    areas <- c(area_name, comparator_1, comparator_2)
  }
  else if (!missing(area_name) & !missing(comparator_1) & missing(comparator_2)) {
    areas <- c(area_name, comparator_1)
  }
  else if (!missing(area_name) & missing(comparator_1) & missing(comparator_2)) {
    areas <- area_name
  }
  else {
    stop("area_name must be complete for a population pyramid to be drawn")
  }
  
  filtered_data <- data %>%
    filter({{ area }} %in% areas) %>%
    group_by({{ area }}) %>%
    mutate({{ value }} := 100 * ({{ value }}) / sum({{ value }}),
           {{ value }} := ifelse({{ sex }} == "Male", -({{ value }}), {{ value }}))
  
  extremex <- scales::breaks_pretty(n = 3)(0:max(abs(pull(filtered_data, {{ value }})), na.rm = TRUE))
  
  population <- ggplot(filter(filtered_data, {{ area }} == area_name), aes(
    y = {{ value }},
    x = {{ age }},
    fill = {{ sex }}
  )) +
    geom_col(col = "black", width = 0.7) +
    coord_flip() +
    scale_y_continuous(
      breaks = c(rev(-extremex), extremex[2:length(extremex)]),
      labels = abs(c(rev(extremex), extremex[2:length(extremex)]))
    ) +
    scale_fill_manual(
      name = "",
      values = c(Male = "#5555E6", Female = "#C2CCFF"),
      breaks = c("Male", "Female"),
      labels = c(paste(area_name, "(Male)"), paste(area_name, "(Female)"))
    ) +
    labs(title = title, subtitle = subtitle, y = xlab) +
    theme(
      legend.position = "bottom",
      legend.key = element_blank(),
      axis.title.y = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      panel.grid.major.x = element_line(colour = "gray80"),
      plot.title = element_text(face = "bold")
    )
  
  if (!missing(comparator_1)) {
    compdata1 <- filter(filtered_data, {{ area }} == comparator_1)
    population <- population +
      geom_line(
        data = compdata1,
        aes(
          y = {{ value }},
          x = {{ age }},
          group = interaction(pull(compdata1, {{ sex }}), pull(compdata1, {{ area }})),
          col = {{ area }}
        ),
        linewidth = 1.5
      )
    
    if (!missing(comparator_2)) {
      compdata2 <- filter(filtered_data, {{ area }} == comparator_2)
      population <- population +
        geom_line(
          data = compdata2,
          aes(
            y = {{ value }},
            x = {{ age }},
            group = interaction(pull(compdata2, {{ sex }}), pull(compdata2, {{ area }})),
            col = {{ area }}
          ),
          linewidth = 1.5
        ) +
        scale_colour_manual(
          name = "",
          breaks = c(comparator_1, comparator_2),
          limits = c(comparator_1, comparator_2),
          values = c("black", "#E563F9")
        )
    } else {
      population <- population +
        scale_colour_manual(
          name = "",
          breaks = c(comparator_1),
          limits = c(comparator_1),
          values = c("black")
        )
    }
  }
  
  return(population)
}

################################################################################
################################################################################

ethnicities <- read_csv("~/Place_Profile_Shiny_Dashboard/population-by-ethnicity-and-local-authority-2021.csv")

population <- read_csv("~/Place_Profile_Shiny_Dashboard/population-by-ethnicity-and-local-authority-2021.csv")

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(tags$li(class = "dropdown",
                                            tags$style(".main-header {max-height: 62px}"),
                                            tags$style(".main-header .logo {height: 62px}")
                    ),
                    title = tags$img(src="image.png", width="195",height="60")),
                    dashboardSidebar(tags$div(style = "height: 20px;"),
                      selectInput("area", "Select area", choices = unique(combined_df$AreaName))
                    ),
                    dashboardBody(
                      tabsetPanel(
                        type = "tabs",
                        tabPanel("Demographics",
                                 tags$div(style = "height: 10px;"),
                                 fluidRow(
                                   column(width = 6,
                                     plotOutput("popPlot", height = 550, width = "100%")),
                                   column(width = 6,
                                          plotOutput("pieplot", height = 550, width = "100%")),
                                   tags$div(style = "height: 10px;"),
                                   box(title = "Population Metrics",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = 12,
                                       valueBoxOutput("pop", width = 3),
                                       valueBoxOutput("deprived", width = 3),
                                       valueBoxOutput("ineqfem", width = 3),
                                       valueBoxOutput("ineqmal", width = 3),
                                       valueBoxOutput("lifeexpfem", width = 3),
                                       valueBoxOutput("lifeexpmal", width = 3),
                                       valueBoxOutput("hlifeexpfem", width = 3),
                                       valueBoxOutput("hlifeexpmal", width = 3)
                                   )
                                 )
                        ),
                        tabPanel("Education, Employment and Households",
                                 tags$div(style = "height: 20px;"),
                                 box(title = "Education and Employment",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     width = 12,
                                   fluidRow(
                                   column(
                                     width = 6,
                                     valueBoxOutput("schoolread", width = 6),
                                     valueBoxOutput("avgattainment", width = 6)
                                   ),
                                   column(
                                     width = 6,
                                     valueBoxOutput("NEET",width=6),
                                     valueBoxOutput("unemployment",width=6)
                                   )
                                 )
                        ),
                        box(
                          title = "Households and Loneliness",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          fluidRow(
                            column(width = 4, valueBoxOutput("fuelpov", width = 10)),
                            column(width = 4, valueBoxOutput("homeless", width = 10)),
                            column(width = 4, valueBoxOutput("chomeless", width = 10)),
                            column(width = 4, valueBoxOutput("lowincome", width = 10)),
                            column(width = 4, valueBoxOutput("oldalone", width = 10)),
                            column(width = 4, valueBoxOutput("lonely", width = 10))
                          )
                        )
                        ),
                        tabPanel("Risk Factors",
                                 tags$div(style = "height: 20px;"),
                                 fluidRow(
                                   box(title = "Childhood Risk factors",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = 12,
                                       valueBoxOutput("childrenactive", width = 3),
                                       valueBoxOutput("childalc", width = 3),
                                       valueBoxOutput("schoolMH", width = 3),
                                       valueBoxOutput("toothdecay", width = 3)),
                                   
                                   box(title = "Childhood Weight",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = 12,
                                       valueBoxOutput("recepover", width = 3),
                                       valueBoxOutput("y6over", width = 3),
                                       valueBoxOutput("recepobese", width = 3),
                                       valueBoxOutput("y6obese", width = 3)),
                                   
                                   box(title = "Other Risk factors",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = 12,
                                       valueBoxOutput("adultsactive", width = 3),
                                       valueBoxOutput("adultalc", width = 3),
                                       valueBoxOutput("adultobese", width = 3),
                                       valueBoxOutput("smoke", width = 3))
                              
                                       )
                                   )
                                     )
                                 )
                      )

server <- function(input, output) {
  output$ineqfem <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Inequality in life expectancy at birth",
             Sex == "Female")
    value <- as.character(format(unique(filtered_data$Value),nsmall=1))
    title_join <- paste("Inequality in life expectancy at birth<br>Female"," - ",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)      
    valueBox(
      value, title, color = "purple", icon = icon("scale-unbalanced")
    )
  })
  
  output$ineqmal <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Inequality in life expectancy at birth",
             Sex == "Male")
    value <- as.character(format(unique(filtered_data$Value),nsmall=1))
    title_join <- paste("Inequality in life expectancy at birth<br>Male"," - ",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "light-blue",icon = icon("scale-unbalanced") )
  })
  
  output$lifeexpfem <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Life expectancy at birth",
             Sex == "Female")
    value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))
    title_join <- paste("Life expectancy at birth<br>Female"," - ",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)      
    valueBox(
      value, title, color = "purple", icon = icon("calendar-days")
    )
  })
  
  output$lifeexpmal <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Life expectancy at birth",
             Sex == "Male")
    value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))
    title_join <- paste("Life expectancy at birth<br>Male"," - ",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "light-blue",icon = icon("calendar-days"))
  })
  
  output$hlifeexpfem <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Healthy life expectancy at birth",
             Sex == "Female")
    value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))
    title_join <- paste("Healthy life expectancy at birth<br>Female"," - ",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)      
    valueBox(
      value, title, color = "purple", icon = icon("heart")
    )
  })
  
  output$hlifeexpmal <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Healthy life expectancy at birth",
             Sex == "Male")
    value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))
    title_join <- paste("Life expectancy at birth<br>Male"," - ",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "light-blue",icon = icon("heart"))
  })
  
  output$schoolread <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "School readiness: percentage of children achieving a good level of development at the end of Reception",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("School ready at end of Reception<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("graduation-cap") )
  })
  
  output$avgattainment <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Average Attainment 8 score",
             Sex == "Persons")
    value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))
    title_join <- paste("Average Attainment 8 score<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("ranking-star") )
  })
  
  output$NEET <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "16 to 17 year olds not in education, employment or training (NEET) or whose activity is not known",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("16-17 year old NEET<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("person-circle-question") )
  })
  
  output$unemployment <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Long-Term Unemployment. Rate per 1,000 working age population",
             Sex == "Persons")
    if(input$area %in% regions_list) {value = paste("N/a for regions")}
    else {value <- as.character(format(round(unique(filtered_data$Value),1),nsmall=1))}
    title_join <- paste("Long-Term Unemployment per 1,000<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("briefcase") )
  })
  
  output$fuelpov <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Fuel poverty (low income, low energy efficiency methodology)",
             Sex == "Not applicable")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Households in Fuel Poverty<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("gas-pump") )
  })
  
  output$chomeless <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Homelessness - households with dependent children owed a duty under the Homelessness Reduction Act",
             Sex == "Not applicable")
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    title_join <- paste("Households with children classed at homeless per 1,000<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("house-circle-xmark") )
  })
  
  output$homeless <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Homelessness: households owed a duty under the Homelessness Reduction Act",
             Sex == "Not applicable")
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    title_join <- paste("Households classed at homeless per 1,000<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("house-circle-xmark") )
  })
  
  output$lowincome <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Children in absolute low income families (under 16s)",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Under 16s living in absolute low income families<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("money-bills") )
  })
  
  output$oldalone <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Older people living alone, Percentage of people aged 65 and over who are living alone",
             Sex == "Persons")
    if(input$area %in% regions_list) {value = paste("N/a for regions")}
    else if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Aged 65+ living alone<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("person-cane") )
  })
  
  output$lonely <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Loneliness: Percentage of adults who feel lonely often or always or some of the time",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Adults who feel lonely<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("heart-crack") )
  })
  
  output$adultsactive <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Percentage of physically active adults",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Physically Active Adults<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("person-running") )
  })
  
  output$childrenactive <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
      IndicatorName == "Percentage of physically active children and young people",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Physically Active CYP<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "blue",icon = icon("person-running") )
  })
  
  output$recepover <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Reception: Prevalence of overweight (including obesity)",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Reception children overweight<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("weight-scale") )
  })
  
  output$y6over <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Year 6: Prevalence of overweight (including obesity)",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Year 6 children overweight<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("weight-scale") )
  })
  
  output$y6obese <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Year 6: Prevalence of obesity (including severe obesity)",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Year 6 children obese<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("weight-scale") )
  })
  
  output$recepobese <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Reception: Prevalence of obesity (including severe obesity)",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Reception children obese<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "aqua",icon = icon("weight-scale") )
  })
  
  output$adultalc <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Admission episodes for alcohol-related conditions (Narrow)",
             Sex == "Persons")
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    title_join <- paste("Alcohol admits per 100,000 (all ages)<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("wine-glass") )
  })
  
  output$childalc <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Admission episodes for alcohol-specific conditions - Under 18s",
             Sex == "Persons")
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    title_join <- paste("Alcohol admits per 100,000 (< 18 yrs)<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "blue",icon = icon("wine-glass") )
  })
  
  output$adultobese <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Percentage of adults (aged 18+) classified as obese",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Adults Obsese<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("weight-scale") )
  })
  
  output$schoolMH <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "School pupils with social, emotional and mental health needs: % of school pupils with social, emotional and mental health needs",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Pupils with social, emotional and MH needs<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "blue",icon = icon("hand-holding-heart") )
  })
  
  output$toothdecay <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Percentage of three year olds with experience of visually obvious tooth decay",
             Sex == "Persons")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("3 year olds with tooth decay<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "blue",icon = icon("tooth") )
  })
  
  output$smoke <- renderValueBox({
    selected_area <- input$area
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,             
             IndicatorName == "Smoking status at time of delivery",
             Sex == "Female")
    if(is.na(filtered_data$Value) == T) {value = paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))}
    else {value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)),"%")}
    title_join <- paste("Smoking at time of delivery<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "purple",icon = icon("smoking") )
  })
  
  output$deprived <- renderValueBox({
    selected_area <- input$area
    england_data <- combined_df %>%
      filter(AreaName == "England",
             IndicatorName == "Deprivation score (IMD 2019)",
             Sex == "Persons")
    filtered_data <- combined_df %>%
      filter(AreaName == selected_area,
             IndicatorName == "Deprivation score (IMD 2019)",
             Sex == "Persons")
    value <- paste(as.character(format(round(unique(filtered_data$Value),1),nsmall=1)))
    title_join <- paste("Deprivation score (IMD 2019)<br>",unique(filtered_data$Timeperiod))
    title <- HTML(title_join)
    valueBox(
      value, title, color = "blue",icon = icon("scale-unbalanced-flip") )
  })
  
  output$pop <- renderValueBox({
    selected_area <- input$area
    filtered_pop <- population %>%
      filter(Upper_Tier == selected_area) %>%
      summarise(Population = sum(Ethnic_Population))
    formatted_value <- scales::comma(unique(filtered_pop$Population))
    title_join <- paste("Population<br>", "2021")
    title <- HTML(title_join)
    valueBox(
      formatted_value, title, color = "blue", icon = icon("users-line")
    )
  })
  
  pop_data <- fingertips_data(IndicatorID = 92708,
                              AreaTypeID = area_type)
  
  pop_data <- pop_data %>% 
    filter(TimeperiodSortable == max(TimeperiodSortable))
  
  year <- unique(pop_data$Timeperiod)
  
  region_map <- read_excel("~/Place_Profile_Shiny_Dashboard/Mappings.xlsx")
  
  pop_data_reg <- merge(x=pop_data, y=region_map, by="AreaName", all.x=TRUE)
  
  output$popPlot <- renderPlot({
    selected_area <- input$area
    
    region_name <- pop_data_reg %>%
      filter(AreaName == selected_area)
    
    selected_region <- unique(region_name$Region)
    filtered_pop_data <- pop_data_reg %>%
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
    
    # Call the generate_ggplot_chart function to create the ggplot chart
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
    # Render the ggplot chart
    print(ggplot_chart)  # Use print to display the ggplot chart
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
  
  # Customize the colors
  pie_plot <- pie_plot + scale_fill_manual(values = custom_colors)
  
  pie_plot + coord_flip()
  
  })
  
}

shinyApp(ui, server)
