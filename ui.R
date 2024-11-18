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
source("Themes.R")

#the user interface for the Shiny dashboard 
ui <- dashboardPage(
  dashboardHeader(tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 62px}"),
                          tags$style(".main-header .logo {height: 62px}")
                          ),
  
#adding logo
  title = tags$img(src="image2.png", width="175",height="60")),
  
#adding sidebar with drop down options 
  dashboardSidebar(tags$div(style = "height: 30px;"),
                   selectInput("Region","Select region", choices = unique(combined_df_mapped$Region),selected = "England")            
                   ,selectInput("area", "Select area", choices = NULL,selected = "England"),
                   selectInput("compare","Select Comparator",choices = c("National","Regional")),
                   tags$style(HTML("
                                     .selectize-input,
                                     .selectize-dropdown {
                                     font-size: 14px;
                                    }
                                    "))
  ),
  
  dashboardBody(
#calling the custom style sheet for modifying text font and size from the css file
    fresh::use_theme(mytheme),
    includeCSS("www/styles.css"),
    tabsetPanel(
      type = "tabs",
      
#setting up the cover page of the dashboard
      tabPanel("Cover Page",
               tags$div(style = "height: 10px;"),
               tags$div(
                 class = "outer",
                 tags$div(
                   class = "middle",
                   tags$div(
                     class = "inner",
                     tags$h1(HTML("<u><strong>Background</strong></u>"), style = "font-size: 20px"),
                     tags$p(HTML("This dashboard has been produced for convenience, and to show how you can represent this publicly available data, 
                              it is not a monitored or validated tool - use at your own peril!"), style = "font-size: 20px"),
                     tags$div(style = "height: 5px;"),
                     tags$p(HTML("Fingertips is a large collection of public health data (managed by OHID). It provides easy access to a rich source 
                               of indicators across a range of health and wellbeing topics. This is publicly available data and on the website, it is organised 
                               into themed profiles. Using the web-based application provides a good way to view and compare organisations within themed profiles 
                               but there are limitations when it comes to looking at metrics across a number of themes."), style = "font-size: 20px"),
                     tags$div(style = "height: 5px;"),
                     tags$p(HTML("The metrics from Fingertips are shown in the dashboard at a geographical level of local authority. The sidebar allows 
                               users to view national, regional and local authority data. To view data for a given local authority ‘Select region’ where the local 
                               authority and then use the ‘Select area’ drop down. The dashboard also allows you to either compare local authority values to the 
                               national values or to the value for the region the local authority sits under by using the ‘Select comparator’ drop down."), 
                            style = "font-size: 20px"),
                     tags$div(style = "height: 10px;"),
                     tags$h2(HTML("<u><strong>Dashboard Navigation</strong></u>"), style = "font-size: 20px"),
                     tags$ul(
                       tags$li(HTML("<strong>Demographics</strong>: key demographic information including population by age and sex; ethnicity breakdown; 
                                                     deprivation scores and various life expectancy metrics"), style = "font-size: 20px"),
                       tags$li(HTML("<strong>Education, Employment and Households</strong>: provides metrics on school readiness and attainment, unemployment, 
                                                    homelessness, income deprivation, fuel poverty and loneliness"), style = "font-size: 20px"),
                       tags$li(HTML("<strong>Risk Factors</strong>: provides metrics on physical activity, alcohol and smoking, obesity, children with 
                                                    social/emotional/mental health needs and tooth decay in children"), style = "font-size: 20px"),
                       tags$li(HTML("<strong>Download</strong>: enables export of regional data in csv format"), style = "font-size: 20px"),
                       tags$li(HTML("<strong>Metadata</strong>: lists all measures in dashboard and provides link to Fingertips site"), style = "font-size: 20px"),
                       tags$li(HTML("<strong>Mappings</strong>: map of ICBs to local government areas. Some areas sit across multiple ICBs therefore the 
                                       mapping is not perfect and includes which partial government areas fall under ICBs"), style = "font-size: 20px")
                     ),
                     tags$div(style = "height: 10px;"),
                     tags$h3(HTML("<u><strong>Dashboard Development</strong></u>"), style = "font-size: 20px"),
                     tags$p(tagList("This dashboard was developed by the NECS Consultancy Analytics Team. To access the source code please refer to my 
                                       Github page:", url1), style = "font-size: 20px"),
                     tags$p(tagList("All data is from OHID’s Fingertips platform and has been pulled into the dashboard through an API using the fingertipsR 
                                                   package (", url2, "). (Westermann A, Fox S, Nanayakkara H, Flowers J (2024). fingertipsR: Fingertips Data 
                                                   for Public Health). R package version 1.0.12. The population pyramid was also created by 
                                                   code developed by the PHE team's fingertipsCharts package."), style = "font-size: 20px"),
                     
                   )
                 )
               )
      ),
      
#setting up the 'Demographics' tab
      tabPanel("Demographics",
               tags$div(style = "height: 10px;"),
               fluidRow(
                 column(width = 6,
#adding the population plot
                        plotOutput("popPlot", height = 550, width = "100%")),
                 column(width = 6,
#adding the ethnicity breakdown
                        plotOutput("pieplot", height = 550, width = "100%")),
                 tags$div(style = "height: 10px;"),
                 box(title = "Population Metrics",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
#calling the relevant outputs to display
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

#setting up the 'Education, Employment and Households' tab
      tabPanel("Education, Employment and Households",
               tags$div(style = "height: 20px;"),
               fluidRow(
                 box(title = "Education and Employment",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
#calling the relevant outputs to display
                     valueBoxOutput("schoolread", width = 3),
                     valueBoxOutput("avgattainment", width = 3),
                     valueBoxOutput("NEET", width = 3),
                     valueBoxOutput("unemployment", width = 3)),
                 
                 box(title = "Households and Loneliness",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
#calling the relevant outputs to display
                     valueBoxOutput("fuelpov", width = 3),
                     valueBoxOutput("homeless", width = 3),
                     valueBoxOutput("chomeless", width = 3),
                     valueBoxOutput("homeless55", width = 3),
                     valueBoxOutput("lowincome", width = 3),
                     valueBoxOutput("lonely", width = 3))
               )),

#adding a 'Risk Factors' tab
      tabPanel("Risk Factors",
               tags$div(style = "height: 20px;"),
               fluidRow(
                 box(title = "Childhood Risk factors",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
#calling the relevant outputs to display
                     valueBoxOutput("childrenactive", width = 3),
                     valueBoxOutput("childalc", width = 3),
                     valueBoxOutput("schoolMH", width = 3),
                     valueBoxOutput("toothdecay", width = 3)),
                 
                 box(title = "Childhood Weight",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
#calling the relevant outputs to display
                     valueBoxOutput("recepover", width = 3),
                     valueBoxOutput("y6over", width = 3),
                     valueBoxOutput("recepobese", width = 3),
                     valueBoxOutput("y6obese", width = 3)),
                 
                 box(title = "Other Risk factors",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
#calling the relevant outputs to display
                     valueBoxOutput("adultsactive", width = 3),
                     valueBoxOutput("adultalc", width = 3),
                     valueBoxOutput("adultobese", width = 3),
                     valueBoxOutput("smoke", width = 3))
                 
               )
      ),

#adding 'Download' tab to allow users to explort data csv
      tabPanel("Download",
               tags$div(style = "height: 20px;"),
               fluidRow(
                 box(title = "Download Regional Data",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     selectInput("dataset", "Choose a dataset:",
                                 choices = unique(combined_df_mapped$Region)),
                     downloadButton("downloadData", "Download csv")
                 ))
      ),

#adding metadata tab with relevant details about metrics included
      tabPanel("Metadata",
               tags$div(style = "height: 20px;"),
               fluidRow(
                 box(title = "Metadata",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("metadata")
                     
                 )
               )
      ),

#adding tab with mappings from ICB to local authorities
      tabPanel("Mappings",
               tags$div(style = "height: 20px;"),
               fluidRow(
                 box(title = "ICB to LA",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("LA_mappings")
                     
                 )
               )
      )
    )
  )
)
