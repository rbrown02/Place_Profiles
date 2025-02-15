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

source("Population_Chart.R")
source("Themes.R")
source("server.R")
source("ui.R")

shinyApp(ui, server)
