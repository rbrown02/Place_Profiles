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

#selecting the area type. There are different area types for different indicators
#depending on when they were last updated.
area_name <- 502
area_name_regions <- 6

regions_list <- c("East Midlands region", "East of England region", "London region",
                  "North East region", "North West region", "South East region",
                  "South West region", "West Midlands region","Yorkshire and the Humber region")

# Input indicator ID and the corresponding y-label depending on data type
input_indicators_1 <- data.frame(Indicator_ID = c(90366, 90631, 93378, 
                                                  93203, 93098, 1730, 90362, 
                                                  93553, 93759, 93739, 93738, 93758, 93701, 93014, 
                                                  93570, 20601, 20602, 90319, 90323, 92904, 93764, 93881, 
                                                  91871, 92500, 93085, 92901, 93736))

indicator_ids <- input_indicators_1$Indicator_ID
new_indicator_ids <- indicator_ids

data_frames_list <- list()

# Call API once to pull in all data needed then subset for each stage of while loop below
data_frame_1 <- fingertips_data(
  IndicatorID = new_indicator_ids,
  AreaTypeID = area_name
)

data_frame_2 <- fingertips_data(
  IndicatorID = new_indicator_ids,
  AreaTypeID = area_name_regions
)

dataframe <- rbind(data_frame_1,data_frame_2)

i <- 1
while (i <= length(new_indicator_ids)) {
  indicator_id <- new_indicator_ids[i]
  sub_data_frame <- dataframe %>%
    filter(IndicatorID == indicator_id)
  
  if (length(unique(sub_data_frame$Sex)) == 1) {
    sex <- unique(sub_data_frame$Sex)
  } else if ("Persons" %in% unique(sub_data_frame$Sex)) {
    sex <- "Persons"
  }else if ("IndicatorID" == 93738) {
    sex <- "Persons"
  } else if ("Not applicable" %in% unique(sub_data_frame$Sex)) {
    sex <- "Not applicable"
  } else if (sum(new_indicator_ids == indicator_id) > 1) {
    sex <- "Male"
  } else {
    new_indicator_ids <- c(new_indicator_ids, indicator_id)
    sex <- "Female"
  }
  i <- i + 1
  
  i_data_frame <- sub_data_frame %>%
    filter(!is.na(TimeperiodSortable)) %>%
    group_by(Sex, AreaName) %>%
    filter(TimeperiodSortable == max(TimeperiodSortable),
           Sex == sex,
           !(indicator_id == 93758 & Age != "16+ yrs"),
           !(indicator_id == 93014 & Age != "19+ yrs"),
           !(indicator_id == 93881 & Age != "18+ yrs"),
           !(indicator_id == 91871 & Age != "School age")) %>%
    select("IndicatorName", "AreaName", "Sex", "Timeperiod", "Value", "ComparedtoEnglandvalueorpercentiles")
  
  data_frames_list[[i]] <- i_data_frame
  
  cat("Processed indicator ID:", indicator_id, "\n")
}

combined_df <- bind_rows(data_frames_list)

combined_df <- combined_df %>%
  mutate(AreaName = str_remove_all(AreaName, " \\(statistical\\)")) %>%
  distinct()

#replaced filepaths to locations which do not exist in this project to relative filepaths
metadata <- read_csv("./metadata.csv")
metadata$Link <- paste0("<a href = '", metadata$Link, "'>", metadata$Link, "</a>")

LA_mappings <- read_csv("./LA_metadata.csv")

ethnicities <- read_csv("./population-by-ethnicity-and-local-authority-2021.csv")

population <- read_csv("./population-by-ethnicity-and-local-authority-2021.csv")

region_map <- read_csv("./Mappings.csv")

#replaced all references to "NHS_Region", which does not exist in the dataframe being used,
#to "Region", which does
combined_df_mapped <- left_join(
  combined_df, 
  region_map %>% select(AreaName, Region),
  by = "AreaName"
)

region_df <- left_join(
  combined_df,
  region_map,
  by = "AreaName"
)