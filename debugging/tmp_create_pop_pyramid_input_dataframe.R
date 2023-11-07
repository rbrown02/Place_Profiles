#temp for debug - make dataset used by plot function

#read in data from Fingertips API
  #select Indicator ID
  #and an AreaType ID coresponding to a choice of area_type (in the Shiny UI)
#Then filter to the max TimePeriodSortable (to get the most recent data point)
#this gives us pop_data

area_type <- 402 #this is the fingertips code for "Counties & UAs"
  #there is a bit of code at the head of the main app code which chooses a code based on a human readable
  #input string of this type
  #but it's no in the Shiny UI - it's just a bit of control flow before then, so effectively hard coded

pop_data <- fingertips_data(IndicatorID = 92708,
                            AreaTypeID = area_type) %>% 
  filter(TimeperiodSortable == max(TimeperiodSortable))

#now read in pop_region_map from a flat file
  #and drop the Region column
pop_region_map <- read_csv("./Mappings.csv") %>% 
  select(-Region)

#Now join the pop_data to the pop_region_map on the AreaName column
#This gives you pop_chart_data_reg
pop_chart_data_reg <- merge(x=pop_data, y=pop_region_map, by="AreaName", all.x=TRUE)

#Now filter on
  #Age
  #Timeperiod
  #Sex
  #AreaName
#Then turn the age values into factors
#This gives us filtered_pop_data - the input into the pyramid plot function

year <- unique(pop_data$Timeperiod)
selected_region <- "London region"
selected_area <- "Hackney"

filtered_pop_data <-
  pop_chart_data_reg %>% 
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






