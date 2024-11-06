This repository contains the code to generate the Place Profiles dashaboard - hosted on Shiny IO (https://ovnnt7-rbrown02.shinyapps.io/Place_Profiles/).

This dashboard has been produced for convenience, and to show how you can represent this publicly available data, it is not a monitored or validated tool - use at your own peril!

Fingertips is a large collection of public health data (managed by OHID). It provides easy access to a rich source of indicators across a range of health and wellbeing topics. This is publicly available data and on the website, it is organised into themed profiles. Using the web-based application provides a good way to view and compare organisations within themed profiles but there are limitations when it comes to looking at metrics across a number of themes.

The metrics from Fingertips are shown in the dashboard at a geographical level of local authority. The sidebar allows users to view national, regional and local authority data. To view data for a given local authority ‘Select region’ where the local authority and then use the ‘Select area’ drop down. The dashboard also allows you to either compare local authority values to the national values or to the value for the region the local authority sits under by using the ‘Select comparator’ drop down.

Dashboard Navigation
Demographics: key demographic information including population by age and sex; ethnicity breakdown; deprivation scores and various life expectancy metrics
Education, Employment and Households: provides metrics on school readiness and attainment, unemployment, homelessness, income deprivation, fuel poverty and loneliness
Risk Factors: provides metrics on physical activity, alcohol and smoking, obesity, children with social/emotional/mental health needs and tooth decay in children
Download: enables export of regional data in csv format
Metadata: lists all measures in dashboard and provides link to Fingertips site
Mappings: map of ICBs to local government areas. Some areas sit across multiple ICBs therefore the mapping is not perfect and includes which partial government areas fall under ICBs
Dashboard Development
This dashboard was developed by the NECS Consultancy Analytics Team. To access the source code please refer to my Github page: https://github.com/rbrown02

All data is from OHID’s Fingertips platform and has been pulled into the dashboard through an API using the fingertipsR package ( https://github.com/ropensci/fingertipsR ). (Westermann A, Fox S, Nanayakkara H, Flowers J (2024). fingertipsR: Fingertips Data for Public Health). R package version 1.0.12. The population pyramid was also created by code developed by the PHE team's fingertipsCharts package.

