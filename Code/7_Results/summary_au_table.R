#Function to create table with AU-specific details for the ArcGIS online
#hub page

#Written by: Hannah Ferriby
#Date updated: 6/28/2024

#Required libraries
library(tidyverse)
library(readxl)
library(sf)

#Load in categorized AU information
categorize_output <- read_csv('Output/results/categorized_aus_20240819.csv')

#Select just the needed columns
table_cat <- categorize_output %>% 
  select(AUID_ATTNS, `Waterbody Type`, Use, Use_Category) %>%
  unique()

#Load in AU shapefiles
lake_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/lakes.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Area) %>%
  st_zm() %>%
  mutate(Shape_4_Summary = AU_Area,
         AU_Shape_Unit = 'acres') %>%
  select(!AU_Area)

river_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/rivers.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Miles) %>%
  st_zm() %>%
  mutate(Shape_4_Summary = AU_Miles,
         AU_Shape_Unit = 'miles') %>%
  select(!AU_Miles)

marine_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/marine.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Area) %>%
  st_zm() %>%
  st_transform(crs = st_crs(lake_aus)) %>%
  mutate(Shape_4_Summary = AU_Area,
         AU_Shape_Unit = 'square miles') %>%
  select(!AU_Area)#Marine AU is in NAD83/Alaska Albers
#All other shapefiles are in WGS 84/Pseudo-Mercator

beach_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/beaches.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Miles) %>%
  mutate(Shape_4_Summary = AU_Miles,
         AU_Shape_Unit = 'miles') %>%
  select(!AU_Miles)

#Combine aus into one large table, add HUC10 if missing
all_aus <- lake_aus %>%
  rbind(river_aus) %>%
  rbind(marine_aus) %>%
  rbind(beach_aus) 

#Join with AU shapefile information
data_all_AUs <- table_cat %>% 
  left_join(all_aus, by = 'AUID_ATTNS') 

#Summarize data
final_summary <- data_all_AUs %>%
  mutate(overallStatus = case_when(Use_Category == 2 ~ 'Fully Supporting',
                                   Use_Category == 3 ~ 'Not Assessed',
                                   Use_Category == 5 ~ 'Not Supporting',
                                   Use_Category == '4a' ~ 'Not Supporting',
                                   Use_Category == '4b' ~ 'Not Supporting',
                                   T ~ NA)) %>% 
  select(AUID_ATTNS, `Waterbody Type`, Use, Use_Category, overallStatus, 
         Name_AU, Shape_4_Summary, AU_Shape_Unit) %>%
  unique() 

write_csv(final_summary, 'Output/results/summary_au_tables_20240819.csv')