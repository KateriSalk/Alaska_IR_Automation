#Create summary HTML documents for all relevant AUs for AKDEC

#Created by: Hannah Ferriby
#Updated on: 6/28/2024

####Load libraries####
library(readr)
library(dplyr)
library(sf)
library(rmarkdown)


####Read in data####
output_df <- read_csv(file = "Output/results/categorized_simplified_aus_20240819.csv")  # path to file
wqs_table <- read_csv(file = 'Data/data_analysis/AK_WQS_Crosswalk_20240514.csv')
output_samples <- read_csv(file = 'Output/data_processing/WQ_data_trimmed_long_withAU20240819.csv')
ml_au_crosswalk <- read_csv(file = 'Data/data_processing/ML_AU_Crosswalk20240809.csv')
au_shape_crs <-st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/marine.shp') %>%
  st_crs()

####Find Number of AUs for loop####
unique_AU <- output_df %>%
  select(AUID_ATTNS) %>%
  unique()


####Required functions####
#Box plot function
boxPlot <- function(data, WQS_table, AU_ID) {
  
  sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
  showtext::showtext_auto() # for fonts
  
  relevant_constituents <- WQS_table %>%
    dplyr::select(TADA.Constituent) %>%
    unique() %>%
    dplyr::pull()
  
  relevant_data <- data %>%
    dplyr::filter(TADA.CharacteristicName %in% relevant_constituents) %>%
    dplyr::filter(AUID_ATTNS == AU_ID)
  
  constituents <- relevant_data %>%
    dplyr::select(TADA.CharacteristicName) %>%
    unique() %>%
    dplyr::pull()
  
  results <- list()
  counter <- 0
  #Loop through constituents
  for(j in constituents) {
    
    counter<- counter+1
    
    filt <- relevant_data %>%
      dplyr::filter(TADA.CharacteristicName == j)
    
    plt<-ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = filt,
                            ggplot2::aes(x = AUID_ATTNS,
                                         y = TADA.ResultMeasureValue),
                            color = 'gray30',
                            outlier.shape = NA) +
      ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                                     y = TADA.ResultMeasureValue,
                                                     fill = MonitoringLocationIdentifier),
                           color = 'black',
                           shape = 21,
                           size = 3.5,
                           width = 0.2,
                           alpha = 0.8) +
      ggplot2::xlab('AU ID') +
      ggplot2::ylab(paste0(str_to_title(j), ' (', tolower(filt$TADA.ResultMeasure.MeasureUnitCode), ')')) +
      ggplot2::scale_y_log10() +
      ggplot2::theme_bw() +
      viridis::scale_fill_viridis(discrete = T,
                                  option = "mako") +
      ggplot2::labs(fill = 'Monitoring Location ID') +
      ggplot2::theme(legend.position="top"
                     , legend.spacing.x = unit(0.5, 'cm')
                     , text = ggplot2::element_text(family = "Open_Sans", size = 24)
                     , axis.text = ggplot2::element_text(family = "Open_Sans", size = 22)
                     , legend.background = element_rect(colour = 'gray60', fill = 'white', linetype='dashed')
                     , plot.margin = unit(c(0.5,0.25,0.5,0.25), "cm")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3),
                                                   byrow=TRUE,
                                                   title.position="top",
                                                   title.hjust = 0.5))

    results[[counter]] <- plt
  }
  return(results)
}

#Time Series function
timeSeries <- function(data, WQS_table, AU_ID) {
  sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
  showtext::showtext_auto() # for fonts
  
  relevant_constituents <- WQS_table %>%
    dplyr::select(TADA.Constituent) %>%
    unique() %>%
    dplyr::pull()
  
  relevant_data <- data %>%
    dplyr::filter(TADA.CharacteristicName %in% relevant_constituents) %>%
    dplyr::filter(AUID_ATTNS == AU_ID)
  
  constituents <- relevant_data %>%
    dplyr::select(TADA.CharacteristicName) %>%
    unique() %>%
    dplyr::pull()
  
  results <- list()
  counter <- 0
  #Loop through constituents
  for(j in constituents) {
    
    counter<- counter+1
    
    filt <- relevant_data %>%
      dplyr::filter(TADA.CharacteristicName == j)
    
    plt<-ggplot2::ggplot() +
      ggplot2::geom_point(data = filt,
                          ggplot2::aes(x = ActivityStartDate,
                                       y = TADA.ResultMeasureValue,
                                       fill = MonitoringLocationIdentifier),
                          color = 'black',
                          shape = 21,
                          size = 3.5,
                          alpha = 0.8) +
      ggplot2::xlab('Time') +
      ggplot2::scale_y_log10() +
      ggplot2::ylab(paste0(str_to_title(j), ' (', tolower(filt$TADA.ResultMeasure.MeasureUnitCode), ')')) +
      ggplot2::theme_bw() +
      viridis::scale_fill_viridis(discrete = T,
                                  option = "mako") +
      ggplot2::labs(fill = 'Monitoring Location ID') +
      ggplot2::theme(legend.position="top"
                     , legend.spacing.x = unit(0.5, 'cm')
                     , text = ggplot2::element_text(family = "Open_Sans", size = 24)
                     , axis.text = ggplot2::element_text(family = "Open_Sans", size = 22)
                     , legend.background = element_rect(colour = 'gray60', fill = 'white', linetype='dashed')
                     , plot.margin = unit(c(0.5,0.25,0.5,0.25), "cm")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3),
                                                   byrow=TRUE,
                                                   title.position="top",
                                                   title.hjust = 0.5))
    results[[counter]] <- plt
  }
  return(results)
}

#### Loop ####
au_loop <- unique_AU$AUID_ATTNS
# au_loop <- unique_AU$AUID_ATTNS[1:15]
# au_loop <- 'AK_M_1030305_003'
# au_loop <- 'AK_B_1010203_001'
# au_loop <- 'AK_M_2020120_011'
no_html <- list()
count <- 1

for (au_loop in unique_AU$AUID_ATTNS){  # for each unique episode
  
  # Isolate AU samples from the dataset
  
  output_df_au <- output_df[output_df$AUID_ATTNS == au_loop, ]  
  au_type <- unique(output_df_au$AU_Type) %>%
    na.omit()
  
  if(au_type == 'Marine') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/marine.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      mutate(Shape_4_Summary = AU_Area,
             AU_Shape_Unit = 'square miles')
    
  } else if(au_type == 'River') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/rivers.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs)) %>%
      mutate(Shape_4_Summary = AU_Miles,
             AU_Shape_Unit = 'miles')
    
  } else if(au_type == 'Beach') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/beaches.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs)) %>%
      mutate(Shape_4_Summary = AU_Miles,
             AU_Shape_Unit = 'miles')
    
  } else if(au_type == 'Lake') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/lakes.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs)) %>%
      mutate(Shape_4_Summary = AU_Area,
             AU_Shape_Unit = 'acres')
    
  } else(
    print("No AU Type Detected")
  )
  
  
  sites <- ml_au_crosswalk %>%
    filter(AUID_ATTNS == au_loop) %>%
    select(MonitoringLocationIdentifier, Latitude, Longitude) %>%
    unique() %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), remove = F) %>%
    st_set_crs(4617)  %>% #NAD83 EPSG
    st_transform(st_crs(au_shape))
    
  if(dim(sites)[1] == 0) { #If no sites in AU/ML crosswalk, move on
    no_html[count] <- au_loop
    count <- count + 1
    next
  }
  
  if(dim(au_shape)[1] == 0) { #If no shapefile, move on
    no_html[count] <- au_loop
    count <- count + 1
    next
  }
  
  # Now render ('knit') the R Markdown file to html format, name it and save it
  
  render(
    input = "Code/8_Stakeholder_Communication/summary_html_template.rmd",  # path to the template
    output_file = paste0("au_", au_loop, ".html"),  # name the output
    output_dir = "Output/results/summary_htmls"  # folder in which to put the output file
  )
  
}  # end of loop


