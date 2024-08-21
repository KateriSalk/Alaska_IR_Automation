#Functions for data analysis package
#Functions to remove Cat 3
#Function for mag/freq/dur analysis

#Created by Hannah Ferriby

####Set up####

library(tidyverse)
library(sf)
library(zoo)
library(psych)

####Load in data####
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240819.csv') 
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240819.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240514.csv')
#Ammonia test file
ammonia_test <- read_csv('Output/data_analysis/ammonia_test_file.csv')

#Remove insufficient data combinations to lessen mdf analysis
filterCat3samples <- function(data_samples, data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    dplyr::filter(Data_Sufficient == 'Yes') %>%
    dplyr::select(AUID_ATTNS, TADA.CharacteristicName) %>%
    unique()
  
  samples <- data_samples %>% dplyr::right_join(suff_sites,
                                                by = join_by('AUID_ATTNS',
                                                             'TADA.CharacteristicName')) %>%
    dplyr::filter(!is.na(TADA.ResultMeasureValue))
  
  return(samples)
}

filterCat3sites <- function(data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    dplyr::filter(Data_Sufficient == 'Yes') %>%
    unique()
  
  return(suff_sites)
}

filterCat3samples(data_samples = input_samples
                  , data_sufficiency = input_sufficiency)
filterCat3sites(input_sufficiency)

input_samples_filtered <- filterCat3samples(data_samples = input_samples, data_sufficiency = input_sufficiency)


MagDurFreq <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency) {
  
  ##Magnitude, Frequency, Duration - unique combinations
  #This is not used in the code, but instead used as reference for making the methods
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent != 'Ammonia') %>%
    dplyr::filter(!(Constituent == 'Pentachloro-phenol' & `Waterbody Type` == 'Freshwater')) %>%
    dplyr::filter(Constituent != 'Turbidity') %>%
    dplyr::filter(!(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                       'Nickel', 'Silver', 'Zinc') & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' & #dec change: use = changed from aquatic life
                      `Waterbody Type` == 'Freshwater')) %>% #DEC change
    dplyr::select(Directionality, Frequency, Duration, Details) %>% 
    unique()
  
  # write_csv(unique_methods, 'Output/data_analysis/wqs_unique_methods.csv')
  
  # use AU_Type to choose Waterbody Type in WQS table
  Unique_AUIDs <- unique(input_samples_filtered$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # Filter data for just AU and make water year
    df_subset <- input_samples_filtered %>% 
      dplyr::filter(AUID_ATTNS == i) %>%
      mutate(year = lubridate::year(ActivityStartDate),
             month = lubridate::month(ActivityStartDate),
             w_year = ifelse(month < 10, year, year+1))
    
    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)
    
    # use AU_Type to choose Waterbody Type in data standards table
    if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake"){
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    } # end if/else statement
    
    # obtain unique constituents from WQ dataset for the AU
    my_constituents <- unique(df_subset$TADA.CharacteristicName)
    
    # trim data WQS table to only relevant information
    #remove information for instances found in the special case functions
    my_data_magfreqdur <- wqs_crosswalk %>% 
      dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text))  %>%
      dplyr::filter(Constituent != 'Ammonia') %>% #Filter out special cases
      dplyr::filter(Constituent != 'Pentachloro-phenol') %>%
      dplyr::filter(Constituent != 'Turbidity') %>%
      dplyr::filter(!(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                         'Nickel', 'Silver', 'Zinc') & 
                        Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' & #dec change: use = changed from aquatic life
                        `Waterbody Type` == 'Freshwater' & #DEC change
                        Type %in% c('Acute', 'Chronic'))) #DEC change
    
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      #Pull relevant methods
      filter_by <- my_data_magfreqdur[j,]
      
      #Pull just that constituent data
      if(is.na(filter_by$Fraction)) {
        
        filt <- df_subset %>%
          dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent) %>%
          dplyr::filter(is.na(TADA.ResultSampleFractionText_new))
        
      } else {
        
        filt <- df_subset %>%
          dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent) %>%
          dplyr::filter(TADA.ResultSampleFractionText_new == toupper(filter_by$Fraction))
      }
      
      if(nrow(filt) == 0) {
        next
      }
      
      #No decided method for analyzing sediment, set to Method not coded!
      if (filter_by$TADA.Constituent == 'SEDIMENT') {
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Requires manual analysis'
      }
      else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == '30-day period' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method #1 ----
        #Maximum, not to exceed, 30-day geometric mean
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::mutate(geo_mean_30d = zoo::rollapplyr(TADA.ResultMeasureValue, 
                                                       seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                                       psych::geometric.mean),
                        Exceed = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_30d)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Water year average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method #2 ----
        #Maximum, Not to exceed, water year average, geometric mean
        results <- filt %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(geo_mean_1yr = psych::geometric.mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(geo_mean_1yr >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_1yr)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      }else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10% of samples' &
               filter_by$Duration == 'Water year average') {
        #Method #3 ----
        #Maximum, 10% of samples, Water year average
        
        results <- filt %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(wyear_row = n(),
                        bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/wyear_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::select(w_year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == '30-day period' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T){
        #Method #4 ----
        #Not to exceed, 30 day geometric mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::mutate(geo_mean_30d = zoo::rollapplyr(TADA.ResultMeasureValue, 
                                                       seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                                       psych::geometric.mean),
                        Exceed = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_30d)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Water year average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method #5 ----
        #Maximum, not to exceed, geometric mean for water year
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(geo_mean_year = psych::geometric.mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(geo_mean_year >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_year) %>%
          dplyr::ungroup()
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Daily average') {
        #Method #6 ----
        #Maximum, not to exceed, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!daily_mean) %>%
          dplyr::ungroup()
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average'){
        #Method #7 ----
        #Maximum, 10% of samples, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                        bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>%
          dplyr::ungroup() %>%
          dplyr::select(year, bad_year) %>%
          unique()
        
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '1m Depth') == T){
        #Method #8 ----
        #Minimum, 10%, daily arithmetic mean, 1m Depth
        
        results <- filt %>%
          dplyr::filter(TADA.ActivityDepthHeightMeasure.MeasureValue <= 1) %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                        bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Estuaries and tidal tributaries') == T){
        #Method #9 ----
        #Minimum, 10%, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                        bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average'){
        #Method #10 ----
        #Minimum, 10%, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                        bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily minimum'){
        #Method #11 ----
        #Minimum, 10%, daily minimum
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_min = min(TADA.ResultMeasureValue)) %>%
          dplyr::mutate(day_row = length(unique(filt$ActivityStartDate)),
                        bad_samp = ifelse(daily_min <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily maximum'){
        #Method #12 ----
        #Maximum, 10%, daily Maximum
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_max = max(TADA.ResultMeasureValue)) %>%
          dplyr::mutate(day_row = length(unique(filt$ActivityStartDate)),
                        bad_samp = ifelse(daily_max >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Not to exceed'){
        #Method #13 ----
        #Maximum, 10%, not to exceed
        
        results <- filt %>%
          dplyr::mutate(num_samples = n(),
                        bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/num_samples>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Not to exceed'){
        #Method #14 ----
        #Maximum, not to exceed, not to exceed
        
        results <- filt %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Arithmetic mean' &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Frequency - harmonic mean of most recent 3 years') == T){
        #Method #15 ----
        #Maximum, not to exceed, arithmetic Mean of last 3 years
        
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::mutate(mean_samps = mean(TADA.ResultMeasureValue), 
                        bad_samp = ifelse(mean_samps >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == '30-day arithmetic average'){
        #Method #16 ----
        #Maximum, not to exceed, 30 day arithmetic mean
        results <- filt %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(mean_30_day = zoo::rollapplyr(TADA.ResultMeasureValue, 
                                                      seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                                      mean), 
                        bad_samp = ifelse(mean_30_day >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour max'){
        #Method #17 ----
        #Maximum, 1 instance in previous 3 years, 24 hour max
        
        max_year <- results %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(max_samps = max(TADA.ResultMeasureValue), 
                        bad_samp = ifelse(max_samps >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                (filter_by$Duration == '24-hour average' | filter_by$Duration == '24-hour arithmetic average') & is.na(filter_by$Details) == T){
        #Method #18 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 24 hour average
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue), 
                        bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                     ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                        #Give every samples a count of 1
                        r_count = 1.0,
                        #Total up number of samples in last 3 years
                        num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                   ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                        #Calculate exceedance frequency
                        Exceed_Freq = Exceedances/num_samples_3yrs,
                        #Determine if exceedance criteria met
                        tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Magnitude is minimum') == T){
        #Method #19 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude is min
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                                 ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                        bad_samp = ifelse(roll_4day_mean < filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                     ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                        #Give every samples a count of 1
                        r_count = 1.0,
                        #Total up number of samples in last 3 years
                        num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                   ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                        #Calculate exceedance frequency
                        Exceed_Freq = Exceedances/num_samples_3yrs,
                        #Determine if exceedance criteria met
                        tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method #20 ----
        #PROBLEM HERE
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude listed
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                                 ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                        bad_samp = ifelse(roll_4day_mean >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                     ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                        #Give every samples a count of 1
                        r_count = 1.0,
                        #Total up number of samples in last 3 years
                        num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                   ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                        #Calculate exceedance frequency
                        Exceed_Freq = Exceedances/num_samples_3yrs,
                        #Determine if exceedance criteria met
                        tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #21 ----
        #Maximum, 1 in most recent 3 years, 1 hour average
        #Could not do 1 hour average as some samples do not have a time recoreded
        #Assumed no samples were taken within 1 hour of each other at the same location
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == F &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #22 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average
        
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                                 ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                        bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                     ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                        #Give every samples a count of 1
                        r_count = 1.0,
                        #Total up number of samples in last 3 years
                        num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                   ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                        #Calculate exceedance frequency
                        Exceed_Freq = Exceedances/num_samples_3yrs,
                        #Determine if exceedance criteria met
                        tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F){ 
        #Method #23 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 1 hour average, magnitude listed
        
        #1-hour averages: use the daily value
        #(it will be rare that multiple samples are taken in a day, and if they are they would be considered duplicates)
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                     ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                        #Give every samples a count of 1
                        r_count = 1.0,
                        #Total up number of samples in last 3 years
                        num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                   ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                        #Calculate exceedance frequency
                        Exceed_Freq = Exceedances/num_samples_3yrs,
                        #Determine if exceedance criteria met
                        tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == F){ #DEC change
        #Method #24 ----
        #Maximum, 1 in most recent 3 years, 24 hour average
        
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0)) #DEC change
        
        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Method not coded!'
      }
      
      result_list[[counter]] <- filter_by
    } 
  } # end of for loop 
  
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  df_AU_data_WQS %>% dplyr::select(Exceed) %>% dplyr::group_by(Exceed) %>% dplyr::mutate(n = n()) %>% unique()
  
  #combine with relevant WQS table, removing the constituents that are calculated in other functions
  #these constituents come back in the hardness, pH, and turbidity specific functions
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(!(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                   'NICKEL', 'SILVER', 'ZINC') & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' & #dec change: use = changed from aquatic life
                      `Waterbody Type` == 'Freshwater' & Type %in% c('Acute', 'Chronic'))) %>% #DEC change
    dplyr::filter(TADA.CharacteristicName != 'AMMONIA') %>%
    dplyr::filter(TADA.CharacteristicName != 'PENTACHLORO-PHENOL') %>%
    dplyr::filter(TADA.CharacteristicName != 'TURBIDITY')
  
  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description', 'Waterbody Type', #DEC added Use Description
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())
  
  return(data_suff_WQS)
  
}


output <- MagDurFreq(wqs_crosswalk, input_samples_filtered, input_sufficiency)



MagDurFreq_hardnessDependent <- function(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency) {
  ##Magnitude, Frequency, Duration
  #Not used in code, but as a resource for creating/updating methods
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                     'Nickel', 'Silver', 'Zinc')) %>% 
    dplyr::filter(Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE') %>% #dec change: use = changed from aquatic life
    dplyr::filter(`Waterbody Type` == 'Freshwater') %>% 
    dplyr::filter(is.na(Magnitude_Numeric)) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  #Find just the required samples
  input_samples_filtered_relevant <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                 'NICKEL', 'SILVER', 'ZINC')) 
  
  
  #Return message if no samples available
  if(nrow(input_samples_filtered_relevant) == 0) {
    #If no samples available - just return sufficiency with empty Exceed column
    relevant_suff <- input_sufficiency %>%
      dplyr::filter(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                   'NICKEL', 'SILVER', 'ZINC')) %>% 
      dplyr::filter(Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE') %>% #dec change: use = changed from aquatic life
      dplyr::filter(`Waterbody Type` == 'Freshwater') %>% #changed 8-13-24
      dplyr::mutate(Exceed = NA)
    
    return(relevant_suff)
  }
  
  #Find unique AU IDs to cycle through
  Unique_AUIDs <- unique(input_samples_filtered_relevant$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # filter data
    df_subset <- input_samples_filtered_relevant %>% 
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::mutate(year = year(ActivityStartDate),
                    month = month(ActivityStartDate),
                    w_year = ifelse(month < 10, year, year+1))
    
    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)
    
    # use AU_Type to choose Waterbody Type in WQS table
    if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake"){
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    } # end if/else statement
    
    # obtain unique constituents from WQ dataset for the AU
    my_constituents <- unique(df_subset$TADA.CharacteristicName)
    
    # trim data WQS table to only relevant information
    my_data_magfreqdur <- wqs_crosswalk %>% 
      dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text)) %>%
      dplyr::filter(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                       'Nickel', 'Silver', 'Zinc')) %>% 
      dplyr::filter(Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' & Type %in% c('Acute', 'Chronic')) %>% #dec change: use = changed from aquatic life, added acute chronic
      dplyr::filter(`Waterbody Type` == 'Freshwater') #changed 8-13-24
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]
      
      if(is.na(filter_by$Fraction)) {
        
        filt <- df_subset %>%
          dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent) %>%
          dplyr::filter(is.na(TADA.ResultSampleFractionText_new))
        
      } else {
        
        filt <- df_subset %>%
          dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent) %>%
          dplyr::filter(TADA.ResultSampleFractionText_new == toupper(filter_by$Fraction))
      }
      
      if(nrow(filt) == 0) {
        next
      }
      
      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
         filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == T &
         stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #1 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes - NOT pH DEPENDENT
        #Chronic
        
        #Pull matching hardness samples from all samples
        hardness <- input_samples %>%
          dplyr::filter(AUID_ATTNS == i) %>%
          dplyr::filter(TADA.CharacteristicName == "HARDNESS") %>%
          dplyr::rename(Hardness = TADA.ResultMeasureValue,
                        Hardness.Date = ActivityStartDate) %>%
          dplyr::select(Hardness.Date, ActivityStartTime.Time, AUID_ATTNS, Hardness) %>%
          dplyr::group_by(Hardness.Date) %>%
          dplyr::reframe(Hardness.Date = Hardness.Date,
                         Hardness = mean(Hardness)) %>%
          unique()
        
        
        #Mark result as insufficient if no hardness available
        if(nrow(hardness) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- "Insufficient hardness"
        } else {
          #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
          if(filter_by$Constituent == 'Chromium (III)'){
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle            
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.819*(log(Hardness))+0.6848)*0.860)
            
          } else if(filter_by$Constituent == 'Copper') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8545*(log(Hardness))-1.702)*0.960)
            
          } else if(filter_by$Constituent == 'Lead') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle            
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.273*(log(Hardness))-4.705)*(1.46203-log(Hardness)*0.145712))
            
          } else if(filter_by$Constituent == 'Nickel') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle            
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.846*(log(Hardness))+0.0584)*0.997)
            
          } else if(filter_by$Constituent == 'Zinc') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle            
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.986)
            
          } else if(filter_by$Constituent == 'Cadmium') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle            
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.7409*(log(Hardness))-4.719)*(1.101672-log(Hardness)*0.041838))
            
          }
          
          results <- joined %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::arrange(ActivityStartDate) %>%
            dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                                   ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                          bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
          
          bad_tot <- results %>% 
            dplyr::ungroup() %>%
            dplyr::select(year, ActivityStartDate, bad_samp) %>%
            unique() %>%
            #If more than 2 exceedances in 3 years assign value of 1, else 0
            #Left bound is date - 3 years, right bound is date
            dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                       ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                          #Give every samples a count of 1
                          r_count = 1.0,
                          #Total up number of samples in last 3 years
                          num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                     ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                          #Calculate exceedance frequency
                          Exceed_Freq = Exceedances/num_samples_3yrs,
                          #Determine if exceedance criteria met
                          tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
          
          bad_sum <- sum(bad_tot$tot_exceed)
          
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        } 
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #2 ----
        #Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes 
        #Acute
        #Could not do 1 hour average as some samples do not have a time recorded
        #Assumed no samples were taken within 1 hour of each other at the same location
        
        #Pull matching hardness samples
        hardness <- input_samples %>%
          dplyr::filter(AUID_ATTNS == i) %>%
          dplyr::filter(TADA.CharacteristicName == "HARDNESS") %>%
          dplyr::rename(Hardness = TADA.ResultMeasureValue,
                        Hardness.Date = ActivityStartDate) %>%
          dplyr::select(Hardness.Date, ActivityStartTime.Time, AUID_ATTNS, Hardness)%>%
          dplyr::group_by(Hardness.Date) %>%
          dplyr::reframe(Hardness.Date = Hardness.Date,
                         Hardness = mean(Hardness)) %>%
          unique()
        #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
        
        #Mark result as insufficient if no hardness available
        if(nrow(hardness) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- "Insufficient hardness"
        } else {
          if(filter_by$Constituent == 'Chromium (III)'){
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle              
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.819*(log(Hardness))+3.7256)*0.316)
            
          } else if(filter_by$Constituent == 'Copper') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle              
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.9422*(log(Hardness))-1.700)*0.960)
            
          } else if(filter_by$Constituent == 'Lead') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle              
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.273*(log(Hardness))-1.460)*(1.46203-log(Hardness)*0.145712))
            
          } else if(filter_by$Constituent == 'Nickel') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle              
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.846*(log(Hardness))+2.255)*0.998)
            
          } else if(filter_by$Constituent == 'Zinc') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle              
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.978)
            
          } else if(filter_by$Constituent == 'Cadmium') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.0166*(log(Hardness))-3.924)*(1.136672-log(Hardness)*0.041838))
            
          }
          
          max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
          
          results <- joined %>%
            dplyr::filter(w_year >= max_year - 3) %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0))
          
          bad_tot <- results %>%
            dplyr::ungroup() %>%
            dplyr::select(bad_samp) %>%
            unique() 
          
          bad_sum <- sum(bad_tot$bad_samp)
          
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        } #End of hardness check
      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Method not coded!'
      } #End of methods if/else
      
      result_list[[counter]] <- filter_by
    } #End of MagDurFreq loop
    
  } #End of AU loop 
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  #combine with relevant data standards table
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                 'NICKEL', 'SILVER', 'ZINC')) %>% 
    dplyr::filter(Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' & Type %in% c('Acute', 'Chronic')) %>% #DEC change
    dplyr::filter(`Waterbody Type` == 'Freshwater')
  
  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description', 'Waterbody Type', #dec added Use Description
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())
  
  return(data_suff_WQS)
} #End of hardness dependent function

output_hardness <- MagDurFreq_hardnessDependent(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency)


#Just doing this to make it more similar to the standard and hardness dependent functions
# ammonia_test_filtered <- ammonia_test %>%
#   dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL'))
# 
# input_samples <- ammonia_test
# input_samples_filtered <- ammonia_test_filtered


#Function to tell you if you have sufficient freshwater ammonia samples and
#will need to change the function to add in fish presence

freshwaterAmmoniaWarning <- function(input_sufficiency){
  
  #Find freshwater ammonia
  ammonia <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName == 'AMMONIA') %>%
    dplyr::filter(`Waterbody Type` == 'Freshwater')
  
  if(nrow(ammonia) > 0) {
    return("There is sufficient freshwater ammonia in at least one AU. Please add fish data to analysis.")
  } else {
    return("There is NO sufficient freshwater ammonia. No changes requried for analysis.")
  }
}

freshwaterAmmoniaWarning(input_sufficiency)

MagDurFreq_pHDependent <- function(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency) {
  ##Magnitude, Frequency, Duration
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent %in% c('Ammonia') | 
                    (Constituent %in% c('Pentachloro-phenol') & `Waterbody Type` == 'Freshwater')) %>% 
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  input_samples_filtered_relevant <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL'))
  
  #Return message if no samples available
  if(nrow(input_samples_filtered_relevant) == 0) {
    #If no samples available - just return sufficiency with empty Exceed column
    relevant_suff <- input_sufficiency %>%
      dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL')) %>%
      dplyr::mutate(Exceed = NA)
    
    return(relevant_suff)
  }
  
  # use AU_Type to choose Waterbody Type in WQS table
  Unique_AUIDs <- unique(input_samples_filtered_relevant$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # dplyr::filter data
    df_subset <- input_samples_filtered_relevant %>% #CHANGE to input_samples_filtered_relevant
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::mutate(year = year(ActivityStartDate),
                    month = month(ActivityStartDate),
                    w_year = ifelse(month < 10, year, year+1))
    
    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)
    
    # use AU_Type to choose Waterbody Type in data sufficiency table
    if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake"){
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    } # end if/else statement
    
    # obtain unique constituents from WQ dataset for the AU
    my_constituents <- unique(df_subset$TADA.CharacteristicName)
    
    # trim data WQS table to only relevant information
    my_data_magfreqdur <- wqs_crosswalk %>% 
      dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text)) %>%
      dplyr::filter(Constituent %in% c('Ammonia', 'Pentachloro-phenol')) 
    
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]
      
      if(is.na(filter_by$Fraction)) {
        
        filt <- df_subset %>%
          dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent) %>%
          dplyr::filter(is.na(TADA.ResultSampleFractionText_new))
        
      } else {
        
        filt <- df_subset %>%
          dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent) %>%
          dplyr::filter(TADA.ResultSampleFractionText_new == toupper(filter_by$Fraction))
      }
      
      if(nrow(filt) == 0) {
        next
      }
      
      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
         filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == T){
        #Method #1 ----
        #Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Acute
        #Could not do 1 hour average as some samples do not have a time recoreded
        #Assumed no samples were taken within 1 hour of each other at the same location
        
        #Pull matching pH 
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()
        
        if(filter_by$Constituent == 'Pentachloro-phenol') {
          #Combine matching pH & calculate magnitude
          #Join with pH - needs to be at least one pH value present during IR cycle
          match_dates <- filt %>%
            dplyr::left_join(pH, by = join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))
          
          #Equation from WQS sheet 'Details'
          joined <- match_dates %>%
            dplyr::mutate(magnitude = exp(1.005*pH-4.869)) 
          
        } else {#Acute freshwater ammonia
          
          match_dates <- filt %>%
            dplyr::left_join(pH, by = join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))
          
          #Equation from Toxics Manual Appendix C - no salmonids
          joined <- match_dates %>%
            dplyr::mutate(magnitude = ((0.411/(1+10^(7.204-pH)))+(58.4/(1+10^(pH-7.204)))))
          
        }
        
        if(nrow(joined) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Insufficient dependent data'
        } else {
          max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()
          
          results <- joined %>%
            dplyr::filter(w_year >= max_year - 3) %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0)) 
          
          bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
          bad_sum <- sum(bad_tot$bad_samp)
          
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        }
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '30-day average' & is.na(filter_by$Magnitude_Numeric) == T){
        #Method #2 ----
        #Chronic, freshwater ammonia 
        #Maximum, 2 or more exceedances and >5% exceedance frequency in 3 year period for 30-day averages
        
        #Pull matching pH 
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()
        
        #Pull matching temperature
        temp <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
          dplyr::rename(temp = TADA.ResultMeasureValue,
                        temp.Date = ActivityStartDate) %>%
          dplyr::select(temp.Date, ActivityStartTime.Time, AUID_ATTNS, temp) %>%
          dplyr::group_by(temp.Date) %>%
          dplyr::reframe(temp.Date = temp.Date,
                         temp = mean(temp)) %>%
          unique()
        
        #Get nearest pH sample to ammonia sample date
        match_dates1 <- filt %>%
          dplyr::left_join(pH, by = join_by(closest(ActivityStartDate >= pH.Date))) %>%
          dplyr::filter(!is.na(pH.Date))
        
        #Get only same-day matches for temp and ammonia
        match_dates <- match_dates1 %>%
          dplyr::inner_join(temp, by = dplyr::join_by(ActivityStartDate == temp.Date))
        
        if(nrow(match_dates) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Insufficient dependent data'
        } else {
          #Calculate the min() part of the equation from Toxics Manual Appendix D
          match_dates_w_min <- match_dates %>%
            mutate(Min_Value = ifelse(1.45*10^(0.028*(25-temp)) > 2.85, 2.85, 1.45*10^(0.028*(25-temp))))
          
          #Calculate magnitude, rolling 30 day average, and if something is a "bad sample" 
          joined <- match_dates_w_min %>%
            dplyr::arrange(ActivityStartDate) %>%
            #Equation from Toxics Manual Appendix D
            dplyr::mutate(magnitude = ((0.0577/(1+10^(7.688-pH)))+(2.487/(1+10^(pH-7.688))))*Min_Value,
                          #Rolling 30 day average
                          average_30_day = zoo::rollapplyr(TADA.ResultMeasureValue, 
                                                           seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                                           mean),
                          bad_samp = ifelse(average_30_day >= magnitude, 1, 0))
          
          bad_tot <- joined %>% 
            dplyr::ungroup() %>%
            dplyr::select(w_year, ActivityStartDate, bad_samp) %>%
            unique() %>%
            #If more than 2 exceedances in 3 years assign value of 1, else 0
            #Left bound is date - 3 years, right bound is date
            dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                       ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                          #Give every samples a count of 1
                          r_count = 1.0,
                          #Total up number of samples in last 3 years
                          num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                     ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                          #Calculate exceedance frequency
                          Exceed_Freq = Exceedances/num_samples_3yrs,
                          #Determine if exceedance criteria met
                          tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
          
          bad_sum <- sum(bad_tot$tot_exceed)
          
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        }
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == T) {
        #Method #3 ----
        #Maximum, 2 or more exceedances or >5% exceedance frequency in 3 years, 96-hour average (4 day)
        #Chronic Pentachloro-phenol
        #Marine chronic Ammonia
        #Pull matching pH 
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()
        
        if(filter_by$Constituent == 'Pentachloro-phenol') {
          #Combine matching pH & calculate magnitude - just need one pH value for IR cycle
          match_dates <- filt %>%
            dplyr::left_join(pH, by = join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))
          
          #Equation from WQS sheet 'Details'
          joined <- match_dates %>%
            dplyr::mutate(magnitude = exp(1.005*pH-5.134)) 
          
          if(nrow(joined) == 0) {
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- 'Insufficient dependent data'
          } else {
            
            max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()
            
            results <- joined %>%
              dplyr::filter(w_year >= max_year - 3) %>%
              dplyr::group_by(ActivityStartDate) %>%
              dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate,
                                                     ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])),
                            bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0))
            
            bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
            bad_sum <- sum(bad_tot$bad_samp)
            
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
          }
          
        } else {#Chronic marine ammonia
          #Pull matching temperature
          temp <- input_samples %>%
            dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
            dplyr::rename(temp = TADA.ResultMeasureValue,
                          temp.Date = ActivityStartDate) %>%
            dplyr::select(temp.Date, ActivityStartTime.Time, AUID_ATTNS, temp) %>%
            dplyr::group_by(temp.Date) %>%
            dplyr::reframe(temp.Date = temp.Date,
                           temp = mean(temp)) %>%
            unique()
          
          
          #Pull matching salinity
          salinity <- input_samples %>%
            dplyr::filter(TADA.CharacteristicName == "SALINITY") %>%
            dplyr::rename(salinity = TADA.ResultMeasureValue,
                          salinity.Date = ActivityStartDate) %>%
            dplyr::select(salinity.Date, ActivityStartTime.Time, AUID_ATTNS, salinity) %>%
            dplyr::group_by(salinity.Date) %>%
            dplyr::reframe(salinity.Date = salinity.Date,
                           salinity = mean(salinity)) %>%
            unique()
          
          #Join ammonia with pH - needs to be at least one pH value present during IR cycle
          match_dates1 <- filt %>%
            dplyr::left_join(pH, by = join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))
          
          #Join ammonia with salinity - needs to be at least one salinity value present during IR cycle
          match_dates2 <- match_dates1 %>%
            dplyr::left_join(salinity, by = join_by(closest(ActivityStartDate >= salinity.Date))) %>%
            dplyr::filter(!is.na(salinity.Date))
          
          #Get only same-day matches for temp and ammonia
          match_dates <- match_dates2 %>%
            dplyr::inner_join(temp, by = dplyr::join_by(ActivityStartDate == temp.Date)) 
          
          #Table from Toxics Manual Appendix G
          cross_table <- readr::read_csv("Data/data_analysis/chronic_marine_ammonia.csv") %>%
            dplyr::mutate(Salinity = DescTools::RoundTo(Salinity, 10000000),
                          Salinity = as.character(Salinity),
                          Temperature = as.character(Temperature),
                          pH = as.character(pH))
          
          #Convert numerics to character in order to get join to work 
          joined <- match_dates %>%
            dplyr::mutate(salinity_round = DescTools::RoundTo(salinity, 10000000),
                          temp_round = DescTools::RoundTo(temp, 5),
                          pH_round = DescTools::RoundTo(pH, 0.2),
                          salinity_round = as.character(salinity_round),
                          temp_round = as.character(temp_round),
                          pH_round = as.character(pH_round)) %>%
            dplyr::left_join(cross_table, by = dplyr::join_by(salinity_round == Salinity,
                                                              temp_round == Temperature,
                                                              pH_round == pH)) %>%
            dplyr::filter(!is.na(Magnitude)) #Get rid of values with no Magnitude match
          
          if(nrow(joined) == 0) {
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- 'Insufficient dependent data'
          } else {
            
            max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()
            
            results <- joined %>%
              dplyr::filter(w_year >= max_year - 3) %>%
              dplyr::group_by(ActivityStartDate) %>%
              dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate,
                                                     ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])),
                            bad_samp = ifelse(roll_4day_mean >= Magnitude, 1, 0))
            
            bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
            bad_sum <- sum(bad_tot$bad_samp)
            
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
          }
          
        }
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '30-day average' & is.na(filter_by$Magnitude_Numeric) == T) {
        #Method #4 ----
        #Acute marine ammonia
        
        #Pull matching pH 
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()
        #Pull matching temperature
        temp <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
          dplyr::rename(temp = TADA.ResultMeasureValue,
                        temp.Date = ActivityStartDate) %>%
          dplyr::select(temp.Date, ActivityStartTime.Time, AUID_ATTNS, temp) %>%
          dplyr::group_by(temp.Date) %>%
          dplyr::reframe(temp.Date = temp.Date,
                         temp = mean(temp)) %>%
          unique()
        
        
        #Pull matching salinity
        salinity <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "SALINITY") %>%
          dplyr::rename(salinity = TADA.ResultMeasureValue,
                        salinity.Date = ActivityStartDate) %>%
          dplyr::select(salinity.Date, ActivityStartTime.Time, AUID_ATTNS, salinity) %>%
          dplyr::group_by(salinity.Date) %>%
          dplyr::reframe(salinity.Date = salinity.Date,
                         salinity = mean(salinity)) %>%
          unique()
        
        #Match ammonia with pH - only need one pH sample for IR cycle
        match_dates1 <- filt %>%
          dplyr::left_join(pH, by = join_by(closest(ActivityStartDate >= pH.Date))) %>%
          dplyr::filter(!is.na(pH.Date))
        
        #Match ammonia with salinity - only need one salinity sample for IR cycle
        match_dates2 <- match_dates1 %>%
          dplyr::left_join(salinity, by = join_by(closest(ActivityStartDate >= salinity.Date))) %>%
          dplyr::filter(!is.na(salinity.Date))
        
        #Get only same-day matches for temp and ammonia
        match_dates <- match_dates2 %>%
          dplyr::inner_join(temp, by = dplyr::join_by(ActivityStartDate == temp.Date)) 
        
        #Table from Toxics Manual Appendix F
        cross_table <- readr::read_csv("Data/data_analysis/acute_marine_ammonia.csv") %>%
          dplyr::mutate(Salinity = DescTools::RoundTo(Salinity, 10000000),
                        Salinity = as.character(Salinity),
                        Temperature = as.character(Temperature),
                        pH = as.character(pH))
        
        #Convert numerics to character in order to get join to work 
        joined <- match_dates %>%
          dplyr::mutate(salinity_round = DescTools::RoundTo(salinity, 10000000),
                        temp_round = DescTools::RoundTo(temp, 5),
                        pH_round = DescTools::RoundTo(pH, 0.2),
                        salinity_round = as.character(salinity_round),
                        temp_round = as.character(temp_round),
                        pH_round = as.character(pH_round)) %>%
          dplyr::left_join(cross_table, by = dplyr::join_by(salinity_round == Salinity,
                                                            temp_round == Temperature,
                                                            pH_round == pH)) %>%
          dplyr::filter(!is.na(Magnitude)) #Get rid of values with no Magnitude match
        
        if(nrow(joined) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Insufficient dependent data'
        } else {
          
          max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()
          
          results <- joined %>%
            dplyr::filter(w_year >= max_year - 3) %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::mutate(roll_30day_mean = map_dbl(ActivityStartDate,
                                                    ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(30), .x)])),
                          bad_samp = ifelse(roll_30day_mean >= Magnitude, 1, 0))
          
          bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
          bad_sum <- sum(bad_tot$bad_samp)
          
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        } 
      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Method not coded!'
      } #End of methods if/else
      
      result_list[[counter]] <- filter_by
    } #End of MagDurFreq loop
    
  } #End of AU loop 
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  #combine with relevant data standards table
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL')) 
  
  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description', 'Waterbody Type', #DEC added use description
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())
  
  return(data_suff_WQS)
} #End of pH dependent function

# output_pH <- MagDurFreq_pHDependent(wqs_crosswalk, ammonia_test, ammonia_test_filtered, input_sufficiency)
output_pH <-  MagDurFreq_pHDependent(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency)


combine_MagDurFreq <- function(standard_output, hardness_output, pH_output, turbidity_output) {
  output <- standard_output %>%
    rbind(hardness_output) %>%
    rbind(pH_output) %>%
    rbind(turbidity_output)
    
    return(output)
}

final_output <- combine_MagDurFreq(output, output_hardness, output_pH, output_turbidity)

write_csv(final_output, 'Output/data_analysis/final_magdurfreq_output_20240819.csv')
