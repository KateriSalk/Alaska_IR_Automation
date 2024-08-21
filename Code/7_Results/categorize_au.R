#Assign AU categories based on exceedance values

#Written by: Hannah Ferriby

#Load Packages
library(tidyverse)

####Load in data####
input_analysis <- read_csv('Output/data_analysis/final_magdurfreq_output_20240819.csv')

assessments <- read_csv('Data/data_analysis/assessments.csv') %>%
  select(ASSESSMENT_UNIT_ID = assessmentUnitId,
         PARAM_NAME = parameterName,
         ATTAINS_USE = useName,
         PARAM_STATUS_NAME = parameterStatus,
         PARAM_ATTAINMENT_CODE = parameterAttainment)

#Don't want to overwrite previous ATTAINs 2s and 5s with new 3s
merge_uses <- input_analysis %>%
  mutate(ATTAINS_waterbody = case_when(`Waterbody Type` == 'Freshwater' ~
                                         'FRESH WATER',
                                       T ~ 'MARINE WATER'),
         ATTAINS_USE_merge = paste0(ATTAINS_waterbody, ' / ',  Use, ' / ', `Use Description`)) %>%
  left_join(assessments, by = c('AUID_ATTNS' = 'ASSESSMENT_UNIT_ID',
                                'TADA.CharacteristicName' = 'PARAM_NAME', 
                                'ATTAINS_USE_merge' = 'ATTAINS_USE')) %>%
  mutate(PARAM_ATTAINMENT_CODE_new = case_when(PARAM_ATTAINMENT_CODE == "Not meeting criteria" ~ 
                                             '5', 
                                             PARAM_ATTAINMENT_CODE == "Meeting criteria" ~
                                             '2', 
                                             PARAM_ATTAINMENT_CODE == "Not enough information" ~
                                             '3', 
                                           T ~
                                             NA))

categorize_AU_uses <- function(input_analysis, simplify_standards){ 
  
  calc_individual <- input_analysis %>%
    mutate(Exceed = ifelse(is.na(Exceed) == T, 'N/A', Exceed)) %>%
    filter(Exceed != 'Requires manual analysis') %>%
    filter(Exceed != 'AU not lake waters') %>%
    filter(Exceed != 'Natural conditions less than or equal to 50 NTU') %>%
    dplyr::mutate(Individual_Category = case_when(is.na(Data_Sufficient) ~ NA,
                                                  (PARAM_ATTAINMENT_CODE_new != '3' &
                                                    Data_Sufficient == 'No') ~
                                                    PARAM_ATTAINMENT_CODE_new,
                                                  Data_Sufficient == "No" ~ '3',
                                                  Exceed == 'Yes' ~ '5',
                                                  Exceed == 'No' ~ '2',
                                                  Exceed == 'Insufficient hardness' ~ '3',
                                                  Exceed == 'Insufficient dependent data' ~ '3',
                                           T ~ NA))
  
  if(simplify_standards == T){
    
    mid_step <- calc_individual %>%
      dplyr::group_by(AUID_ATTNS, Use, `Use Description`, TADA.CharacteristicName) %>%
      dplyr::mutate(n = n(),
             is_2 = sum(ifelse(Individual_Category == '2', 1, 0)),
             is_3 = sum(ifelse(Individual_Category == '3', 1, 0)),
             is_5 = sum(ifelse(Individual_Category == '5', 1, 0)),
             #If n > 1, choose worse category
             new_Individual_Category = case_when(n > 1 & is_5 == 1 ~
                                                   '5',
                                                 n > 1 & is_5 == 0 & is_2 > 0 ~
                                                   '2',
                                                 T ~ Individual_Category)) %>%
      dplyr::filter(Individual_Category == new_Individual_Category) %>%
      dplyr::select(!c(Individual_Category, n, is_2, is_3, is_5)) %>%
      dplyr::rename(Individual_Category = new_Individual_Category)
      
    
  } else {
    mid_step <- calc_individual
  }
  
  
  calc_overall <- mid_step %>%
    dplyr::group_by(AUID_ATTNS, Use, `Use Description`) %>%
    dplyr::mutate(cat_5_present = length(Individual_Category[Individual_Category=='5']),
           cat_2_present = length(Individual_Category[Individual_Category=='2']),
           Use_Category = case_when(cat_5_present > 0 ~ '5',
                                        cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                        cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                        T~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS, Use)
  
  return(calc_overall)
  
}

output <- categorize_AU_uses(merge_uses, simplify_standards = F)
output_simp <- categorize_AU_uses(merge_uses, simplify_standards = T)

write_csv(output, 'Output/results/categorized_aus_20240819.csv')
write_csv(output_simp, 'Output/results/categorized_simplified_aus_20240819.csv')



categorize_AU <- function(input_categorized_uses){ 

  calc_overall <- input_categorized_uses %>%
    dplyr::group_by(AUID_ATTNS) %>%
    dplyr::mutate(cat_5_present = length(Use_Category[Use_Category=='5']),
                  cat_2_present = length(Use_Category[Use_Category=='2']),
                  Overall_Category = case_when(cat_5_present > 0 ~ '5',
                                           cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                           cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                           T~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS)
  
  return(calc_overall)

}


output_overall <- categorize_AU(output)
output_simp_overall <- categorize_AU(output_simp)
