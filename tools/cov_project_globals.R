# This script provides globals and import functions 

# toolbox ----

  source('./tools/sys_tools.R')

  library(readr)
  library(readxl)
  library(tibble)
  library(ggplot2)
  library(stringi)


# Data containers ----

  globals <- list()

# graphics theme -----

  globals$present_scale <- c(yes = 'firebrick4', 
                             no = 'steelblue4')
  
  globals$reg_scale <- c(positive = 'firebrick3', 
                         negative = 'steelblue3', 
                         ns = 'gray60')
  
  globals$common_text <- element_text(size = 8, 
                                      face = 'plain', 
                                      color = 'black')
  
  globals$common_margin <- margin(t = 5, 
                                  l = 5, 
                                  r = 5, 
                                  unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold'), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0, 
                                                                          vjust = 1), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'black'), 
                                                  plot.margin = globals$common_margin)

# Co-morbidities -----
  
  globals$comorb <- c('autoimmunity', 
                      'anemia', 
                      'hypertension', 
                      'depression_burnout',
                      'diabetes', 
                      'epilepsy',
                      'dementia', 
                      'surgery_6m_before', 
                      'frequent_flu_like', 
                      'heart_circulation', 
                      'hay_fever', 
                      'malignancy', 
                      'pins_needles_feet', 
                      'gastrointenstinal', 
                      'lung', 
                      'two_plus_infections_antibiotics', 
                      'multiple_sclerosis', 
                      'night_dyspnoe', 
                      'bruxism', 
                      'kidney', 
                      'parkinson', 
                      'insomnia', 
                      'stroke', 
                      'embolism', 
                      'transplantation')

# Daily medication -----
  
  globals$daily_medication <- c('daily_immunosuppression', 
                                'daily_cortison', 
                                'daily_ace_drugs', 
                                'daily_pain_killers', 
                                'daily_anti_coagulation')
  
# self-reported symptoms -------
  
  globals$symptoms <- c('fever', 
                        'ague', 
                        'sore_throat', 
                        'running_nose', 
                        'fatigue', 
                        'dry_cough', 
                        'wet_cough', 
                        'breath_short', 
                        'dyspnoe', 
                        'chest_pain', 
                        'tachycardia', 
                        'extrasystole', 
                        'joint_pain', 
                        'bone_pain', 
                        'muscle_pain', 
                        'abdominal_pain', 
                        'nausea', 
                        'vomiting', 
                        'dim_appetite', 
                        'diarrhea', 
                        'dizziness', 
                        'headache', 
                        'anosmia', 
                        'taste_loss', 
                        'confusion', 
                        'tingle_feet', 
                        'tingle_hands', 
                        'ache_feet', 
                        'ache_hands', 
                        'numb_feet', 
                        'numb_hands', 
                        'unhandiness_walk', 
                        'unhandiness_micromotor', 
                        'sleep_prob', 
                        'fatigue_day', 
                        'imp_concentration', 
                        'forgetfulness', 
                        'epilepsy_covid', 
                        'swelling', 
                        'blue_fingers', 
                        'urticaria', 
                        'blister_rash', 
                        'net_rash', 
                        'red_eyes')

# colors, tiles, descriptions and labels, read from the variable lexicon ------  
  
 globals$var_lexicon <- read_excel('./input data/variable_lexicon.xlsx')
  
# cohort colors and labels -----
  
  globals$cohort_labs <- c(north = 'TY', 
                           south = 'STY')
  
  globals$cohort_colors <- globals$var_lexicon %>% 
    filter(variable == 'cohort') %>% 
    .$level_colors %>% 
    stri_split_fixed(pattern = ', ') %>% 
    unlist %>% 
    set_names(names(globals$cohort_labs))
  
# time point colors and labels -----
  
  globals$time_labs <- c(acute = 'acute COVID-19', 
                         long = 'long COVID', 
                         chronic = 'PASC')
  
  globals$time_colors <- c(acute = 'firebrick4',
                           long = 'coral3', 
                           chronic = 'bisque')
  

# END ----
