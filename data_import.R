# This script reads and clears the complete survey data
# For the North and South Tyrol cohorts

# tools ------

  library(plyr)
  library(tidyverse)
  library(foreign)

  c('./tools/sys_tools.R', 
    './tools/cov_project_tools.R', 
    './tools/cov_project_globals.R', 
    './tools/survey_import_tools.R') %>% 
    walk(source, 
         encoding = 'UTF-8')
  
  insert_head()

# data containers -----
  
  raw_data <- list()
  cov_data <- list()
  
  consort_data <- list()

# reading the spss data -----
  
  insert_msg('Reading the SPSS data tables')
  
  raw_data[c('north', 
             'south')] <- c('./input data/nordtirol_05072021.sav', 
                            './input data/suedtirol_05072021.sav') %>% 
    map(read.spss) %>% 
    map(as_tibble)
  
# Clearing the demographics data -----
  
  insert_msg('Clearing the demographic data')
  
  cov_data$demo_tbl <- list(raw_survey = raw_data, 
                            form_fill_var = c('creation_timestamp_gesundheit_nach_covid_19_1_0', 
                                              'creation_timestamp_gesundheit_nach_covid_19__suedtirol__@'), 
                            responder_var = c('ausf_llende_person', 
                                              'gesundheit_nach_covid_19__suedtirol__1_0_item_001'), 
                            region_var = c('bezirk', 
                                           'gesundheit_nach_covid_19__suedtirol__1_0_item_005'), 
                            lang_var = c('German', 
                                         'gesundheit_nach_covid_19__suedtirol__1_0_item_004'), 
                            age_var = c('gesundheit_nach_covid_19_1_0_item_002', 
                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_002'), 
                            cohabitant_var = c('gesundheit_nach_covid_19_1_0_item_005', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_006'), 
                            pos_cohabitant_var = c('gesundheit_nach_covid_19_1_0_item_006', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_007'), 
                            height_var = c('gesundheit_nach_covid_19_1_0_item_007', 
                                           'gesundheit_nach_covid_19__suedtirol__1_0_item_008'), 
                            w_before_var = c('gesundheit_nach_covid_19_1_0_item_008', 
                                             'gesundheit_nach_covid_19__suedtirol__1_0_item_009'), 
                            w_recent_var = c('gesundheit_nach_covid_19_1_0_item_009', 
                                             'gesundheit_nach_covid_19__suedtirol__1_0_item_010'),
                            emplovment_var = c('gesundheit_nach_covid_19_1_0_item_011', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_012'), 
                            employment_sec_var = c('gesundheit_nach_covid_19_1_0_item_012', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_013')) %>% 
    pmap(extract_demo)
  
# Clearing the co-morbidity data -----
 
  insert_msg('Clearing the co-morbidity data')
  
  cov_data$comorb_tbl <- list(raw_survey = raw_data, 
                              comorb_var_prefix = c('gesundheit_nach_covid_19_1_0_item_013', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_014')) %>% 
    pmap(extract_comorb)
  
# Clearing the daily medication, smoking and vaccination data -----
  
  insert_msg('Clearing the medication, smoking and vaccination data')
  
  cov_data$medication_tbl <- list(raw_survey = raw_data, 
                                  drug_type_vars = list(c('gesundheit_nach_covid_19_1_0_item_015', 
                                                          'gesundheit_nach_covid_19_1_0_item_016', 
                                                          'gesundheit_nach_covid_19_1_0_item_017', 
                                                          'gesundheit_nach_covid_19_1_0_item_018',
                                                          'gesundheit_nach_covid_19_1_0_item_019'), 
                                                        c('gesundheit_nach_covid_19__suedtirol__1_0_item_016', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_017', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_018', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_019',
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_020')), 
                                  daily_medication_var = c('gesundheit_nach_covid_19_1_0_item_014', 
                                                           'gesundheit_nach_covid_19__suedtirol__1_0_item_015'), 
                                  anti_coagulation_var = c('gesundheit_nach_covid_19_1_0_item_020', 
                                                           'gesundheit_nach_covid_19__suedtirol__1_0_item_021'), 
                                  flu_vacc_var = c('gesundheit_nach_covid_19_1_0_item_021', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_022'), 
                                  pneumo_vacc_var = c('gesundheit_nach_covid_19_1_0_item_022', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_023'), 
                                  smoking_var = c('gesundheit_nach_covid_19_1_0_item_023', 
                                                  'gesundheit_nach_covid_19__suedtirol__1_0_item_024')) %>% 
    pmap(extract_medication)

# Clearing the CoV diagnosis data ------
  
  cov_data$diagn_tbl <- list(raw_survey = raw_data, 
                             test_date_var = c('gesundheit_nach_covid_19_1_0_item_024_0_datum_des_ersten', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_025_0_datu'), 
                             symptom_onset_date_var = c('gesundheit_nach_covid_19_1_0_item_024_3_datum_der_ersten', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_025_0_datu'), 
                             contact_var = c('gesundheit_nach_covid_19_1_0_item_025', 
                                             'gesundheit_nach_covid_19__suedtirol__1_0_item_026'), 
                             contact_sympt_var = c('gesundheit_nach_covid_19_1_0_item_026', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_027'), 
                             quarantine_var = c('gesundheit_nach_covid_19_1_0_item_027', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_028'), 
                             test_auth_var = c('gesundheit_nach_covid_19_1_0_item_028', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_029'), 
                             time_result_var= c('gesundheit_nach_covid_19_1_0_item_029', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_030'), 
                             result_report_var = c('gesundheit_nach_covid_19_1_0_item_030_4_wer_@', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_031_3_wer_@'), 
                             information_quality_var = c('gesundheit_nach_covid_19_1_0_item_031', 
                                                         'gesundheit_nach_covid_19__suedtirol__1_0_item_032'), 
                             auth_support_var = c('gesundheit_nach_covid_19_1_0_item_032', 
                                                  'gesundheit_nach_covid_19__suedtirol__1_0_item_033'), 
                             support_who_var = c('gesundheit_nach_covid_19_1_0_item_033', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_034'), 
                             suport_quality_var = c('gesundheit_nach_covid_19_1_0_item_034', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_035'), 
                             test_pos_week_var = c('gesundheit_nach_covid_19_1_0_item_035_0_anzahl_der_woche', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_036_0_anza'), 
                             ab_test_var = c('gesundheit_nach_covid_19_1_0_item_036', 
                                             'gesundheit_nach_covid_19__suedtirol__1_0_item_037'), 
                             diagn_pcr_var = c('gesundheit_nach_covid_19_1_0_item_024_1_abstrich', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_025_1_abst'), 
                             diagn_antibodies_var = c('gesundheit_nach_covid_19_1_0_item_024_2_antik_rpertest', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_025_2_anti'), 
                             no_symptom_var = c('gesundheit_nach_covid_19_1_0_item_024_4_keine_symptome', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_025_4_kein'), 
                             reporting_body_vars = list(c('gesundheit_nach_covid_19_1_0_item_030_0_beh_rde_gesundhe', 
                                                          'gesundheit_nach_covid_19_1_0_item_030_1_krankenhaus', 
                                                          'gesundheit_nach_covid_19_1_0_item_030_3_andere'), 
                                                        c('gesundheit_nach_covid_19__suedtirol__1_0_item_031_0_hygi', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_031_1_kran', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_031_2_ande')), 
                             repeated_test_var = c('gesundheit_nach_covid_19_1_0_item_035_1_wurde_nicht_gete', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_036_1_wurd')) %>% 
    pmap(extract_cov_diagnosis)


  
    
# Clearing the disease course data ------
  
  insert_msg('Clearing the disease course data')
  
  cov_data$course_tbl <- list(raw_survey = raw_data, 
                              sympt_free_var = c('gesundheit_nach_covid_19_1_0_item_037', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_038'), 
                              hair_loss_var = c('gesundheit_nach_covid_19_1_0_item_082', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_083'), 
                              other_sympt_var = c('gesundheit_nach_covid_19_1_0_item_083', 
                                                  'gesundheit_nach_covid_19__suedtirol__1_0_item_084'), 
                              w_loss_index = c('gesundheit_nach_covid_19_1_0_item_084', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_085'), 
                              w_loss_kg_var = c('gesundheit_nach_covid_19_1_0_item_085', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_086'), 
                              surgery_infection_var = c('gesundheit_nach_covid_19_1_0_item_086', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_087'), 
                              surgery_infection_type_var = c('gesundheit_nach_covid_19_1_0_item_087', 
                                                             'gesundheit_nach_covid_19__suedtirol__1_0_item_088'), 
                              illness_feeling_var = c('gesundheit_nach_covid_19_1_0_item_088', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_089'), 
                              relapse_var = c('gesundheit_nach_covid_19_1_0_item_089', 
                                              'gesundheit_nach_covid_19__suedtirol__1_0_item_090'), 
                              subj_inf_var = c('gesundheit_nach_covid_19_1_0_item_090', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_091'), 
                              gp_contact_var = c('gesundheit_nach_covid_19_1_0_item_091', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_092'), 
                              hosp_var = c('gesundheit_nach_covid_19_1_0_item_094', 
                                           'gesundheit_nach_covid_19__suedtirol__1_0_item_095'), 
                              hosp_days_var = c('gesundheit_nach_covid_19_1_0_item_095', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_096'), 
                              icu_var = c('gesundheit_nach_covid_19_1_0_item_096', 
                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_097'), 
                              pregn_problems_var = c('gesundheit_nach_covid_19_1_0_item_098', 
                                                     'gesundheit_nach_covid_19__suedtirol__1_0_item_099'), 
                              pregn_problems_desc = c('gesundheit_nach_covid_19_1_0_item_099', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_100'), 
                              birth_problems_var = c('gesundheit_nach_covid_19_1_0_item_100', 
                                                     'gesundheit_nach_covid_19__suedtirol__1_0_item_101'), 
                              birth_problems_desc = c('gesundheit_nach_covid_19_1_0_item_101', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_102'), 
                              baby_sep_var = c('gesundheit_nach_covid_19_1_0_item_102', 
                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_103'), 
                              feeding_problems_var = c('gesundheit_nach_covid_19_1_0_item_103', 
                                                       'gesundheit_nach_covid_19__suedtirol__1_0_item_104'), 
                              feeding_problems_desc = c('gesundheit_nach_covid_19_1_0_item_104', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_105'), 
                              bonding_problems_var = c('gesundheit_nach_covid_19_1_0_item_105', 
                                                       'gesundheit_nach_covid_19__suedtirol__1_0_item_106'), 
                              bonding_problems_desc = c('gesundheit_nach_covid_19_1_0_item_106', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_107'), 
                              partner_birth_var = c('gesundheit_nach_covid_19_1_0_item_107', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_108'), 
                              partner_birth_vis_var = c('gesundheit_nach_covid_19_1_0_item_108', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_109'), 
                              contact_gp_var = c('gesundheit_nach_covid_19_1_0_item_092_0_hausarzt', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_093_0_haus'), 
                              contact_internist_var = c('gesundheit_nach_covid_19_1_0_item_092_1_internist', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_093_1_inte'), 
                              contact_pulmo_var = c('gesundheit_nach_covid_19_1_0_item_092_2_lungenfacharzt', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_093_2_lung'), 
                              contact_neurologist_var = c('gesundheit_nach_covid_19_1_0_item_092_3_neurologe', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_093_3_neur'), 
                              contact_other_phys_var = c('gesundheit_nach_covid_19_1_0_item_092_4_anderer_facharzt', 
                                                         'gesundheit_nach_covid_19__suedtirol__1_0_item_093_4_ande'), 
                              contacted_hotline_var = c('gesundheit_nach_covid_19_1_0_item_092_6_gesundheitshotli', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_093_5_b_rg'), 
                              contacted_emergency_var = c('gesundheit_nach_covid_19_1_0_item_092_7__rztliche_versor', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_093_6_notr'), 
                              cov_therapy_none_var = c('gesundheit_nach_covid_19_1_0_item_093_0_keine_therapie_e', 
                                                       'gesundheit_nach_covid_19__suedtirol__1_0_item_094_0_kein'), 
                              cov_therapy_antipyretic = c('gesundheit_nach_covid_19_1_0_item_093_1_fiebersenker', 
                                                          'gesundheit_nach_covid_19__suedtirol__1_0_item_094_1_fieb'), 
                              cov_therapy_antibiotic = c('gesundheit_nach_covid_19_1_0_item_093_2_antibiotika', 
                                                         'gesundheit_nach_covid_19__suedtirol__1_0_item_094_2_anti'), 
                              cov_pregnancy = c('gesundheit_nach_covid_19_1_0_item_097_0_schwangerschaft', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_098_0_schw'), 
                              cov_birth = c('gesundheit_nach_covid_19_1_0_item_097_1_geburt', 
                                            'gesundheit_nach_covid_19__suedtirol__1_0_item_098_1_gebu'),
                              cov_breast_feeding = c('gesundheit_nach_covid_19_1_0_item_097_2_stillend', 
                                                     'gesundheit_nach_covid_19__suedtirol__1_0_item_098_2_stil'), 
                              cov_no_circumst = c('gesundheit_nach_covid_19_1_0_item_097_3_keine', 
                                                  'gesundheit_nach_covid_19__suedtirol__1_0_item_098_3_kein')) %>% 
    pmap(extract_course)
  
# Clearing the symptom data -----
  
  insert_msg('Clearing the symptom data')
  
  cov_data$symptom_tbl <- list(raw_survey = raw_data, 
                               symptom_vars = list(paste('gesundheit_nach_covid_19_1_0_item_0', 
                                                         38:81, 
                                                         sep = ''), 
                                                   paste('gesundheit_nach_covid_19__suedtirol__1_0_item_0', 
                                                         39:82, 
                                                         sep = ''))) %>% 
    pmap(extract_symptoms)
  
# Clearing the mental health data ------
  
  insert_msg('Clearing the mental health data')
  
  cov_data$mental_tbl <- list(raw_survey = raw_data, 
                              mental_health_vars = list(paste('gesundheit_nach_covid_19_1_0_item', 
                                                              109:114, 
                                                              sep = '_'), 
                                                        paste('gesundheit_nach_covid_19__suedtirol__1_0_item', 
                                                              110:115, 
                                                              sep = '_')), 
                              mental_health_var = c('gesundheit_nach_covid_19_1_0_item_115', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_116'), 
                              life_quality_var = c('gesundheit_nach_covid_19_1_0_item_116', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_117')) %>% 
    pmap(extract_mental)
  
# Clearing the psychosocial data -------
  
  insert_msg('Clearing the psychsocial data')
  
  cov_data$psychococ_tbl <- list(raw_survey = raw_data, 
                                 psychsoc_vars = list(paste('gesundheit_nach_covid_19_1_0_item', 
                                                            117:125, sep = '_'), 
                                                      paste('gesundheit_nach_covid_19__suedtirol__1_0_item', 
                                                            118:126, sep = '_')), 
                                 wokplace_change_var = c('gesundheit_nach_covid_19_1_0_item_126', 
                                                         'gesundheit_nach_covid_19__suedtirol__1_0_item_127'), 
                                 workplace_changes_descr = c('gesundheit_nach_covid_19_1_0_item_128', 
                                                             'gesundheit_nach_covid_19__suedtirol__1_0_item_129'), 
                                 workplace_changes_quality = c('gesundheit_nach_covid_19_1_0_item_127', 
                                                               'gesundheit_nach_covid_19__suedtirol__1_0_item_128')) %>% 
    pmap(extract_psychsoc)
  
# Clearing the follow-up data ------
  
  insert_msg('Clearing the follow-up data')
  
  cov_data$follow_up <- list(raw_survey = raw_data, 
                             fup_heart_infr_var = c('gesundheit_nach_covid_19_1_0_item_129_0_herzinfarkt', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_130_0_herz'), 
                             fup_stroke_var = c('gesundheit_nach_covid_19_1_0_item_129_1_schlaganfall', 
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_130_1_schl'), 
                             fup_embo_var = c('gesundheit_nach_covid_19_1_0_item_129_2_embolie_thrombos', 
                                              'gesundheit_nach_covid_19__suedtirol__1_0_item_130_2_embo'), 
                             fup_infect_var = c('gesundheit_nach_covid_19_1_0_item_129_3_andere_infekte',
                                                'gesundheit_nach_covid_19__suedtirol__1_0_item_130_3_ande'), 
                             fup_infect_desc = c('gesundheit_nach_covid_19_1_0_item_129_4_welche_@', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_130_4_welc'), 
                             fup_infect_number = c('gesundheit_nach_covid_19_1_0_item_129_5_wie_viele_@', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_130_5_wie_@'), 
                             fup_no_conditions_var = c('gesundheit_nach_covid_19_1_0_item_129_6_keine', 
                                                       'gesundheit_nach_covid_19__suedtirol__1_0_item_130_6_kein'), 
                             acute_surgery_fup_var = c('gesundheit_nach_covid_19_1_0_item_130', 
                                                       'gesundheit_nach_covid_19__suedtirol__1_0_item_131'), 
                             acute_surgery_desc = c('gesundheit_nach_covid_19_1_0_item_131', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_132'), 
                             new_med_fup_var = c('gesundheit_nach_covid_19_1_0_item_136', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_137'), 
                             new_med_fup_descr = c('gesundheit_nach_covid_19_1_0_item_137', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_138'), 
                             postp_treatment_var = c('gesundheit_nach_covid_19_1_0_item_138', 
                                                     'gesundheit_nach_covid_19__suedtirol__1_0_item_139'),
                             postp_treatment_desc = c('gesundheit_nach_covid_19_1_0_item_139', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_140'), 
                             treatment_madeup_var = c('gesundheit_nach_covid_19_1_0_item_140', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_141'), 
                             treatment_madeup_delay = c('gesundheit_nach_covid_19_1_0_item_141', 
                                                        'gesundheit_nach_covid_19__suedtirol__1_0_item_142'), 
                             new_symptoms_covindep_var = c('gesundheit_nach_covid_19_1_0_item_142', 
                                                           'gesundheit_nach_covid_19__suedtirol__1_0_item_143'), 
                             new_symptoms_covindep_desc = c('gesundheit_nach_covid_19_1_0_item_143', 
                                                            'gesundheit_nach_covid_19__suedtirol__1_0_item_144'), 
                             rehabilitation_var = c('gesundheit_nach_covid_19_1_0_item_144', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_145'), 
                             rehabilitation_type = c('gesundheit_nach_covid_19_1_0_item_145', 
                                                     'gesundheit_nach_covid_19__suedtirol__1_0_item_146'), 
                             rehabilitation_advised_var = c('gesundheit_nach_covid_19_1_0_item_146', 
                                                            'gesundheit_nach_covid_19__suedtirol__1_0_item_147'), 
                             performance_var = c('gesundheit_nach_covid_19_1_0_item_147', 
                                                 'gesundheit_nach_covid_19__suedtirol__1_0_item_148'), 
                             perf_percent = c('gesundheit_nach_covid_19_1_0_item_148', 
                                              'gesundheit_nach_covid_19__suedtirol__1_0_item_149'), 
                             rehabilitation_need_var = c('gesundheit_nach_covid_19_1_0_item_149', 
                                                         'gesundheit_nach_covid_19__suedtirol__1_0_item_150'), 
                             convalescence_var = c('gesundheit_nach_covid_19_1_0_item_150', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_151'), 
                             convalescence_date = c('gesundheit_nach_covid_19_1_0_item_151', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_152')) %>% 
    pmap(extract_followup)
  
# clearing the vaccination readiness data -----
  
  insert_msg('Clearing the vaccination readiness data')
  
  cov_data$vacc_readiness <- list(raw_survey = raw_data, 
                                  flu_vacc_var = c('gesundheit_nach_covid_19_1_0_item_132', 
                                                   'gesundheit_nach_covid_19__suedtirol__1_0_item_133'), 
                                  flu_vacc_desc = c('gesundheit_nach_covid_19_1_0_item_133', 
                                                    'gesundheit_nach_covid_19__suedtirol__1_0_item_134'), 
                                  covid_vacc_var = c('gesundheit_nach_covid_19_1_0_item_134', 
                                                     'gesundheit_nach_covid_19__suedtirol__1_0_item_135'), 
                                  covid_vacc_desc = c('gesundheit_nach_covid_19_1_0_item_135', 
                                                      'gesundheit_nach_covid_19__suedtirol__1_0_item_136')) %>% 
    pmap(extract_vacc_readiness)
  
# Creating a single table for each cohort ------
  
  insert_msg('Creating a common table for each cohort')
  
  cov_data <- cov_data[c('demo_tbl', 
                         'comorb_tbl', 
                         'medication_tbl', 
                         'diagn_tbl', 
                         'course_tbl', 
                         'symptom_tbl', 
                         'mental_tbl', 
                         'psychococ_tbl', 
                         'follow_up', 
                         'vacc_readiness')] %>% 
    transpose %>% 
    map(reduce, 
        left_join, 
        by = 'ID')

  cov_data <- cov_data %>% 
    map2(., names(.), ~mutate(.x, cohort = factor(.y, c('north', 'south'))))
  
# Consort data and filtering by hospitalization and observation time (test to completion) -----
  
  insert_msg('CONSORT data and filtering')
  
  ## adding the observation time
  
  cov_data <- cov_data %>% 
    map(mutate, 
        obs_time = as.numeric(date - first_pos_test_date))
  
  ## participants recruited in total
  
  consort_data$total_recruited <- cov_data %>% 
    map(nrow)
  
  ## filtering out the hospitalized patients
  
  consort_data$hospitalized <- cov_data %>% 
    map(filter, 
        hospitalization == 'yes' | icu == 'yes') %>% 
    map(nrow)
  
  cov_data <- cov_data %>% 
    map(filter, 
        hospitalization != 'yes') %>% 
      map(filter, 
          is.na(icu) | icu == 'no')
  
  consort_data$outpatients <- cov_data %>% 
    map(nrow)
  
  ## filtering out by the 28 day cutoff
  
  consort_data$below_cutoff <- cov_data %>% 
    map(filter, 
        obs_time < 28) %>% 
    map(nrow)
  
  cov_data <- cov_data %>% 
    map(filter, 
        obs_time >= 28)
  
  consort_data$included_analysis <- cov_data %>% 
    map(nrow)
  
# Adding the observation time strata, adjusting long_covid, sum_symptoms_long variables -----
  
  insert_msg('Stratification of the observation time, adjustment of the variables refering to a longer obs. time')

  cov_data <- cov_data %>% 
    map(mutate, 
        obs_time_strata = cut(obs_time, 
                              c(-Inf, 60, 120, 180, Inf), 
                              c('up to 60 days', 
                                '61 - 120 days', 
                                '121 - 180 days', 
                                'more than 180 days')),  
        sum_symptoms_long = ifelse(obs_time < 28, NA, sum_symptoms_long))
  
# Setting the symptom count to 0 for people reporting asymptomatic course -----
  
  insert_msg('Symptom count set to zero for people with asymptomatic course')
  
  cov_data <- cov_data %>% 
    map(mutate, 
        sum_symptoms_acute = ifelse(symptom_free == 'yes', 0, sum_symptoms_acute), 
        sum_symptoms_immediate = ifelse(symptom_free == 'yes', 0, sum_symptoms_immediate), 
        sum_symptoms_subacute = ifelse(symptom_free == 'yes', 0, sum_symptoms_subacute), 
        sum_symptoms_long = ifelse(symptom_free == 'yes', 0, sum_symptoms_long), 
        sum_symptoms_chronic = ifelse(symptom_free == 'yes', 0, sum_symptoms_chronic)) %>% 
    map(mutate, 
        acute_covid = ifelse(sum_symptoms_acute > 0, 'yes', 'no') %>% 
          factor(c('no', 'yes')), 
        subacute_covid = ifelse(sum_symptoms_subacute > 0, 'yes', 'no') %>% 
          factor(c('no', 'yes')), 
        long_covid = ifelse(sum_symptoms_long > 0, 'yes', 'no')%>% 
          factor(c('no', 'yes')), 
        pasc = ifelse(sum_symptoms_chronic > 0, 'yes', 'no')%>% 
          factor(c('no', 'yes')))
  
# Calculating the maximal symptom duration for each participant -----
  
  insert_msg('Calculating the max. symptom duration')
  
  cov_data <- cov_data %>% 
    map(find_max_duration, 
        sympt_var_vec = globals$symptoms) %>% 
    map2(cov_data, 
         .,
         left_join, 
         by = 'ID')
  
  ## setting the symptom duration to 
  
  cov_data <- cov_data %>% 
    map(mutate, 
        max_sympt_duration = ifelse(symptom_free == 'yes', 
                                    'absent', 
                                    max_sympt_duration) %>% 
          factor(c('absent', 
                   '1 - 3 days', 
                   'up to 1 week', 
                   'up to 2 weeks', 
                   'up to 4 weeks', 
                   'up to 3 months', 
                   'up to 6 months', 
                   'over 6 months')), 
        max_sympt_duration_class = ifelse(symptom_free == 'yes', 
                                          'absent', 
                                          max_sympt_duration_class) %>% 
          factor(c('absent', 
                  '1 - 3 days', 
                  'up to 1 week', 
                  'up to 2 weeks', 
                  'up to 4 weeks', 
                  'over 4 weeks')))

# Generating tables with subsets of participants with the minimal observation time of 90 days for PASC sub-analyses ----
  
  insert_msg('Subsets for PASC analyses')

  cov_data[c('north_pasc', 'south_pasc')] <- cov_data %>% 
    map(filter, 
        obs_time >= 90)
  
  consort_data$pasc <- cov_data[c('north_pasc', 'south_pasc')] %>% 
    map(nrow)
  
# END -----
  
  insert_tail()