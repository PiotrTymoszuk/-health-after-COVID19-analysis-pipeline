# This script stitches together the paper tables

  insert_head()

# tools -----
  
  library(writexl)
  
# data containers ------
  
  paper_tables <- list()
  suppl_tables <- list()
  
# Tables 1 - 3: cohort characteristic at baseline, COVID-19 course and follow-up/convalescence  ------
  
  insert_msg('Tables 1 - 3: cohort characteristic')
  
  paper_tables[c('cohort_baseline', 
                 'covid_course', 
                 'follow_up')] <- cohort_character$summary_tbl %>% 
    map(mutate, 
        significance = ifelse(class == 'numeric', 
                              ifelse(p_U_FDR >= 0.05, 
                                     'ns', 
                                     paste('p =', 
                                           signif(p_U_FDR, 2))), 
                              ifelse(p_chi_FDR >= 0.05, 
                                     'ns', 
                                     paste('p =', 
                                           signif(p_chi_FDR, 2)))), 
        test = ifelse(class == 'numeric', 
                      'U', 
                      'Chi squared')) %>% 
    map(select, 
        label, 
        north, 
        south, 
        significance, 
        test) %>% 
    map(set_names, 
        c('Variable', 
          'North Tyrol', 
          'South Tyrol', 
          'Significance', 
          'Test'))
  
  paper_tables$follow_up$Significance[paper_tables$follow_up$Variable == 'Stroke after COVID-19'] <- 'ns'
  
# Table S1: Study variables ------
  
  insert_msg('Table S1: study variables')
  
  suppl_tables$study_variables <- globals$var_lexicon %>% 
    select(label, 
           label_short, 
           unit, 
           description, 
           cutpoints, 
           level_labs, 
           module) %>% 
    set_names(c('Variable name', 
                'Variable short name', 
                'Unit', 
                'Description',
                'Cutpoints', 
                'Levels', 
                'Variable type'))
  
# Table S2: Stratification of the symptom sum variables -----
  
  insert_msg('Table S2: Symptom sum stratification scheme')
  
  suppl_tables$symptom_sum_strata <- map2(cov_data[c('north', 'south')], 
                                          symptom_clust$symptom_sums[c('north', 'south')], 
                                          left_join, by = 'ID') %>% 
    map(function(cohort) c('sum_symptoms_acute', 'acute.NIP', 'acute.MOP') %>% 
          map_dfr(variable_stats,
                  inp_tbl = cohort)) %>% 
    map(column_to_rownames, 'variable') %>% 
    map(select, 
        min, perc25, median, perc75, max) %>% 
    map(t) %>% 
    map(as_tibble)
  
  suppl_tables$symptom_sum_strata <- suppl_tables$symptom_sum_strata %>% 
    map(function(cohort) cohort %>% 
          map_dfc(~c(paste0('(', .x[1], ', ', .x[2], ']'), 
                     paste0('(', .x[2], ', ', .x[3], ']'), 
                     paste0('(', .x[3], ', ', .x[4], ']'), 
                     paste0('(', .x[4], ', ', .x[5], ']'))) %>% 
          mutate(quartile = c('Q1', 'Q2', 'Q3', 'Q4'))) %>% 
    map2_dfr(., c('North Tyrol', 'South Tyrol'), ~mutate(.x, cohort = .y)) %>% 
    select(cohort, quartile, sum_symptoms_acute, acute.NIP, acute.MOP) %>% 
    set_names(c('Cohort', 
                'Quartile', 
                '# acute symptoms', 
                '# acute NIP symptoms', 
                '# acute MOP symptoms'))

# Table S3: frequencies of the symptoms in the entire cohort and significance in the chi test for trend ----
  
  insert_msg('Table S3: Symptom frequencies in the entire cohorts and Chi testing results')
  
  suppl_tables$sympt_frequency <- cov_symptfreq$plotting_tbls$cohort %>% 
    select(cohort, 
           split_var, 
           symptom_label, 
           percent, 
           n, 
           total_n, 
           statistic, 
           parameter, 
           p_value, 
           p_fdr) %>% 
    mutate(split_var = car::recode(split_var, 
                                   "'acute' = '0 - 2 weeks'; 
                                   'subacute' = '2 - 4 weeks'; 
                                   'long' = 'over 4 weeks'"), 
           significance = ifelse(p_fdr < 0.5, 
                                 paste('p =', signif(p_fdr, 2)), 
                                 'ns'), 
           statistic = signif(statistic, 3), 
           cohort = car::recode(cohort, 
                                "'north' = 'North Tyrol';
                                'south' = 'South Tyrol'")) %>%
    select(cohort, 
           split_var, 
           symptom_label, 
           percent, 
           n, 
           total_n, 
           significance) %>% 
    set_names(c('Cohort', 
                'Time point', 
                'Symptom', 
                'Percent of cohort', 
                'N', 
                'N complete answers', 
                'Significance'))
  
# Table S4: assignment of the symptoms to the clusters ------
  
  insert_msg('Table S4: Assignment of the symptoms to the phenotypes')
    
  suppl_tables$phenotypes <- symptom_clust$analyses_north %>% 
    map(extract, 'assignment') %>% 
    map(mutate, 
        Phenotype = clust_id, 
        observation = translate_var(observation, out_value = 'label'), 
        observation = tolower(observation)) %>% 
    map(group_by, Phenotype) %>% 
    map(summarise, Symptoms = paste(observation, collapse = ', ')) %>% 
    map2_dfr(., c('acute COVID-19', 
                  'long COVID', 
                  'PASC'), 
             ~mutate(.x, `Time point` = .y)) %>% 
    mutate(Phenotype = car::recode(Phenotype, 
                                   "'MOP' = 'Multi-Organ Phenotype (MOP)'; 
                                   'NIP' = 'Non-specific Infection Phenotype (NIP)'; 
                                   'FAP' = 'Fatigue Phenotype (FAP)'; 
                                   'HAP' = 'Hyposmia/Anosmia Phenotype (HAP)'")) %>% 
    select(`Time point`, 
           Phenotype,
           Symptoms)

# Table S5: demographic features of the participant clusters in long COVID and PASC ----
  
  insert_msg('Table S5: demographic and clinical features of the participant clusters')
  
  suppl_tables$part_subsets_features <- part_clinics$analyses_factor %>% 
    map(get_feature_summary) %>% 
    map2_dfr(., names(.), ~mutate(.x, 
                                  cohort = .y, 
                                  p_adj = p.adjust(p_chi, 'BH'))) %>% 
    mutate(timepoint = ifelse(stri_detect(cohort, fixed = 'pasc'), 'PASC', 'long COVID'), 
           cohort = ifelse(stri_detect(cohort, fixed = 'north'), 'Tyrol', 'South Tyrol'), 
           variable = translate_var(variable, out_value = 'label'), 
           p_raw = ifelse(p_chi < 0.05, 
                          paste('p =', signif(p_chi, 2)), 
                          paste0('ns (p = ', signif(p_chi, 2), ')')), 
           p_fdr = ifelse(p_adj < 0.05, 
                          paste('p =', signif(p_adj, 2)), 
                          paste0('ns (p = ', signif(p_adj, 2), ')'))) %>% 
    select(timepoint, cohort, variable, starts_with('HAP'), p_raw, p_fdr)
  
  suppl_tables$part_subsets_features <- suppl_tables$part_subsets_features %>% 
    set_names(c('Time point', 
                'Cohort', 
                'Feature', 
                'HAP neg', 
                'HAP int', 
                'HAP high', 
                'Raw p', 
                'Adjusted p')) %>% 
    mutate(`HAP neg` = stri_replace_all(`HAP neg`, regex = '^no:.*\\nyes:\\s{1}', replacement = ''), 
           `HAP int` = stri_replace_all(`HAP int`, regex = '^no:.*\\nyes:\\s{1}', replacement = ''), 
           `HAP high` = stri_replace_all(`HAP high`, regex = '^no:.*\\nyes:\\s{1}', replacement = ''))
  
# Table S6: recovery scoring in the participant clusters in long COVID and PASC -----  
  
  insert_msg('Table S6: particant cluster characteristic')
  
  suppl_tables$part_subsets_scores <- part_clinics$analyses_numeric %>% 
    map(get_feature_summary) %>% 
    map2_dfr(., names(.), ~mutate(.x, 
                                  cohort = .y, 
                                  p_adj = p.adjust(p_non_param, 'BH'))) %>% 
    mutate(timepoint = ifelse(stri_detect(cohort, fixed = 'pasc'), 'PASC', 'long COVID'), 
           cohort = ifelse(stri_detect(cohort, fixed = 'north'), 'Tyrol', 'South Tyrol'), 
           variable = translate_var(variable, out_value = 'label'), 
           p_raw = ifelse(p_non_param < 0.05, 
                          paste('p =', signif(p_non_param, 2)), 
                          paste0('ns (p = ', signif(p_non_param, 2), ')')), 
           p_fdr = ifelse(p_adj < 0.05, 
                          paste('p =', signif(p_adj, 2)), 
                          paste0('ns (p = ', signif(p_adj, 2), ')'))) %>% 
    select(timepoint, cohort, variable, starts_with('HAP'), p_raw, p_fdr) %>% 
    set_names(c('Time point', 
                'Cohort', 
                'Score', 
                'HAP neg', 
                'HAP int', 
                'HAP high', 
                'Raw p', 
                'Adjusted p'))
  
# Table S7: candidate co-variates in modeling tasks -----
  
  insert_msg('Table S7: candidate co-variates in modeling tasks')
  
  suppl_tables$candidate_factors <- risk_modeling$resp_lexicon_north %>% 
    mutate(indep_variable = map(indep_variable, translate_var, out_value = 'label_short')) %>% 
    mutate(indep_variable = map_chr(indep_variable, paste, collapse = ', '), 
           response = translate_var(response, out_value = 'label_short'), 
           method = car::recode(family, "'quasipoisson' = 'GLM Poisson'; 'quasibinomial' = 'logistic regression'")) %>% 
    select(response,  method, indep_variable) %>% 
    set_names(c('Response', 'Method', 'Co-variates'))

# Table S8: weights based on age and sex in the general Tyrolean and Italian COVID-19 convalescents -----
  
  insert_msg('Table S8: Weights used in modeling')
  
  suppl_tables$modeling_weights <- model_wrangling$weights %>% 
    map2_dfr(., 
             c('North Tyrol', 'South Tyrol'), 
             function(x, y) mutate(x, cohort = y)) %>% 
    select(cohort, 
           age_strata, 
           sex, 
           number_convalescents, 
           freq_weight) %>% 
    set_names(c('Cohort', 
                'Age strata', 
                'Sex', 
                'Convalescents', 
                'Freq. weight'))

# Table S9: univariate modeling results ------
  
  insert_msg('Table S9: Results of univariate modeling: common significant factors')
  
  suppl_tables$uni_modeling_results <- risk_modeling$signif_fct_tbls %>% 
    reduce(rbind) %>% 
    format_res_tbl %>% 
    filter(stri_detect(`Co-variate`, fixed = 'baaeline'))

# saving the tables on the disc -----
  
  insert_msg('Saving the tables in the disc')
  
  paper_tables[c('cohort_baseline', 
                 'covid_course', 
                 'follow_up')] %>% 
    set_names(c('table1', 
                'table2',
                'table3')) %>% 
    write_xlsx(path = './paper/tables/tables.xlsx')

  suppl_tables[c('study_variables', 
                 'symptom_sum_strata', 
                 'sympt_frequency', 
                 'phenotypes', 
                 'part_subsets_features', 
                 'part_subsets_scores', 
                 'candidate_factors', 
                 'modeling_weights', 
                 'uni_modeling_results')] %>% 
    set_names(c('table S1 study variables', 
                'table S2 symptom strata', 
                'table S3 symptom frequency', 
                'table S4 symptom phenotypes', 
                'table S5 part subsets clinics', 
                'table S6 part subsets scores', 
                'table S7 candidate factors', 
                'table S8 modeling weights', 
                'table S9 univariate modeling')) %>% 
    write_xlsx(path = './paper/supplementary tables/supplementary_tables.xlsx')

# END -----
  
  insert_tail()