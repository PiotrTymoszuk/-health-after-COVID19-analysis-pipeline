# This script prepares tables with characteristic of the North and South Tyrol cohorts:
# (1) baseline data, (2) acute COVID-19 course and (3) convalescence/follow-up

  insert_head()

# data container ----

  cohort_character <- list()

# globals and analysis table -----
  
  insert_msg('Analysis table')
  
  ## table with variables of interest
  
  cohort_character$var_lexicons <- c('cohort_characteristic', 
                                     'course_characteristic', 
                                     'followup_characteristic') %>% 
    map(function(x) filter(globals$var_lexicon, 
                           .data[[x]] == 'yes', 
                           variable != 'cohort')) %>% 
    map(select, 
        variable, 
        label) %>% 
    set_names(c('cohort_characteristic', 
                'course_characteristic', 
                'followup_characteristic'))

  ## analysis table
  
  cohort_character$analysis_tbl <- cov_data[c('north', 'south')] %>% 
    reduce(rbind)
  
# serial analysis -----
  
  insert_msg('Serial analysis of the cohort features')

  cohort_character$analyses <- cohort_character$var_lexicons %>% 
    map(function(x) x$variable) %>% 
    map(function(x) map(x, 
                        analyze_feature, 
                        inp_tbl = cohort_character$analysis_tbl, 
                        split_var = 'cohort')) %>%
    map2(., 
         cohort_character$var_lexicons %>% 
           map(function(x) x$variable), 
         set_names)

# creating a paper table, correcting the p values for multiple comparisons with BH method -----
  
  insert_msg('Creating a table with cohort characteristics')
  
  cohort_character$summary_tbls <- list(analysis_obj = cohort_character$analyses, 
                                        label = cohort_character$var_lexicons %>% 
                                          map(function(x) x$label)) %>% 
    pmap(get_feature_summary) %>% 
    map(set_names, 
        c('variable', 
          'label', 
          'class', 
          'north', 
          'south', 
          'p_T', 
          'p_U', 
          'p_chi')) %>% 
    map(mutate, 
        p_T_FDR = p.adjust(p_T, 'BH'), 
        p_U_FDR = p.adjust(p_U, 'BH'), 
        p_chi_FDR = p.adjust(p_chi, 'BH')) %>% 
    map(mutate, 
        significance = ifelse(p_U_FDR < 0.05 | p_chi_FDR < 0.05, 
                              'yes',
                              'no'))
  
# a convenience vector with significant variables ------
  
  insert_msg('Identifying the significant variables')
  
  cohort_character$signif_variables <- cohort_character$summary_tbl %>% 
    map(filter, 
        significance == 'yes') %>% 
    map(function(x) x$variable)
  
# END -----
  
  insert_tail()