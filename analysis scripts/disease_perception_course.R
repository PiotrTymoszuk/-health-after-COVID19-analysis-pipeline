# This script analyzes the disease perception (character, severe illness feeling)
# max. duration, recurrence and symptom number trajectories

  insert_head()
  
# data container -----
  
  cov_course <- list()
  
# variables of interest and the analysis table -----
  
  insert_msg('Globals setup and analysis table')
  
  ## variables of interest
  
  cov_course$dis_perception$variables <- c('subj_infection', 
                                           'illness_feeling', 
                                           'max_sympt_duration_class', 
                                           'relapse')
  
  cov_course$sympt_presence$variables <- c('acute_covid', 
                                           'subacute_covid', 
                                           'long_covid')

# analysis of the disease perception features ------
  
  insert_msg('Analysis of the disease perception and max symptom duration')
  
  ## analyses
  
  cov_course$dis_perception$analyses <- cov_course$dis_perception$variables %>% 
    map(analyze_feature, 
        inp_tbl = cov_data[c('north', 'south')] %>% reduce(rbind), 
        split_var = 'cohort') %>% 
    set_names(cov_course$dis_perception$variables)
  
  ## plots
  
  cov_course$dis_perception$plots <- list(analysis_obj = cov_course$dis_perception$analyses, 
                                          label = translate_var(cov_course$dis_perception$variables, 
                                                                out_value = 'label'), 
                                          fill_colors = translate_var(cov_course$dis_perception$variables, 
                                                                      out_value = 'level_colors'), 
                                          pie = c(T, F, T, T)) %>% 
    pmap(plot_analysis, 
         y_lab = '% cohort', 
         labeller = globals$cohort_labs, 
         cust_theme = globals$common_theme)
  
# analysis of the percents of individuals with symptoms in the acute, sub-acute and long phase of COVID-19 -----
  
  insert_msg('Analysis of the symptom presence in acute, sub-acute and long COVID-19')
  
  ### analysis table
  
  cov_course$sympt_presence$analysis_tbl <-  cov_data[c('north', 'south')] %>% 
    map(gather, 
        key = 'timepoint', 
        value = 'sympt_present', 
        cov_course$sympt_presence$variables) %>% 
    map(select, 
        timepoint, 
        sympt_present, 
        cohort) %>% 
    map(mutate, sympt_present = factor(sympt_present, 
                                       c('no', 'yes')), 
        timepoint = car::recode(timepoint, 
                                "'acute_covid' = '0 - 2'; 
                                   'subacute_covid' = '2 - 4'; 
                                   'long_covid' = 'over 4'") %>% 
          factor(c('0 - 2', '2 - 4', 'over 4')))
  
  ## analysis objects for the North and South Tyrol
  
  cov_course$sympt_presence$analyses <- cov_course$sympt_presence$analysis_tbl %>% 
    map(analyze_feature, 
        variable = 'sympt_present', 
        split_var = 'timepoint')
  
  ## plot features
  
  cov_course$sympt_presence$plotting_tbl <- cov_course$sympt_presence$analyses %>% 
    map(extract_counts) %>% 
    map(filter, strata == 'yes') %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y)) %>% 
    mutate(plot_lab = paste(signif(percent, 3), '%', sep = ''))

  cov_course$sympt_presence$plot_tag <- paste('\nTY: n = ', 
                                              cov_course$sympt_presence$plotting_tbl$total_n[1], 
                                              ', STY: n = ', 
                                              cov_course$sympt_presence$plotting_tbl$total_n[4], 
                                              sep = '')
  
  cov_course$sympt_presence$plot_subtitle <- paste('TY: p = ', 
                                                   signif(cov_course$sympt_presence$analyses$north$summary$p_value[1], 2), 
                                                   ', STY: p = ', 
                                                   signif(cov_course$sympt_presence$analyses$south$summary$p_value[1], 2), 
                                                   sep = '')
  ## plot 
  
  cov_course$sympt_presence$plot <- cov_course$sympt_presence$plotting_tbl %>% 
    ggplot(aes(x = split_var,  
               y = percent, 
               fill = cohort)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    geom_text(aes(label = plot_lab, 
                  y = percent + 2), 
              size = 2.6, 
              vjust = 0, 
              position = position_dodge(0.9)) + 
    scale_fill_manual(values = globals$cohort_colors, 
                      labels = globals$cohort_labs, 
                      name = 'Cohort') + 
    globals$common_theme + 
    labs(title = 'Symptom presence', 
         subtitle = cov_course$sympt_presence$plot_subtitle, 
         tag = cov_course$sympt_presence$plot_tag, 
         y = '% cohort', 
         x = 'Time after clinical onset, weeks')
  
# END -----
  
  insert_tail()