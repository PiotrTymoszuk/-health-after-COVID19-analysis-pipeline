# This script analyzes single MOP, HAP, FAP features in the participant clusters
# The counts of symptoms are compared with Chi-Sq test with FDR correction

  insert_head()
  
# data container ----
  
  part_sympt <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  ## symptom variables of interest (top 10 in long COVID and PASC)
  
  symptom_clust$top_vars <- cov_symptfreq$top_10_table %>% 
    filter(split_var %in% c('long', 'chronic')) %>% 
    .$variable %>% unique()
  
  symptom_clust$variables <- c('long', 'chronic') %>% 
    map(~paste(symptom_clust$top_vars, .x, sep = '_')) %>% 
    set_names(c('long', 'chronic'))
  
# Analysis tables with long and chronic symptom indexes and cluster assignments -----
  
  insert_msg('Analysis table')
  
  part_sympt$analysis_tbl <- part_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, extract, 'assignment')) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    set_names(names(cov_data)) %>% 
    map(mutate, 
        ID = observation, 
        clust_id = factor(clust_id, unname(part_clust$clust_labels))) %>% 
    map(filter, clust_id != 'noise')
  
  part_sympt$analysis_tbl <- map2(cov_data, 
                                  list(symptom_clust$variables$long, 
                                       symptom_clust$variables$long, 
                                       symptom_clust$variables$chronic, 
                                       symptom_clust$variables$chronic), 
                                  ~select(.data = .x, ID, all_of(.y))) %>% 
    map(set_names, c('ID', symptom_clust$top_vars)) %>% 
    map2(part_sympt$analysis_tbl, ., left_join, by = 'ID')
  
# Serial analysis ----
  
  insert_msg('Serial analysis of the symptom frequency in the participant clusters')
  
  part_sympt$analyses <- part_sympt$analysis_tbl %>% 
    map(function(dataset) symptom_clust$top_vars %>% 
          map(analyze_feature, 
              inp_tbl = dataset, 
              split_var = 'clust_id') %>% 
          set_names(symptom_clust$top_vars))
  
# Extracting model summaries and p value correction with FDR, indetification of the significantly regulated symptoms -----
  
  insert_msg('Testing summary and symptom frequencies')
  
  ## frequencies
  
  part_sympt$frequencies <- part_sympt$analyses %>% 
    map(~map_dfr(.x, extract_counts) %>% 
          filter(strata == 'yes')) %>% 
    map2(., names(.), ~mutate(.x, dataset = factor(.y, names(part_sympt$analyses))))
  
  ## testing summary
  
  part_sympt$testing_summary <- part_sympt$analyses %>% 
    map(~map_dfr(.x, extract_test_summary) %>% 
          mutate(p_adj = p.adjust(p_value, 'BH'), 
                 significant = ifelse(p_adj < 0.05, 'yes', 'no'))) %>% 
    map2(., names(.), ~mutate(.x, dataset = factor(.y, names(part_sympt$analyses))))
  
  ## significant and near-significant symptoms to be presented in a plot
  
  part_sympt$signif_sympt <- list(long = part_sympt$testing_summary[c('north', 'south')], 
                                  chronic = part_sympt$testing_summary[c('north_pasc', 'south_pasc')]) %>% 
    map(~map(.x, filter, significant == 'yes'))
  
  ## plotting features
  
  part_sympt$plot_features <-  part_sympt$signif_sympt %>% 
    map(~map(.x, ~.x$variable)) %>% 
    unlist %>% 
    unique

# presenting the symptoms in a heat map -----

  insert_msg('Symptom presence in a heat map')  
  
  part_sympt$heat_maps <- list(inp_tbl = part_sympt$analysis_tbl, 
                               plot_title = c('TY: long COVID', 
                                              'STY: long COVID', 
                                              'TY: PASC', 
                                              'STY: PASC'), 
                               plot_tag = part_clust$n_tags %>% 
                                 transpose %>% 
                                 unlist(recursive = FALSE)) %>% 
    pmap(plot_sympt_hm, 
         vars = part_sympt$plot_features) %>% 
    map(~.x + 
          scale_y_discrete(labels = translate_var(globals$symptoms, out_value = 'label_short')))
  
# plotting symptom frequencies in the clusters ----
  
  insert_msg('Plotting symptom frequencies')
  
  part_sympt$freq_plots <- list(freq_tbl = part_sympt$frequencies, 
                                testing_summary = part_sympt$testing_summary, 
                                plot_title = c('TY: long COVID', 
                                               'STY: long COVID', 
                                               'TY: PASC', 
                                               'STY: PASC'), 
                                plot_tag = part_clust$n_tags %>% 
                                  transpose %>% 
                                  unlist(recursive = FALSE)) %>% 
    pmap(plot_sympt_freq) %>% 
    map(~.x + 
          scale_y_discrete(labels = translate_var(globals$symptoms, out_value = 'label_short')) + 
          scale_fill_manual(values = part_clust$clust_colors, 
                            name = '') + 
          scale_x_continuous(limits = c(0, 135), breaks = seq(0, 100, 25)))

# END -----
  
  insert_tail()