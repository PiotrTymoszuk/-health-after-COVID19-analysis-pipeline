# This scritpt investigates differences in demographic and clinical features between the long COVID and PASC participant clusters
# The tool: Ci Sq test

  insert_head()
  
# data container -----
  
  part_clinics <- list()
  
# globals: variables ------
  
  insert_msg('Globals')
  
  part_clinics$variables_all <- globals$var_lexicon %>% 
    filter(cluster_characteristic == 'yes') %>% 
    .$variable
  
  part_clinics$variables$numeric <- c('sum_symptoms_acute',
                                      'perf_impairment', 
                                      'life_quality_score', 
                                      'mental_health_score',
                                      'stress_score')
  
  part_clinics$variables$factor <- part_clinics$variables_all[!part_clinics$variables_all %in% part_clinics$variables$numeric]
  
# Analysis tables with long and chronic symptom indexes and cluster assignments -----
  
  insert_msg('Analysis table')
  
  part_clinics$analysis_tbl <- part_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, extract, 'assignment')) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    set_names(names(cov_data)) %>% 
    map(mutate, 
        ID = observation, 
        clust_id = factor(clust_id, c('HAP-', 'HAPi', 'HAP+'))) %>% 
    map(filter, clust_id != 'noise')
  
  part_clinics$analysis_tbl <- cov_data %>% 
    map(select, 
        ID, 
        all_of(part_clinics$variables_all)) %>%
    map2(part_clinics$analysis_tbl, ., left_join, by = 'ID')
  
# Serial analysis -----
  
  insert_msg('Serial analysis')
  
  part_clinics$analyses_numeric <- part_clinics$analysis_tbl %>% 
    map(function(dataset) part_clinics$variables$numeric %>% 
          map(analyze_feature, 
              inp_tbl = dataset, 
              split_var = 'clust_id') %>% 
          set_names(part_clinics$variables$numeric))
  
  part_clinics$analyses_factor <- part_clinics$analysis_tbl %>% 
    map(function(dataset) part_clinics$variables$factor %>% 
          map(analyze_feature, 
              inp_tbl = dataset, 
              split_var = 'clust_id') %>% 
          set_names(part_clinics$variables$factor))
  
# Extracting statistics ----
  
  insert_msg('Extracting statistics')
  
  part_clinics$stats_numeric <- part_clinics$analyses_numeric %>% 
    map(~map_dfr(.x, extract_counts)) %>% 
    map2(., names(.), ~mutate(.x, dataset = .y))
  
  part_clinics$stats_factor <- part_clinics$analyses_factor %>% 
    map(~map_dfr(.x, extract_counts)) %>% 
    map2(., names(.), ~mutate(.x, dataset = .y))
  
# Extracting testing results ----
  
  insert_msg('Extrating testing results, FDR correction')

  part_clinics$test_numeric <- part_clinics$analyses_numeric %>% 
    map(~map_dfr(.x, extract_test_summary) %>% 
          filter(test == 'kruskal') %>% 
          mutate(p_adj = p.adjust(p_value, 'BH'), 
                 significant = ifelse(p_adj < 0.05, 'yes', 'no'), 
                 p_lab = ifelse(significant == 'yes', 
                                paste('p = ', signif(p_adj, 2)), 
                                paste0('ns (p = ', signif(p_adj, 2), ')')))) %>% 
    map2(., names(.), ~mutate(.x, dataset = .y))
  
  part_clinics$test_factor <- part_clinics$analyses_factor %>% 
    map(~map_dfr(.x, extract_test_summary) %>% 
          mutate(p_adj = p.adjust(p_value, 'BH'), 
                 significant = ifelse(p_adj < 0.05, 'yes', 'no'), 
                 p_lab = paste0('pFDR = ', signif(p_adj, 2), 
                                ', p = ', signif(p_value, 2)), 
                 p_lab = ifelse(significant == 'yes', 
                                p_lab, 
                                paste0('ns (', p_lab, ')')))) %>% 
    map2(., names(.), ~mutate(.x, dataset = .y))
  
# Plotting numeric convalescence features -----
  
  insert_msg('Numeric scores of convalescence')
  
  ## plotting table
  
  part_clinics$numeric_plot$plotting_tbl <- part_clinics$analysis_tbl %>% 
    map(select, clust_id, perf_impairment, life_quality_score, mental_health_score, stress_score) %>% 
    map(gather, 
        key = 'phenotype', 
        value = 'count', 
        perf_impairment, life_quality_score, mental_health_score, stress_score) %>% 
    map(mutate, 
        phenotype = factor(phenotype, levels = part_clinics$variables$numeric))

  ## FDR p value labels
  
  part_clinics$numeric_plot$p_values <- part_clinics$test_numeric %>%
    map(filter, 
        variable %in% c('perf_impairment', 'life_quality_score', 'mental_health_score', 'stress_score')) %>% 
    map(mutate, 
        phenotype = factor(variable, levels = part_clinics$variables$numeric)) %>% 
    map(arrange, phenotype) %>% 
    map(~.x$p_lab)
  
  ## plotting
  
  part_clinics$numeric_plot$plots <- list(inp_long_tbl = part_clinics$numeric_plot$plotting_tbl, 
                                          plot_title = c('TY: long COVID subsets', 
                                                         'STY: long COVID subsets', 
                                                         'TY: PASC subsets', 
                                                         'STY: PASC subsets'), 
                                          plot_tag = part_clust$n_tags %>% 
                                            transpose %>% 
                                            unlist(recursive = FALSE)) %>% 
    pmap(plot_violin_summary, 
         box = TRUE, 
         x_lab = '', 
         y_lab = 'Normalized score') %>% 
    map(~.x + 
          scale_x_discrete(labels = translate_var(part_clinics$variables$numeric, out_value = 'label_short')) +
          scale_fill_manual(values = part_clust$clust_colors, name = ''))
  
  ## appending with p values
  
  part_clinics$numeric_plot$plots <- list(summary_plot = part_clinics$numeric_plot$plots, 
                                          p_txt = part_clinics$numeric_plot$p_values) %>% 
    pmap(add_cluster_p) %>% 
    map(~.x + 
          scale_fill_manual(values = part_clust$clust_colors))

# Plotting demographic and clinical features ------
  
  insert_msg('Plotting the factor features differing between the clusters')
  
  ## base plots
  
  part_clinics$factor_plots <- part_clinics$analyses_factor %>% 
    map(function(dataset) list(analysis_object = dataset, 
                               label = translate_var(names(dataset), out_value = 'label_short'), 
                               fill_colors = translate_var(names(dataset), out_value = 'level_colors')) %>% 
          pmap(plot_analysis, 
               pie = F, 
               y_lab = '% participant subset', 
               cust_theme = globals$common_theme))
  
  ## appending the plots with FDR-adjusted p values
  
  part_clinics$factor_plots <- map2(part_clinics$factor_plots, 
                                    part_clinics$test_factor %>% 
                                      map(~.x$p_lab), 
                                    function(plot, p_lab) list(x = plot, y = p_lab) %>% 
                                      pmap(function(x, y) x + labs(subtitle = y)))
  
# Plotting the numbers of acute symptoms in the clusters -----
  
  insert_msg('Plotting the numbers of acute symptoms in the clusters')
  
  ## base plots
  
  part_clinics$acute_covid_plots <- part_clinics$analyses_numeric %>% 
    map(~.x$sum_symptoms_acute) %>%
    map(plot_analysis, 
         fill_colors = part_clust$clust_colors, 
         cust_theme = globals$common_theme, 
         y_lab = translate_var('sum_symptoms_acute', out_value = 'label_short'))
  
  ## appending with FDR p values
  
  part_clinics$acute_covid_plots <- part_clinics$test_numeric %>% 
    map_dfr(filter, variable == 'sum_symptoms_acute') %>% 
    mutate(p_lab = paste0('pFDR = ', signif(p_adj, 2), 
                          ', p = ', signif(p_value, 2)), 
           p_lab = ifelse(significant == 'yes', 
                          p_lab, 
                          paste0('ns (', p_lab, ')'))) %>% 
    .$p_lab %>% 
    map2(part_clinics$acute_covid_plots, ., 
         ~.x + labs(subtitle = .y)) %>% 
    map2(., c('TY: long COVID subsets', 
              'STY: long COVID subsets', 
              'TY: PASC subsets', 
              'STY: PASC subsets'), 
         ~.x + labs(title = .y))
  
# END -----
  
  insert_tail()