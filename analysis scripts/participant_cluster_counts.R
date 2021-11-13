# This script analyzes counts of MOP, HAP and FAP symptoms in the participant clusters
# The counts of symptoms are compared with Kruskal-Wallis test with FDR correction

  insert_head()
  
# data container ----
  
  part_count <- list()
  
# globals ------
  
  insert_msg('Globals')
  
  part_count$variables$long <- c('long.HAP', 
                                 'long.FAP', 
                                 'long.MOP')
  
  part_count$variables$chronic <- c('chronic.HAP', 
                                    'chronic.FAP', 
                                    'chronic.MOP')
  
  part_count$labels <- c('long.HAP' = 'HAP', 
                         'long.FAP' = 'FAP', 
                         'long.MOP' = 'MOP', 
                         'chronic.HAP' = 'MOP', 
                         'chronic.FAP' = 'FAP', 
                         'chronic.MOP' = 'MOP')
  
# analysis tables with the phenotype counts -----
  
  insert_msg('Analysis tables, phenotype counts')
  
  part_count$analysis_tbl <- part_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, extract, 'assignment') %>% 
          map(mutate, 
              ID = observation, 
              clust_id = factor(clust_id, unname(part_clust$clust_labels)))) %>% 
    transpose %>% 
    unlist(recursive = FALSE)
  
  part_count$analysis_tbl <- map2(symptom_clust$symptom_sums, 
                                  part_count$analysis_tbl, 
                                  right_join, 
                                  by = 'ID')

# serial analysis ----
  
  insert_msg('Serial analysis')
  
  part_count$analyses <- list(data = part_count$analysis_tbl, 
                              vars = list(part_count$variables$long, 
                                          part_count$variables$long, 
                                          part_count$variables$chronic, 
                                          part_count$variables$chronic)) %>% 
    pmap(function(data, vars) vars %>% 
           map(analyze_feature, 
               inp_tbl = data, 
               split_var = 'clust_id'))

  
# extracting the test results, FDR correction applied ----
  
  insert_msg('Testing results')
  
  part_count$test_summary <- part_count$analyses %>% 
    map(~map_dfr(.x, extract_test_summary)) %>% 
    map2_dfr(., names(.), ~mutate(.x, dataset = factor(.y, levels = names(part_count$analyses)))) %>% 
    filter(test == 'kruskal') %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           p_lab = ifelse(p_adj >= 0.05, 
                          paste0('ns (p = ', signif(p_adj, 2), ')'), 
                          paste('p =', signif(p_adj, 2))), 
           phenotype = stri_extract(variable, regex = 'HAP|FAP|MOP'), 
           phenotype = factor(phenotype, c('MOP', 'FAP', 'HAP')))
  
# serial plotting: box plots ----- 
  
  insert_msg('Serial plotting')
  
  ## plotting table
  
  part_count$summary_plot$plotting_tbl <- list(data = part_count$analysis_tbl, 
                                               vars = list(part_count$variables$long, 
                                                           part_count$variables$long, 
                                                           part_count$variables$chronic, 
                                                           part_count$variables$chronic)) %>% 
    pmap(function(data, vars) select(data, clust_id, all_of(vars))) %>% 
    map(gather, 
        key = 'phenotype', 
        value = 'count', 
        any_of(part_count$variables$long), 
        any_of(part_count$variables$chronic)) %>% 
    map(mutate, 
        phenotype = stri_extract(phenotype, regex = 'HAP|FAP|MOP'), 
        phenotype = factor(phenotype, c('MOP', 'FAP', 'HAP')))
  
  ## FDR p value labels
  
  part_count$summary_plot$p_values <- part_count$test_summary %>%
    arrange(dataset, phenotype) %>% 
    dlply(.(dataset), function(x) x$p_lab)
  
  ## plots
  
  part_count$summary_plot$plots <- list(inp_long_tbl = part_count$summary_plot$plotting_tbl, 
                                        plot_title = c('TY: long COVID', 
                                                       'STY: long COVID', 
                                                       'TY: PASC', 
                                                       'STY: PASC'), 
                                        plot_tag = part_clust$n_tags %>% 
                                          transpose %>% 
                                          unlist(recursive = FALSE)) %>% 
    pmap(plot_violin_summary, 
         box = TRUE)
  
  ## appending them with KW FDR-corrected p values
  
  part_count$summary_plot$plots <- list(summary_plot = part_count$summary_plot$plots, 
                                        p_txt = part_count$summary_plot$p_values) %>% 
    pmap(add_cluster_p) %>% 
    map(~.x + 
          scale_fill_manual(values = part_clust$clust_colors))
  
# END ----
  
  insert_tail()