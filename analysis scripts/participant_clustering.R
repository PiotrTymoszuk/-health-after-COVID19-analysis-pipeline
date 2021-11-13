# This script performs clustering of symptomatic participants in long COVID-19 and PASC
# by the number of the phenotype symptom sums. The method of choice is dbscan
# The clusters are developed in the Tyrol cohort and 'predicted' by kNN label propagation
# in the South Tyrol cohort

  insert_head()
  
# data container -----
  
  part_clust <- list()
  
# globals: cluster labels and colors -----
  
  insert_msg('Globals: cluster labels')
  
  part_clust$clust_labels <- c('0' = 'noise', 
                               '1' = 'HAP-', 
                               '2' = 'HAPi', 
                               '3' = 'HAP+')
  
  part_clust$clust_colors <- c('noise' = 'gray60', 
                               'HAP-' = 'coral3', 
                               'HAPi' = 'steelblue', 
                               'HAP+' = 'darkolivegreen4')
  
# analysis tables with normalized counts of phenotype symptoms -----
  
  insert_msg('Analysis tables')
  
  part_clust$analysis_tbl$long <- symptom_clust$symptom_sums[c('north', 'south')] %>% 
    map(filter, long_covid == 'yes') %>% 
    map(select, ID, starts_with('long.')) %>% 
    map(~map_dfc(.x, function(x) if(is.numeric(x)) min_max(x) else x)) %>% 
    map(column_to_rownames, 'ID')
  
  part_clust$analysis_tbl$chronic <- symptom_clust$symptom_sums[c('north_pasc', 'south_pasc')] %>% 
    set_names(c('north', 'south')) %>% 
    map(filter, pasc == 'yes') %>% 
    map(select, ID, starts_with('chronic.')) %>% 
    map(~map_dfc(.x, function(x) if(is.numeric(x)) min_max(x) else x)) %>% 
    map(column_to_rownames, 'ID')
  
  part_clust$analysis_tbl <- transpose(part_clust$analysis_tbl)
  
# Assessing the general clustering tendency of the data with Hopkins statistic -----
  
  insert_msg('General clustering tendency')
  
  plan('multisession')
  
  part_clust$clustering_tendency <- part_clust$analysis_tbl %>% 
    future_map(function(cohort) cohort %>% 
                 map(~get_clust_tendency(.x, nrow(.x)/2)), 
               .options = furrr_options(seed = TRUE))
  
  plan('sequential')
  
# Cluster development in the North Tyrol cohort ----
  
  insert_msg('Cluster development in the North Tyrol cohort')
  
  part_clust$analyses_north <- list(data = part_clust$analysis_tbl$north, 
                                    eps = c(0.25, 0.4)) %>% 
    pmap(dbscan_cluster, 
         distance_method = 'manhattan', 
         minPts = 5)
  
# Diagnostic plots ----
  
  insert_msg('Diagnostic plots')
  
  part_clust$diagnostic_plots <- part_clust$analyses_north %>% 
    map(plot, type = 'diagnostic', cust_theme = globals$common_theme)
  
# Cluster assignment, appending with the cluster names ------
  
  insert_msg('Cluster assignment: appending with the cluster names')

  part_clust$analyses_north$long$clust_assignment <- part_clust$analyses_north$long %>% 
    extract('assignment') %>% 
    mutate(clust_origin_id = clust_id, 
           clust_id = factor(unname(part_clust$clust_labels[as.character(clust_id)]), 
                             levels = unname(part_clust$clust_labels)))
  
  part_clust$analyses_north$chronic$clust_assignment <- part_clust$analyses_north$chronic %>% 
    extract('assignment') %>% 
    mutate(clust_origin_id = clust_id, 
           clust_id = factor(unname(part_clust$clust_labels[as.character(clust_id)]), 
                             levels = unname(part_clust$clust_labels)))
  
# Prediction of the cluster assignment in the South Tyrol cohort -----
  
  insert_msg('Cluster prediction in the South Tyrol cohort')
  
  part_clust$analyses_south <- list(clust_analysis_object = part_clust$analyses_north, 
                                    newdata = part_clust$analysis_tbl$south) %>% 
    pmap(predict, 
         type = 'propagation', 
         k = 50, 
         simple_vote = FALSE)
  
# Cluster assignment, appending with the cluster names in the South Tyrol cohort ------
  
  insert_msg('Cluster assignment: appending with the cluster names')
  
  part_clust$analyses_south$long$clust_assignment <- part_clust$analyses_south$long %>% 
    extract('assignment') %>% 
    mutate(clust_id = factor(clust_id, levels = unname(part_clust$clust_labels)))
  
  part_clust$analyses_south$chronic$clust_assignment <- part_clust$analyses_south$chronic %>% 
    extract('assignment') %>% 
    mutate(clust_id = factor(clust_id, levels = unname(part_clust$clust_labels)))
  
# Clustering variances in the North and south Tyrol cohort ----
  
  insert_msg('Clustering variances')
  
  part_clust$clust_variances <- part_clust[c('analyses_north', 'analyses_south')] %>%
    map(function(cohort) cohort %>% 
          map(var) %>% 
          map(~.x[c('total_wss', 'total_ss', 'between_ss', 'frac_var')]) %>% 
          map(as_tibble) %>% 
          map2_dfr(., names(.), ~mutate(.x, timepoint = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y))
  
# N numbers, plot tags and titles -----
  
  insert_msg('N numbers, plot tags and titles')
  
  ## tags
  
  part_clust$n_tags <- part_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, ngroups) %>% 
          map(filter, clust_id != 'noise') %>% 
          map(~paste0('\nHAP-: n = ', .x$n[1], 
                      ', HAPi: n = ', .x$n[2], 
                      ', HAP+: n = ', .x$n[3]))) %>% 
    set_names(c('north', 'south'))
  
  ## titles
  
  part_clust$plot_titles <- map(globals$cohort_labs, 
                                ~paste(.x, c('long COVID', 'PASC'), sep = ': ')) %>% 
    set_names(c('north', 'south'))
  
# PCA plots ----
  
  insert_msg('PCA plots')
  
  ## base plots
  
  part_clust$pca_plots <- part_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, 
             plot, 
             type = 'components',
             red_fun = 'pca', 
             with = 'distance', 
             kdim = 2, 
             cust_theme = globals$common_theme, 
             jitter_width = 0.3, 
             jitter_height = 0.3)) %>% 
    set_names(c('north', 'south'))
  
  ## adjustments
  
  part_clust$pca_plots <- c('north', 'south') %>% 
    map(function(cohort) list(plot = part_clust$pca_plots[[cohort]], 
                              plot_title = part_clust$plot_titles[[cohort]], 
                              plot_tag = part_clust$n_tags[[cohort]]) %>% 
          pmap(function(plot, plot_title, plot_tag) plot + 
                 labs(title = plot_title, 
                      tag = plot_tag) + 
                 theme(plot.subtitle = element_blank()) + 
                 scale_fill_manual(values = part_clust$clust_colors, 
                                   name = ''))) %>% 
    set_names(c('north', 'south'))
  
# Displaying the sum-of-square stats for the training and test clustering -----
  
  insert_msg('Comparing the cluster sum-of-squares in the training and test cohort')
  
  part_clust$var_plots <- part_clust$clust_variances %>% 
    mutate(timepoint = factor(timepoint, c('long', 'chronic'))) %>% 
    compare_clust_var(plot_title = 'Symptom clustering', 
                      plot_subtitle = 'Train: TY, test: STY')  
  
# END -----
  
  insert_tail()