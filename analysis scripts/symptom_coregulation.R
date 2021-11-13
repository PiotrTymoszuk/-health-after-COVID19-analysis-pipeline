# This script investigates co-occurrence/co-absence of self-reported symptoms
# by means of PAM clustering. The applied distance measure between
# the symptom vectors is simple matching distance provided by scrime package.
# Symptom clusters for acute and post-acute/chronic COVID are termed 'phenotypes'
# in the manuscript

  insert_head()
  
# Data container ----
  
  symptom_clust <- list()
  
# Globals and analysis tables: numeric encoding of the symptoms, compete, symptomatic cases ------
  
  insert_msg('Globals setup and analysis tables')
  
  ## acute and long COVID: filtering out the asymptomatic individuals at the time point, selecting the symptoms
  ## renaming
  
  plan('multisession')
  
  symptom_clust$analysis_tbl$acute <- cov_data[c('north', 'south')] %>% 
    map(filter, 
        symptom_free == 'no') %>% 
    future_map(clust_analysis_tbl, 
               sympt_suffix = 'acute', 
               .options = furrr_options(seed = TRUE))
    
  symptom_clust$analysis_tbl$long <- cov_data[c('north', 'south')] %>% 
    map(filter, 
        symptom_free == 'no', 
        long_covid == 'yes') %>% 
    future_map(clust_analysis_tbl, 
               sympt_suffix = 'long', 
               .options = furrr_options(seed = TRUE))
  
  symptom_clust$analysis_tbl$chronic <- cov_data[c('north_pasc', 'south_pasc')] %>% 
    set_names(c('north', 'south')) %>% 
    map(filter, 
        symptom_free == 'no', 
        pasc == 'yes') %>% 
    future_map(clust_analysis_tbl, 
               sympt_suffix = 'chronic', 
               .options = furrr_options(seed = TRUE))
  
  symptom_clust$analysis_tbl <- transpose(symptom_clust$analysis_tbl) %>% 
    map(~map(.x, t))
  
  plan('sequential')
  
  ## phenotype names and their short names
  
  symptom_clust$acute_pheno_names <- c('1' = 'NIP', '2' = 'MOP')
  
  symptom_clust$long_pheno_names <- c('3' = 'HAP', '2' = 'FAP', '1' = 'MOP')
  
  symptom_clust$chronic_pheno_names <- symptom_clust$long_pheno_names
  
  symptom_clust$acute_pheno_labs <- c('NIP' = 'Non-specific infection phenotype', 
                                      'MOP' = 'Multi-organ phenotype')
  
  symptom_clust$long_pheno_labs <- c('HAP' = 'Hyposmia phenotype', 
                                     'FAP' = 'Fatigue phenotype', 
                                     'MOP' = 'Multi-organ phenotype')
  
  symptom_clust$chronic_pheno_labs <- symptom_clust$long_pheno_labs

# Assessing the general clustrering tendency of the data with Hopkins statistic -----
  
  insert_msg('General clustering tendency')
  
  plan('multisession')
  
  symptom_clust$clustering_tendency <- symptom_clust$analysis_tbl %>% 
    map(function(cohort) cohort %>% 
          future_map(~get_clust_tendency(.x, nrow(.x)/2), 
                     .options = furrr_options(seed = TRUE)))
  
  plan('sequential')

# PAM clustering in the Tyrol cohort - done as before -------
  
  insert_msg('PAM clustering in the training Tyrol cohort')
  
  symptom_clust$analyses_north <- symptom_clust$analysis_tbl$north %>% 
    list(data = ., 
         k = c(2, 3, 3)) %>% 
    pmap(kcluster, 
         distance_method = 'smc', 
         clust_fun = 'pam', 
         seed = 123)
  
# Diagnostic plots for the training cohort clusters -----
  
  insert_msg('Diagnostic plots for the training cohort')
  
  symptom_clust$diagnostic_plots <- symptom_clust$analyses_north %>% 
    map(plot, 
        type = 'diagnostic', 
        cust_theme = globals$common_theme)
  
# Cluster assignment, appending with the cluster names ------
  
  insert_msg('Cluster assignment: appending with the cluster names')
  
  symptom_clust$analyses_north$acute$clust_assignment <- symptom_clust$analyses_north$acute %>% 
    extract('assignment') %>% 
    mutate(clust_origin_id = clust_id, 
           clust_id = factor(unname(symptom_clust$acute_pheno_names[as.character(clust_id)]), 
                             levels = unname(symptom_clust$acute_pheno_names)))
  
  symptom_clust$analyses_north$long$clust_assignment <- symptom_clust$analyses_north$long %>% 
    extract('assignment') %>% 
    mutate(clust_origin_id = clust_id, 
           clust_id = factor(unname(symptom_clust$long_pheno_names[as.character(clust_id)]), 
                             levels = unname(symptom_clust$long_pheno_names)))
  
  symptom_clust$analyses_north$chronic$clust_assignment <- symptom_clust$analyses_north$chronic %>% 
    extract('assignment') %>% 
    mutate(clust_origin_id = clust_id, 
           clust_id = factor(unname(symptom_clust$chronic_pheno_names[as.character(clust_id)]), 
                             levels = unname(symptom_clust$chronic_pheno_names)))
  
# Class-based prediction in the South Tyrol cohort ----
  
  insert_msg('Cluster prediction in the test South Tyrol cohort')
  
  symptom_clust$analyses_south <- list(clust_analysis_object = symptom_clust$analyses_north, 
                                       newdata = symptom_clust$analysis_tbl$south) %>% 
    pmap(predict, 
         type = 'class')
  
# Clustering variances in the North and south Tyrol cohort ----
  
  insert_msg('Clustering variances')
  
  symptom_clust$clust_variances <- symptom_clust[c('analyses_north', 'analyses_south')] %>%
    map(function(cohort) cohort %>% 
          map(var) %>% 
          map(~.x[c('total_wss', 'total_ss', 'between_ss', 'frac_var')]) %>% 
          map(as_tibble) %>% 
          map2_dfr(., names(.), ~mutate(.x, timepoint = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y))
  
# N numbers, plot tags and titles -----
  
  insert_msg('N numbers, plot tags and titles')
  
  ## tags
  
  symptom_clust$n_tags <- symptom_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, nobs)) %>% 
    map(~map(.x, ~.x$variables)) %>% 
    map(~map(.x, ~paste('\nn =', .x))) %>% 
    set_names(c('north', 'south'))
  
  ## titles
  
  symptom_clust$plot_titles <- map(globals$cohort_labs, 
                                   ~paste(.x, c('acute COVID-19', 'long COVID', 'PASC'), sep = ': '))
  
# Vectors with the major symptoms to be shown in the heat map figure ----
  
  insert_msg('Major symptoms to be shown in the figure')
  
  symptom_clust$major_symptoms <- cov_symptfreq$plotting_tbls$sympt %>% 
    filter(cohort == 'north', 
           split_var != 'subacute') %>% 
    dlply(.(split_var), top_n, 25, percent) %>% 
    map(~.x$variable)
  
  symptom_clust$major_symptoms <- symptom_clust$major_symptoms %>% 
    map(function(x) ifelse(globals$symptoms %in% x, 
                           translate_var(globals$symptoms, out_value = 'label_short'), 
                           '') %>% 
          set_names(globals$symptoms))
  
# Displaying the clustering results as heat maps ----
  
  insert_msg('Distance heat maps')
  
  ## base plots
  
  symptom_clust$heat_maps <- symptom_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, 
             plot, 
             type = 'heat_map', 
             cust_theme = globals$common_theme)) %>% 
    set_names(c('north', 'south'))

  ## adjustment

  symptom_clust$heat_maps <- c('north', 'south') %>% 
    map(function(cohort) list(plot = symptom_clust$heat_maps[[cohort]], 
                              plot_title = symptom_clust$plot_titles[[cohort]], 
                              plot_tag = symptom_clust$n_tags[[cohort]]) %>% 
          pmap(function(plot, plot_title, plot_tag) plot + 
                 labs(title = plot_title, 
                      tag = plot_tag) + 
                 theme(plot.subtitle = element_blank()) + 
                 scale_fill_gradient2(low = 'indianred3',
                                      mid = 'white', 
                                      high = 'steelblue3', 
                                      midpoint = 0.5, 
                                      limits = c(0, 1), 
                                      name = 'SMD', 
                                      oob = scales::squish) + 
                 scale_x_discrete(labels = translate_var(globals$symptom, out_value = 'label_short')) + 
                 scale_y_discrete(labels = translate_var(globals$symptom, out_value = 'label_short')))) %>% 
    set_names(c('north', 'south'))
  
# Calculating the phenotype symptom sums per participant -----
  
  insert_msg('Phenotype symptom sums')
  
  ## symptom vectors
  
  symptom_clust$sympt_assignment <- symptom_clust$analyses_north %>% 
    map(extract, 'assignment') %>% 
    map(dlply, 'clust_id', function(x) x$observation) %>% 
    map2(., names(.), function(x, y) x %>% map(~paste(.x, y, sep = '_'))) %>% 
    unlist(recursive = FALSE)
  
  ## symptom sums
  
  symptom_clust$symptom_sums <- cov_data %>% 
    map(function(cohort) symptom_clust$sympt_assignment %>% 
          map(sum_symptoms, 
              inp_tbl = cohort)) %>% 
    map(as_tibble)
  
  symptom_clust$symptom_sums <- map2(cov_data, 
                                     symptom_clust$symptom_sums, 
                                     ~mutate(.y, 
                                             ID = .x$ID, 
                                             long_covid = .x$long_covid, 
                                             pasc = .x$pasc))
  
# Displaying the sum-of-square stats for the training and test clustering -----
  
  insert_msg('Comparing the cluster sum-of-squares in the training and test cohort')
  
  symptom_clust$var_plots <- symptom_clust$clust_variances %>% 
    mutate(timepoint = factor(timepoint, c('acute', 'long', 'chronic'))) %>% 
    compare_clust_var(plot_title = 'Symptom clustering', 
                      plot_subtitle = 'Train: TY, test: STY')
  
# END ----
  
  insert_tail()