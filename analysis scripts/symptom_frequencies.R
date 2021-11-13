# This script calculates and visualizes frequencies of self-reported symptoms in acute
# and post-acute COVID-19

  insert_head()
  
# data container ----
  
  cov_symptfreq <- list()

# Symptom variables ----
  
  insert_msg('Setting the symptoms variables')
  
  ## timepoints and indexes of symptom presence
  
  cov_symptfreq$timepoints <- c('acute', 
                                'subacute', 
                                'long', 
                                'chronic')
  
  cov_symptfreq$symptomatic_vars <- c('acute_covid', 
                                      'subacute_covid', 
                                      'long_covid', 
                                      'pasc')
  
  ## vectors with symptom names in the acute and post-acute setting
  
  cov_symptfreq$symptom_vars <- cov_symptfreq$timepoints %>% 
    map(~paste(globals$symptoms, .x, sep = '_')) %>% 
    set_names(cov_symptfreq$timepoints)
  
# Analysis tables: for symptomatic patients at the given time point only-----
  
  insert_msg('Analysis tables for the symptomatic cases')
  
  cov_symptfreq$symptomatic_tbl <- cov_data[c('north', 'south')] %>% 
    map(function(cohort) cov_symptfreq$symptomatic_vars %>% 
          map(function(sympt_var) filter(cohort, .data[[sympt_var]] == 'yes')) %>% 
          map2(., cov_symptfreq$symptom_vars, 
               ~select(.x, ID, all_of(.y))) %>% 
          map(set_names, c('ID', globals$symptoms)) %>% 
          set_names(cov_symptfreq$timepoints) %>%  
          map2_dfr(., names(.), ~mutate(.x, timepoint = factor(.y, levels = cov_symptfreq$timepoints))))
  
# Analysis tables: first four weeks, all participants ----
  
  insert_msg('Analysis tables for all participants, first four weeks')
  
  cov_symptfreq$all_tbl <- cov_data[c('north', 'south')] %>% 
    map(function(cohort) cov_symptfreq$symptom_vars[c('acute', 'subacute', 'long')] %>% 
          map(~select(cohort, ID, all_of(.x))) %>% 
          map(set_names, c('ID', globals$symptoms)) %>% 
          set_names(cov_symptfreq$timepoints[cov_symptfreq$timepoints != 'chronic']) %>% 
          map2_dfr(., names(.), ~mutate(.x, timepoint = factor(.y, levels = cov_symptfreq$timepoints))))

# Plot tags with the n numbers per time point ----
  
  insert_msg('Plot tags with the n numbers')
  
  cov_symptfreq$n_numbers <- cov_symptfreq[c('symptomatic_tbl', 'all_tbl')] %>% 
    map(~map(.x, count, timepoint)) %>% 
    set_names(c('sympt', 'cohort'))
  
  ## n number tags
  
  cov_symptfreq$n_tags$sympt <- cov_symptfreq$n_numbers$sympt %>% 
    map(~paste0('0 - 2: n = ', .x$n[1], 
                ', 2 - 4: n = ', .x$n[2], 
                ', 4 - 12: n = ', .x$n[3], 
                ', > 12: n = ', .x$n[4])) %>% 
    map2_chr(globals$cohort_labs, ., paste, sep = ': ') %>% 
    paste(collapse = '\n') %>% 
    paste0('\n', .)
  
  cov_symptfreq$n_tags$cohort <- map2_chr(globals$cohort_labs, cov_symptfreq$n_numbers$cohort, 
                                          ~paste0(.x, ': n = ', .y$n[1])) %>% 
    paste(collapse = ', ') %>% 
    paste0('\n', .)
  
# Counting the symptom variables as frequencies of the whole cohort and symptomatic cases -----
  
  insert_msg('Counting the symptoms')
  
  ## symptom counts and significance for the difference in time, chi squared test for trend
  ## adjustment for multiple testing by Benjamini-Hochberg
  
  cov_symptfreq$plotting_tbls <- cov_symptfreq[c('symptomatic_tbl', 
                                                 'all_tbl')] %>% 
    map(analyze_symptoms) %>% 
    set_names(c('sympt', 
                'cohort'))

# Bubble plot representation of frequencies within symptomatic in the time point and frequencies within the cohort  ----
  
  insert_msg('Bubble plot of symptom frequencies')
  
  cov_symptfreq$bubble_plots <- list(inp_tbl = cov_symptfreq$plotting_tbls, 
                                     plot_subtitle = c('Participants with at least one symptom at the given time point', 
                                                       'All participants'), 
                                     legend_name = c('% symptomatic\nparticipants\nat the time point', 
                                                     '% all participants'), 
                                     plot_tag = cov_symptfreq$n_tags) %>% 
    pmap(plot_bubble, 
         fill_size_var = 'percent', 
         label_var = 'plot_lab', 
         x_var = 'split_var', 
         y_var = 'symptom_label', 
         plot_title = 'Symptom frequency', 
         x_lab = 'Time after clinical onset, weeks')
  
  ## splitting by the cohort
  
  cov_symptfreq$bubble_plots <- map2(cov_symptfreq$bubble_plots, 
                                     list(c('acute' = '0 - 2', 
                                            'subacute' = '2 - 4', 
                                            'long' = '4 - 12', 
                                            'chronic' = '> 12'), 
                                          c('acute' = '0 - 2', 
                                            'subacute' = '2 - 4', 
                                            'long' = '> 4')), 
                                     ~.x + 
                                       facet_grid(. ~ cohort, 
                                                  labeller = as_labeller(globals$cohort_labs)) + 
                                       theme(strip.background = element_rect(fill = 'gray95')) + 
                                       scale_x_discrete(labels = .y))

# Top 10 most frequent symptoms per time-point ----
  
  insert_msg('Top 10 plots')
  
  ## plotting table
  
  cov_symptfreq$top_10_table <- cov_symptfreq$plotting_tbls$sympt %>% 
    dlply(.(cohort, split_var), top_n, 10, percent) %>% 
    map(arrange, percent) %>%  
    map_dfr(function(x) mutate(x, y_txt = 1:nrow(x))) %>% 
    mutate(symptom_label = translate_var(variable, out_value = 'label_short')) %>% 
    as_tibble

  ## label plot
  
  cov_symptfreq$top_10_plot <- cov_symptfreq$top_10_table %>% 
    ggplot(aes(x = split_var, 
               y = y_txt, 
               label = symptom_label, 
               fill = percent)) + 
    geom_label(aes(label = symptom_label), 
               size = 2.6) + 
    scale_fill_gradient2(high = 'firebrick4', 
                         low = 'steelblue4', 
                         mid = 'white', 
                         midpoint = 50, 
                         name = '% symptomatic\nparticipants\nat the timepoint', 
                         breaks = seq(0, 100, by = 20))  + 
    scale_y_continuous(breaks = 1:10, 
                       labels = 10:1) + 
    scale_x_discrete(labels = c('acute' = '0 - 2', 
                                'subacute' = '2 - 4', 
                                'long' = '4 - 12', 
                                'chronic' = '> 12')) + 
    globals$common_theme + 
    theme(strip.background = element_rect(fill = 'gray95')) + 
    labs(x = 'Time after clinical onset, weeks', 
         y = 'Frequency Rank', 
         title = 'Top 10 most frequent symptoms', 
         subtitle = 'Participants with at least one symptom at the given time point', 
         tag = cov_symptfreq$bubble_plots$sympt$labels$tag) + 
    facet_grid(. ~ cohort, 
               labeller = as_labeller(globals$cohort_labs))
  
# END ----
  
  insert_tail()
  