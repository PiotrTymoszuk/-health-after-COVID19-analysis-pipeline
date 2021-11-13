# This script analyzes the disease perception (character, severe illness feeling)
# max. duration, recurrence and symptom number trajectories

  insert_head()
  
# data container -----
  
  sympt_traj <- list()
  
# variables of interest and the analysis table -----
  
  insert_msg('Globals setup and analysis table')
  
  ## variables of interest

  sympt_traj$variables <- c('sum_symptoms_acute', 
                                       'sum_symptoms_immediate', 
                                       'sum_symptoms_subacute', 
                                       'sum_symptoms_long')
  
  ## analysis table: first four weeks
  
  sympt_traj$kinet_tbl <- cov_data[c('north', 'south')] %>% 
    map(gather, 
        key = 'timepoint', 
        value = 'symptom_sum', 
        sympt_traj$variable) %>% 
    map(mutate, 
        timepoint = car::recode(timepoint, 
                                "'sum_symptoms_acute' = 0; 
                                'sum_symptoms_immediate' = 1;
                                'sum_symptoms_subacute' = 2;
                                'sum_symptoms_long' = 4")) %>% 
    map(select, 
        ID, 
        timepoint, 
        symptom_sum, 
        cohort, 
        long_covid, 
        pasc, 
        obs_time)
  
  ## analysis table: PASC analyses
  
  sympt_traj$pasc_kinet_tbl <- cov_data[c('north_pasc', 'south_pasc')] %>% 
    map(gather, 
        key = 'timepoint', 
        value = 'symptom_sum', 
        c(sympt_traj$variable, 'sum_symptoms_chronic')) %>% 
    map(mutate, 
        timepoint = car::recode(timepoint, 
                                "'sum_symptoms_acute' = 0; 
                                'sum_symptoms_immediate' = 1;
                                'sum_symptoms_subacute' = 2;
                                'sum_symptoms_long' = 4; 
                                'sum_symptoms_chronic' = 12")) %>% 
    map(select, 
        ID, 
        timepoint, 
        symptom_sum, 
        cohort, 
        long_covid, 
        pasc, 
        obs_time)
  
# GLM symptom trajectory modeling for the whole cohort -----
  
  ## GLM models and summaries
  
  plan('multisession')
  
  sympt_traj$whole[c('models', 
                          'summary', 
                          'stat_tbl')] <- sympt_traj$kinet_tbl %>% 
    future_map(model_trajectory, 
               response = 'symptom_sum', 
               fixed_vars = 'timepoint', 
               .options = furrr_options(seed = TRUE)) %>% 
    transpose
  
  plan('sequential')
  
  sympt_traj$whole$summary <- sympt_traj$whole$summary %>% 
    map(mutate, 
        beta_lab = c('\u03B2(0)', '\u03B2(time)'), 
        plot_lab = paste(beta_lab, plot_label, sep = ' = '))
  
  ## plotting the trajectories
  
  sympt_traj$whole$mod_labs <- sympt_traj$whole$summary %>% 
    map(~paste(.x$plot_lab, collapse = '\n'))
  
  sympt_traj$whole$plots <- list(inp_tbl = sympt_traj$kinet_tbl, 
                                      stat_tbl = sympt_traj$whole$stat_tbl, 
                                      ribbon_color = globals$cohort_colors, 
                                      median_color = c('steelblue4', 'lightsalmon4'), 
                                      plot_title = globals$cohort_labs, 
                                      plot_tag = sympt_traj$whole$stat_tbl %>% 
                                        map_dbl(~.x$n_complete[1]) %>% 
                                        paste('\nn =', .)) %>% 
    pmap(plot_trajectory, 
         x_var = 'timepoint', 
         y_var = 'symptom_sum', 
         line_color = 'gray50', 
         x_lab = 'Time after clinical onset, weeks', 
         y_lab = '# symptoms')
  
  sympt_traj$whole$plots <- map2(sympt_traj$whole$plots, 
                                      sympt_traj$whole$mod_labs, 
                                      ~.x + 
                                        annotate('text', 
                                                 label = .y, 
                                                 size = 2.75, 
                                                 hjust = 1, 
                                                 vjust = 1, 
                                                 x = 4, 
                                                 y = 40) + 
                                        scale_x_continuous(breaks = c(0, 1, 2, 4)) + 
                                        scale_y_continuous(limits = c(0, 42)))
  
  ## panel of plots
  
  sympt_traj$whole$panel <- plot_grid(plotlist = sympt_traj$whole$plots, 
                                           ncol = 2, 
                                           align = 'hv')
  
# modeling the differences in symptom sum trajectories between the individuals with/without long COVID -----
  
  insert_msg('Modeling the differences in symtom sums between the long COVID and symptom-free individuals')
  
  ## GLM models and summaries
  
  plan('multisession')
  
  sympt_traj$long_covid[c('models', 
                          'summary', 
                          'stat_tbl')] <- sympt_traj$kinet_tbl %>% 
    future_map(model_trajectory, 
               response = 'symptom_sum', 
               fixed_vars = c('timepoint', 'long_covid'),
               .options = furrr_options(seed = TRUE)) %>% 
    transpose
  
  plan('sequential')
  
  sympt_traj$long_covid$summary <- sympt_traj$long_covid$summary %>% 
    map(mutate, 
        beta_lab = c('\u03B2(0)', 
                     '\u03B2(time)', 
                     '\u03B2(long COVID)', 
                     '\u03B2(interaction)'), 
        plot_lab = paste(beta_lab, plot_label, sep = ' = '))

  ## plotting the trajectories as medians with IQRs
  
  sympt_traj$long_covid$mod_labs <- sympt_traj$long_covid$summary %>% 
    map(~paste(.x$plot_lab, collapse = '\n'))
  
  sympt_traj$long_covid$plot_tag <- sympt_traj$long_covid$stat_tbl %>% 
    map(~paste0('\nNo persist.sympt.: n = ', 
                .x$n_complete[1], 
                '\nlong COVID: n = ', 
                .x$n_complete[2]))

  sympt_traj$long_covid$plots <- list(inp_tbl = sympt_traj$kinet_tbl %>% map(filter, !is.na(long_covid)), 
                                      stat_tbl = sympt_traj$long_covid$stat_tbl %>% map(filter, !is.na(long_covid)), 
                                      plot_title = globals$cohort_labs, 
                                      plot_tag = sympt_traj$long_covid$plot_tag) %>% 
    pmap(plot_trajectory, 
         x_var = 'timepoint', 
         y_var = 'symptom_sum', 
         split_var = 'long_covid', 
         x_lab = 'Time after clinical onset, weeks', 
         y_lab = '# symptoms')
  
  sympt_traj$long_covid$plots <- map2(sympt_traj$long_covid$plots, 
                                      sympt_traj$long_covid$mod_labs, 
                                      ~.x + 
                                        scale_x_continuous(breaks = c(0, 1, 2, 4)) + 
                                        scale_y_continuous(limits = c(0, 24)) + 
                                        annotate('text', 
                                                 label = .y, 
                                                 hjust = 1, 
                                                 vjust = 1, 
                                                 x = 4, 
                                                 y = 23, 
                                                 size = 2.75) + 
                                        scale_fill_manual(values = c(no = 'steelblue4', 
                                                                     yes = 'firebrick4'), 
                                                          name = 'long COVID') + 
                                        scale_color_manual(values = c(no = 'steelblue4', 
                                                                      yes = 'firebrick4')) + 
                                        guides(color = F))
  
  ## panel of plots
  
  sympt_traj$long_covid$panel <- sympt_traj$long_covid$plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(sympt_traj$long_covid$plots$north), 
              ncol = 2, 
              rel_widths = c(0.89, 0.11))

# modeling the differences in symptom sum trajectories between the individuals with/without PASC (subset with obs time >= 90) -----
  
  insert_msg('Modeling the differences in symtom sums between the PASC and symptom-free individuals')
  
  ## GLM models and summaries
  
  plan('multisession')
  
  sympt_traj$pasc[c('models', 
                    'summary', 
                    'stat_tbl')] <- sympt_traj$pasc_kinet_tbl %>% 
    future_map(model_trajectory, 
               response = 'symptom_sum', 
               fixed_vars = c('timepoint', 'pasc'), 
               .options = furrr_options(seed = TRUE)) %>% 
    transpose
  
  plan('sequential')
  
  sympt_traj$pasc$summary <- sympt_traj$pasc$summary %>% 
    map(mutate, 
        beta_lab = c('\u03B2(0)', 
                     '\u03B2(time)', 
                     '\u03B2(PASC)', 
                     '\u03B2(interaction)'), 
        plot_lab = paste(beta_lab, plot_label, sep = ' = '))
  
  ## plotting the trajectories as medians with IQRs
  
  sympt_traj$pasc$mod_labs <- sympt_traj$pasc$summary %>% 
    map(~paste(.x$plot_lab, collapse = '\n'))
  
  sympt_traj$pasc$plot_tag <- sympt_traj$pasc$stat_tbl %>% 
    map(~paste0('\nNo PASC.: n = ', 
                .x$n_complete[1], 
                '\nPASC: n = ', 
                .x$n_complete[2]))
  
  sympt_traj$pasc$plots <- list(inp_tbl = sympt_traj$pasc_kinet_tbl %>% map(filter, !is.na(pasc)), 
                                stat_tbl = sympt_traj$pasc$stat_tbl %>% map(filter, !is.na(pasc)), 
                                plot_title = globals$cohort_labs, 
                                plot_tag = sympt_traj$pasc$plot_tag) %>% 
    pmap(plot_trajectory, 
         x_var = 'timepoint', 
         y_var = 'symptom_sum', 
         split_var = 'pasc', 
         x_lab = 'Time after clinical onset, weeks', 
         y_lab = '# symptoms')
  
  sympt_traj$pasc$plots <- map2(sympt_traj$pasc$plots, 
                                sympt_traj$pasc$mod_labs, 
                                ~.x + 
                                  scale_x_continuous(breaks = c(0, 1, 2, 4, 12)) + 
                                  scale_y_continuous(limits = c(0, 24)) + 
                                  annotate('text', 
                                           label = .y, 
                                           hjust = 1, 
                                           vjust = 1, 
                                           x = 12, 
                                           y = 23, 
                                           size = 2.75) + 
                                  scale_fill_manual(values = c(no = 'steelblue4', 
                                                               yes = 'firebrick4'), 
                                                    name = 'PASC') + 
                                  scale_color_manual(values = c(no = 'steelblue4', 
                                                                yes = 'firebrick4')) + 
                                  guides(color = F))
  
  ## panel of plots
  
  sympt_traj$pasc$panel <- sympt_traj$pasc$plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(sympt_traj$pasc$plots$north), 
              ncol = 2, 
              rel_widths = c(0.89, 0.11))
  
# END -----
  
  insert_tail()