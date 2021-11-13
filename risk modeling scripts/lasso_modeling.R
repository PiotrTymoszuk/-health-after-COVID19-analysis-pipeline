# This script identifies the independent c-variate sets linked to the sum of acute symptoms
# sum of persistent symptoms, presence of any persistent symptoms, relapse and major 
# physical impairment using LASSO. No inference by LASSO is done.

  insert_head()
  
# data containers ------
  
  lasso <- list()
  
# globals: variables and analysis tables -----
  
  insert_msg('LASSO modeling globals')
  
  ## responses, symptomatic participants and complete cases only in the modeling tables
  
  lasso[c('resp_lexicon_north', 'resp_lexicon_south')] <- risk_modeling[c('resp_lexicon_north', 'resp_lexicon_south')] %>% 
    map(function(cohort) cohort %>% 
          mutate(sel_vars = map2(response, indep_variable, c), 
                 inp_tbl = map2(data, sel_vars, ~select(.data = .x, all_of(.y), freq_weight)), 
                 inp_tbl = map(inp_tbl, function(x) filter(x, complete.cases(x))), 
                 variables = indep_variable, 
                 family = stri_replace(family, fixed = 'quasi', replacement = ''), 
                 weights = map(inp_tbl, ~.x$freq_weight), 
                 type.measure = c('mae', 'mse', 'mse'), 
                 alpha = c(1, 1, 1)) %>%
          select(inp_tbl, response, family, variables, weights, type.measure))

# Serial LASSO modeling in the North Tyrol cohort, 50-fold cross-validation ------
  
  insert_msg('Training the LASSO models in the Tyrol cohort')
  
  lasso$models_north <- lasso$resp_lexicon_north %>% 
    pmap(model_boot_lasso,
         inference = F, 
         lambda_crit = 'lambda.1se',
         nfolds = 50) %>% 
    set_names(lasso$resp_lexicon_north$response)
  
# obtaining re-distribution and cross-validation error values, Tyrol cohort models -----
  
  insert_msg('Re-distribution and CV stats for the training models')
  
  lasso$stats_north <- lasso$models_north %>% 
    map(get_lasso_cv_stats) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y)) %>% 
    mutate(family = lasso$resp_lexicon_north$family)
  
# QC plots, Tyrol cohort models -----

  insert_msg('Visual QC of the train cohort models')
  
  lasso$qc_north <- list(cv_object = lasso$models_north %>% 
                           map(~.x$fit), 
                         new_data_tbl = lasso$resp_lexicon_north$inp_tbl, 
                         response = lasso$resp_lexicon_north$response,
                         variables = lasso$resp_lexicon_north$variables) %>% 
    pmap(get_qc_plots_lasso) %>% 
    set_names(lasso$resp_lexicon_north$response)
  
# predictions in the North Tyrol cohort -----
  
  insert_msg('Predictions in the North Tyrol cohort')
  
  lasso$predictions_north <- list(cv_object = lasso$models_north %>% 
                                    map(~.x$fit), 
                                  new_data_tbl = lasso$resp_lexicon_north$inp_tbl, 
                                  response = lasso$resp_lexicon_north$response,
                                  variables = lasso$resp_lexicon_north$variables, 
                                  type = 'response') %>% 
    pmap(get_qc_tbl_lasso) %>% 
    set_names(lasso$resp_lexicon_north$response)
  
# predictions in the South Tyrol cohort -----
  
  insert_msg('Predictions in South Tyrol cohort')
  
  lasso$predictions_south <- list(cv_object = lasso$models_north %>% 
                               map(~.x$fit), 
                             new_data_tbl = lasso$resp_lexicon_south$inp_tbl, 
                             response = lasso$resp_lexicon_south$response,
                             variables = lasso$resp_lexicon_south$variables, 
                             type = 'response') %>% 
    pmap(get_qc_tbl_lasso) %>% 
    set_names(lasso$resp_lexicon_south$response)
  
# obtaining external validation error values, Tyrol cohort models -----
  
  insert_msg('Re-distribution and CV stats for the training models')
  
  lasso$pred_stats_north <- list(lasso_qc_tbl = lasso$predictions_north, 
                                 family = lasso$resp_lexicon_north$family) %>% 
    pmap(get_lasso_pred_stats) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  lasso$pred_stats_south <- list(lasso_qc_tbl = lasso$predictions_south, 
                                 family = lasso$resp_lexicon_north$family) %>% 
    pmap(get_lasso_pred_stats) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))

# obtaining summary tables with the non-zero estimate values, trainint Tyrol cohort ----
  
  insert_msg('Summary tables with estimate stats')
  
  lasso$lasso_summaries_north <- lasso$models_north %>% 
    map(~.x$fit_coef) %>% 
    map(filter, estimate != 0)

# Forest plots with non-zero coefficients, appending them with model goodness stats -----
  
  insert_msg('Forest plots for non-zero coefficients')
  
  lasso$forest_plot_north <- list(lasso_summary_tbl = lasso$lasso_summaries_north, 
                                  plot_title = paste('TY:', 
                                                     translate_var(names(lasso$lasso_summaries_north), 
                                                                   out_value = 'label_short')), 
                                  x_lab = list(expression('exp '*beta[LASSO]), 
                                               expression('OR'[LASSO]), 
                                               expression('OR'[LASSO])), 
                                  plot_tag = paste('\nn =', lasso$stats_north$n)) %>%
    pmap(plot_forest_lasso) %>% 
    map(~.x + 
          scale_fill_gradient2(low = 'steelblue3', 
                               mid = 'white', 
                               high = 'indianred3', 
                               midpoint = 0, 
                               limits = c(log(0.8), log(2))) + 
          guides(fill = FALSE))
  
# Point plot for symptom count: North and south Tyrol -----
  
  insert_msg('Point plots for the predictions of the acute symptom numbers')
  
  ## plot tags
  
  lasso$fit_tags$north <- paste0('\nn = ', lasso$stats_north$n[1], 
                                 ', MAE(redistribution) = ', signif(lasso$stats_north$mae[1], 2), 
                                 ', MAE(CV) = ', signif(lasso$stats_north$error_cv[1], 2), 
                                 ' [', signif(lasso$stats_north$error_cv_lower_ci[1], 2), 
                                 ' - ', signif(lasso$stats_north$error_cv_upper_ci[1], 2), 
                                 ']')
  
  lasso$fit_tags$south <- paste0('\nn = ', lasso$pred_stats_south$n[1], 
                                 ', MAE(ext. validation) = ', signif(lasso$pred_stats_south$mae[1], 2))
  
  
  ## plots
  
  lasso$fit_plots <- list(data = list(lasso$predictions_north$sum_symptoms_acute, 
                                      lasso$predictions_south$sum_symptoms_acute), 
                          point_color = globals$cohort_colors, 
                          plot_title = paste(globals$cohort_labs, 
                                             translate_var('sum_symptoms_acute', out_value = 'label_short'), 
                                             sep = ': '), 
                          plot_tag = lasso$fit_tag) %>% 
    pmap(plot_point, 
         x_var = '.fitted', 
         y_var = 'y', 
         jitter_height = 0.05, 
         jitter_width = 0.05, 
         cust_theme = globals$common_theme, 
         x_lab = 'Model-fitted', 
         y_lab = 'True count') %>% 
    map(~.x + geom_smooth(method = 'lm')) %>% 
    set_names(c('north', 'south'))
  
# ROC plots for the binomial responses ----
  
  insert_msg('ROC plots for the binomial responses')
  
  # plot tags
  
  lasso$roc_tags$north <- 2:3 %>% 
    map(~paste0('MSE(redistribution) = ', signif(lasso$stats_north$mse[.x], 2), 
               '\nMSE(CV) = ', signif(lasso$stats_north$error_cv[.x], 2), 
               ' [', signif(lasso$stats_north$error_cv_lower_ci[.x], 2), 
               ' - ', signif(lasso$stats_north$error_cv_upper_ci[.x], 2), 
               ']',
               '\nAUC = ', signif(lasso$pred_stats_north$auc[.x], 2), 
               ' [', signif(lasso$pred_stats_north$auc_lower_ci[.x], 2), 
               ' - ', signif(lasso$pred_stats_north$auc_upper_ci[.x], 2), 
               ']')) %>% 
    map(~paste('TY:\n', .x)) %>% 
    set_names(c('long_covid', 'pasc'))
  
  lasso$roc_tags$south <- 2:3 %>% 
    map(~paste0('MSE(ext.validation) = ', signif(lasso$pred_stats_south$mse[.x], 2), 
                '\nAUC = ', signif(lasso$pred_stats_south$auc[.x], 2), 
                ' [', signif(lasso$pred_stats_south$auc_lower_ci[.x], 2), 
                ' - ', signif(lasso$pred_stats_south$auc_upper_ci[.x], 2), 
                ']')) %>%
    map(~paste('STY:\n', .x)) %>% 
    set_names(c('long_covid', 'pasc'))
  
  lasso$roc_tags <- transpose(lasso$roc_tags) %>% 
    map(~reduce(.x, paste, sep = '\n\n'))
  
  ## plots
  
  lasso$roc_plots <- list(long_covid = list(north = lasso$predictions_north$long_covid, 
                                            south = lasso$predictions_south$long_covid), 
                          pasc = list(north = lasso$predictions_north$pasc, 
                                      south = lasso$predictions_south$pasc)) %>% 
    map(function(response) map2_dfr(response, 
                                    names(response), 
                                    ~mutate(.x, cohort = .y))) %>% 
    map(plot_roc, 
        m_variable = '.fitted', 
        d_variable = 'y',
        marker_variable = 'cohort', 
        cutoffs.at = 0.5, 
        labelsize = 2.75) %>% 
    map(~.x + 
          scale_color_manual(values = globals$cohort_colors, 
                             labels = globals$cohort_labs) + 
          geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
          theme(plot.title = element_text(size = 8))) %>% 
    map2(., translate_var(c('long_covid', 'pasc'), out_value = 'label_short'), 
         ~.x + labs(title = .y)) %>% 
    map2(., lasso$roc_tags, 
         ~.x + labs(tag = .y))

# END -----
  
  insert_tail()