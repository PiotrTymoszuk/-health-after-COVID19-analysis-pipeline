# This script performs univariate risk modeling

  insert_head()
  
# data container -----
  
  risk_modeling <- list()
  
# Responses and variable lexicons -----
  
  insert_msg('Risk modeling globals')
  
  ## responses
  
  risk_modeling$resp_lexicon <- globals$var_lexicon %>% 
    filter(response == 'yes')
  
  ## independent variables
  
  risk_modeling$indep_variables  <- c(sum_symptoms_acute = 'acute_sympt_modeling', 
                                      long_covid = 'long_cov_modeling', 
                                      pasc = 'long_cov_modeling') %>% 
    map(~filter(globals$var_lexicon, 
                .data[[.x]] == 'yes')) %>% 
    map(~.x$variable) %>% 
    tibble(variable = names(.), 
           indep_variable = .)
  
  ## common table with modeling globals
  
  risk_modeling$resp_lexicon <- risk_modeling$resp_lexicon %>% 
    left_join(risk_modeling$indep_variables, 
              by = 'variable') %>% 
    mutate(response = variable) %>% 
    select(response, 
           family, 
           indep_variable)

# Appending the response lexicon with analysis tables ----
  
  insert_msg('Analysis tables')
  
  risk_modeling$resp_lexicon_north <- risk_modeling$resp_lexicon %>% 
    mutate(data = cov_data[c('north', 'north', 'north_pasc')] %>% 
             map(filter, symptom_free == 'no') %>% 
             map(mutate, 
                 long_covid = as.numeric(long_covid) - 1, 
                 pasc = as.numeric(pasc) - 1))
  
  risk_modeling$resp_lexicon_south <- risk_modeling$resp_lexicon %>% 
    mutate(data = cov_data[c('south', 'south', 'south_pasc')] %>% 
             map(filter, symptom_free == 'no') %>% 
             map(mutate, 
                 long_covid = as.numeric(long_covid) - 1, 
                 pasc = as.numeric(pasc) - 1))
  
  risk_modeling <- risk_modeling[c('resp_lexicon_north', 'resp_lexicon_south')]

# serial modeling ------
  
  insert_msg('Serial modeling')
  
  risk_modeling$analyses_north <- risk_modeling$resp_lexicon_north %>% 
    pmap(make_lm_model, 
         weight_variable = 'freq_weight', 
         confounder = 'obs_time', 
         mod_fun = glm, 
         est_transf = exp) %>% 
    set_names(risk_modeling$resp_lexicon_north$response)
  
  risk_modeling$analyses_south <- risk_modeling$resp_lexicon_south %>% 
    pmap(make_lm_model, 
         weight_variable = 'freq_weight', 
         confounder = 'obs_time', 
         mod_fun = glm, 
         est_transf = exp) %>% 
    set_names(risk_modeling$resp_lexicon_south$response)
  
# Modeling summary, correction for multiple testing (BH) -----
  
  insert_msg('Model summary')
  
  risk_modeling$summaries_north <- risk_modeling$analyses_north %>% 
    map(get_model_summary)
  
  risk_modeling$summaries_south <- risk_modeling$analyses_south %>% 
    map(get_model_summary)
  
# Identification of significant non-confounder risk-modifying variables in each cohort and common -----
  
  insert_msg('Identification of the significant risk-modifying factors')
  
  risk_modeling$signif_fcts_north <- risk_modeling$summaries_north %>% 
    map(identify_significant, 
        confounder = 'obs_time')
  
  risk_modeling$signif_fcts_south <- risk_modeling$summaries_south %>% 
    map(identify_significant, 
        confounder = 'obs_time')
  
  risk_modeling$signif_fcts_common <- map2(risk_modeling$signif_fcts_north, 
                                           risk_modeling$signif_fcts_south,  
                                           intersect)
  
# Displaying common significant favorable and unfavorable factors in both cohorts in Venn plots ----
  
  insert_msg('Venn plots with common significant factors')
  
  risk_modeling$venn_plots <- list(signif_fct_north = risk_modeling$signif_fcts_north, 
                                   signif_fct_south = risk_modeling$signif_fcts_south, 
                                   plot_title = translate_var(names(risk_modeling$signif_fcts_north), 
                                                              out_value = 'label_short'), 
                                   plot_subtitle = risk_modeling$resp_lexicon_north$indep_variable %>% 
                                     map(length) %>% 
                                     paste('candidate factors')) %>% 
    pmap(plot_venn)
  
# A summary table with common significant factors in both cohorts identified for each response -----
  
  insert_msg('Common summary table with the factors significant in both cohorts')
  
  risk_modeling$signif_fct_tbls <- list(fct_vector = risk_modeling$signif_fcts_common, 
                                        modeling_summary_north = risk_modeling$summaries_north, 
                                        modeling_summary_south = risk_modeling$summaries_south) %>% 
    pmap(common_summary_tbl)
  
# Aggregating the betas and their errors (inverse variance weighting) for the paper text ------
  
  insert_msg('Beta aggregation between the cohorts')
  
  risk_modeling$pooled_betas <- risk_modeling$signif_fct_tbls %>% 
    map(aggregate_modeling)
  
# Forest plots with top 10 strongest factors for both cohorts ------  
  
  insert_msg('Forest plots with top 10 strongest risk modifying factors')
  
  risk_modeling$forest_plots <- list(modeling_summary = risk_modeling$signif_fct_tbls, 
                                     plot_title = names(risk_modeling$signif_fct_tbls) %>% 
                                       translate_var(out_value = 'label_short'), 
                                     x_lab = list(expression('exp '*beta), 
                                                  'OR', 
                                                  'OR')) %>% 
    pmap(plot_forest_cohorts, 
         x_transf = 'log2', 
         plot_subtitle = 'Top 10 influential positive and negative factors', 
         n_top = 10)

# END -----
  
  insert_tail()