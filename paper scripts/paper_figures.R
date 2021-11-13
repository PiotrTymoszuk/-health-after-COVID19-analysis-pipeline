# This script generates paper figures

  insert_head()
  
# tools -----
  
  library(cowplot)

# data containers -----
  
  paper_figures <- list()
  suppl_figures <- list()
  
# Figure 1: CONSORT plot -----
  
  insert_msg('Figure 1: CONSORT plot')
  
  paper_figures$consort <- ggdraw() + 
    draw_image('./study consort/consort_diagram.png') + 
    theme(plot.margin = globals$common_margin)
  
  paper_figures$consort <- paper_figures$consort %>% 
    as_figure_object(figure_label = 'figure_1_consort', 
                     w = 180, 
                     h = 180)
  
# Figure 2: symptom presence and symptoms sum trajectories in time ------
  
  insert_msg('Figure 3: symptom sum trajectories')
  
  paper_figures$symptom_sum <- plot_grid(plot_grid(cov_course$sympt_presence$plot, 
                                                   ggdraw(), 
                                                   ncol = 2, 
                                                   rel_widths = c(0.7, 0.3)), 
                                         plot_grid(sympt_traj$whole$panel, 
                                                   ggdraw(), 
                                                   ncol = 2, 
                                                   rel_widths = c(0.9, 0.1)), 
                                         sympt_traj$long_covid$panel,
                                         nrow = 3, 
                                         align = 'hv', 
                                         labels = c('A', 'B', 'C'), 
                                         label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_2_symptom_sum_trajectory', 
                     w = 180, 
                     h = 210)
  
# Figure 3: symptom frequencies in the symptomatic collective at the time points -----
  
  insert_msg('Figure 3: symptom frequencies in the symptomatic individuals at the given time points')
  
  paper_figures$frequencies_sympt <- cov_symptfreq$bubble_plots$sympt + 
    theme(legend.position = 'bottom')
  
  paper_figures$frequencies_sympt <- paper_figures$frequencies_sympt %>% 
    as_figure_object(figure_label = 'figure_3_symptom_frequency', 
                     w = 180, 
                     h = 210)
  
# Figure 4: phenotypes of acute COVID-19, Tyrol -----
  
  insert_msg('Figure 4: phenotypes of acute COVID-19')
  
  paper_figures$acute_phenotypes <- plot_grid(symptom_clust$heat_maps$north$acute + 
                                                theme(legend.position = 'bottom')) %>% 
    as_figure_object(figure_label = 'figure_4_acute_phenotypes', 
                     w = 180, 
                     h = 190)
  
# Figure 5: phenotypes of long COVID, Tyrol -----
  
  insert_msg('Figure 5: phenotypes of long COVID')
  
  paper_figures$long_phenotypes <- plot_grid(symptom_clust$heat_maps$north$long + 
                                               theme(legend.position = 'bottom')) %>% 
    as_figure_object(figure_label = 'figure_5_long_phenotypes', 
                     w = 180, 
                     h = 210)

# Figure 6: subsets of participants in long COVID -----
  
  insert_msg('Figure 6: subsets in long COVID-19')
  
  paper_figures$long_covid_subsets$top_panel <- plot_grid(part_clust$pca_plots$north$long + 
                                                            theme(legend.position = 'none'), 
                                                          part_clust$pca_plots$south$long + 
                                                            theme(legend.position = 'none'), 
                                                          get_legend(part_clust$pca_plots$north$long), 
                                                          ncol = 3, 
                                                          align = 'hv', 
                                                          axis = 'tblr', 
                                                          rel_widths = c(1, 1, 0.28))
  
  paper_figures$long_covid_subsets$middle_panel <- plot_grid(part_count$summary_plot$plots$north + 
                                                               theme(legend.position = 'none', 
                                                                     plot.tag = element_blank()), 
                                                             part_count$summary_plot$plots$south + 
                                                               theme(legend.position = 'none', 
                                                                     plot.tag = element_blank()), 
                                                             get_legend(part_clust$pca_plots$north$chronic), 
                                                             ncol = 3, 
                                                             align = 'hv', 
                                                             axis = 'tblr', 
                                                             rel_widths = c(1, 1, 0.28))
  
  paper_figures$long_covid_subsets$bottom_panel <- plot_grid(part_sympt$heat_maps$north + 
                                                               theme(legend.position = 'none', 
                                                                     plot.tag = element_blank(), 
                                                                     strip.background.y = element_blank(), 
                                                                     strip.text.y = element_blank()), 
                                                             part_sympt$heat_maps$south + 
                                                               theme(legend.position = 'none', 
                                                                     plot.tag = element_blank(), 
                                                                     axis.text.y = element_blank()), 
                                                             ncol = 2, 
                                                             rel_widths = c(0.53, 0.47), 
                                                             align = 'h') %>% 
    plot_grid(get_legend(part_sympt$heat_maps$north + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  paper_figures$long_covid_subsets <- plot_grid(paper_figures$long_covid_subsets$top_panel,
                                                paper_figures$long_covid_subsets$middle_panel,
                                                paper_figures$long_covid_subsets$bottom_panel, 
                                                nrow = 3, 
                                                rel_heights = c(1, 1, 1.3), 
                                                labels = LETTERS, 
                                                label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_6_long_covid_subsets',
                     w = 180, 
                     h = 230)
  
# Figure 7: the most essential features of clinical background of the long COVID participant clusters -------
  
  insert_msg('Figure 7: clinical background of the long COVID subsets')
  
  paper_figures$long_covid_clinics <- plot_grid(make_fct_panel(part_clinics$factor_plots$north$sex, 
                                                               part_clinics$factor_plots$south$sex), 
                                                make_fct_panel(part_clinics$factor_plots$north$bmi_class_before, 
                                                               part_clinics$factor_plots$south$bmi_class_before), 
                                                make_fct_panel(part_clinics$factor_plots$north$multi_morbidity, 
                                                               part_clinics$factor_plots$south$multi_morbidity), 
                                                make_fct_panel(part_clinics$factor_plots$north$therapy_home_antibiotic, 
                                                               part_clinics$factor_plots$south$therapy_home_antibiotic, 
                                                               show_tag = TRUE), 
                                                nrow = 4, 
                                                labels = LETTERS, 
                                                label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_7_long_covid_clinics', 
                     w = 180, 
                     h = 220)
  
# Figure 8: recovery features in the long COVID-19 participant clusters -----
  
  insert_msg('Figure 8: recovery features in the long COVID-19 subsets')
  
  paper_figures$long_covid_recovery$top_panel <- plot_grid(part_clinics$numeric_plot$plots$north + 
                                                             theme(legend.position = 'none'), 
                                                           part_clinics$numeric_plot$plots$south + 
                                                             theme(legend.position = 'none'), 
                                                           ncol = 2, 
                                                           align = 'hv', 
                                                           axis = 'tblr') %>% 
    plot_grid(get_legend(part_clust$pca_plots$north$chronic + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15))
  
  paper_figures$long_covid_recovery$bottom_panel <- plot_grid(make_fct_panel(part_clinics$factor_plots$north$complete_covelescence, 
                                                                             part_clinics$factor_plots$south$complete_covelescence, 
                                                                             show_legend = FALSE), 
                                                              make_fct_panel(part_clinics$factor_plots$north$relapse, 
                                                                             part_clinics$factor_plots$south$relapse, 
                                                                             show_legend = FALSE), 
                                                              make_fct_panel(part_clinics$factor_plots$north$hair_loss, 
                                                                             part_clinics$factor_plots$south$hair_loss, 
                                                                             show_legend = FALSE), 
                                                              nrow = 3, 
                                                              labels = LETTERS[-1], 
                                                              label_size = 10) %>% 
    plot_grid(get_legend(part_clinics$factor_plots$north$complete_covelescence), 
              ncol = 2, 
              rel_widths = c(2, 0.3))
  
  paper_figures$long_covid_recovery <- plot_grid(paper_figures$long_covid_recovery$top_panel, 
                                                 paper_figures$long_covid_recovery$bottom_panel, 
                                                 nrow = 2, 
                                                 rel_heights = c(0.4, 0.6), 
                                                 labels = c('A', ''), 
                                                 label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_8_long_covid_recovery', 
                     w = 180, 
                     h = 230)

# Figure S1: symptom presence and symptoms sum trajectories, PASC -----
  
  insert_msg('Figure S1: symptom presence and trajectory in the PASC analysis subset')

  suppl_figures$symptom_sum <- plot_grid(plot_grid(cov_course_pasc$sympt_presence$plot, 
                                                   ggdraw(), 
                                                   ncol = 2, 
                                                   rel_widths = c(0.75, 0.25)), 
                                         sympt_traj$pasc$panel, 
                                         nrow = 2, 
                                         labels = LETTERS, 
                                         label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s1_symptom_sum_trajectory_pasc', 
                     w = 180, 
                     h = 150)
  
# Figure S2: symptom frequencies in the entire cohort -----
  
  insert_msg('Figure S2: symptom frequencies in the cohort')
  
  suppl_figures$frequencies_cohort <- cov_symptfreq$bubble_plots$cohort + 
    theme(legend.position = 'bottom')
  
  suppl_figures$frequencies_cohort <- suppl_figures$frequencies_cohort %>% 
    as_figure_object(figure_label = 'figure_S2_symptom_frequency', 
                     w = 180, 
                     h = 210)
  
# Figure S3: Top 10 symptoms in the symptomatic collective -------
  
  insert_msg('Figure S3: top 10 symptoms per time point in the symptomatic collectives')
  
  suppl_figures$top_10_symptoms <- cov_symptfreq$top_10_plot + 
    theme(legend.position = 'bottom')
  
  suppl_figures$top_10_symptoms <- suppl_figures$top_10_symptoms %>% 
    as_figure_object(figure_label = 'figure_S3_top_symptoms', 
                     w = 180, 
                     h = 120)
  

# Figure S4: symptom co-regulation, clustering diagnostic plots ----
  
  insert_msg('Figure S4: diagnostic of the symptom clustering')

  suppl_figures$symptom_clust_diagnosis <- symptom_clust$diagnostic_plots %>% 
    map(~.x$wss) %>% 
    map2(., c('TY: acute COVID-19', 
              'TY: long COVID', 
              'TY: PASC'), 
         ~.x + labs(title = .y, 
                    tag = stri_extract(.x$labels$tag, regex = 'Variables:.*') %>% 
                      stri_replace(fixed = 'Variables: ', replacement = '\n'))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    plot_grid(plot_grid(symptom_clust$var_plots, 
                        ggdraw(), 
                        ncol = 2, 
                        rel_widths = c(0.7, 0.3)), 
              nrow = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_S4_sympt_clust_diagnostic',
                     w = 180, 
                     h = 170)
  
# Figure S5: phenotypes of acute COVID-19, South Tyrol ------
  
  insert_msg('Figure S5: phenotypes of acute COVID-19, South Tyrol')
  
  suppl_figures$acute_phenotypes <- plot_grid(symptom_clust$heat_maps$south$acute + 
                                                theme(legend.position = 'bottom')) %>% 
    as_figure_object(figure_label = 'figure_S5_acute_phenotypes', 
                     w = 180, 
                     h = 190)
  
# Figure S6: phenotypes of long COVID, South Tyrol -----
  
  insert_msg('Figure S6: phenotypes of long COVID, South Tyrol')
  
  suppl_figures$long_phenotypes <- plot_grid(symptom_clust$heat_maps$south$long + 
                                               theme(legend.position = 'bottom')) %>% 
    as_figure_object(figure_label = 'figure_S6_long_phenotypes', 
                     w = 180, 
                     h = 210)
  
# Figure S7: phenotypes of PASC, Tyrol -----
  
  insert_msg('Figure S7: phenotypes of PASC, Tyrol')
  
  suppl_figures$pasc_phenotypes_north <- plot_grid(symptom_clust$heat_maps$north$chronic + 
                                                     theme(legend.position = 'bottom')) %>% 
    as_figure_object(figure_label = 'figure_S7_pasc_phenotypes_tyrol', 
                     w = 180, 
                     h = 210)
  
# Figure S8: phenotypes of PASC, South Tyrol -----
  
  insert_msg('Figure S8: phenotypes of PASC, South Tyrol')
  
  suppl_figures$pasc_phenotypes_south <- plot_grid(symptom_clust$heat_maps$south$chronic + 
                                                     theme(legend.position = 'bottom')) %>% 
    as_figure_object(figure_label = 'figure_S8_pasc_phenotypes_south_tyrol', 
                     w = 180, 
                     h = 210)
  
  
  
  
# Figure S9: participants, clustering diagnostic plots ----
  
  insert_msg('Figure S9: diagnostic of the participant clustering')
  
  suppl_figures$part_clust_diagnosis <- part_clust$diagnostic_plots %>% 
    map(~.x$knn_dist) %>% 
    map2(., c('TY: long COVID', 
              'TY: PASC'), 
         ~.x + labs(title = .y, 
                    subtitle = 'DBSCAN clustering, Manhattan distance')) %>%
    map2(., part_clust$analyses_north, 
         ~.x + labs(tag = paste('\nn =', nobs(.y)[['observations']]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(plot_grid(part_clust$var_plots, 
                        ggdraw(), 
                        ncol = 2, 
                        rel_widths = c(0.55, 0.45)), 
              nrow = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_S9_part_clust_diagnostic',
                     w = 180, 
                     h = 170)
  
# Figure S10: subsets of participants in PASC -----
  
  insert_msg('Figure S10: subsets in PASC')
  
  suppl_figures$pasc_subsets$top_panel <- plot_grid(part_clust$pca_plots$north$chronic + 
                                                      theme(legend.position = 'none'), 
                                                    part_clust$pca_plots$south$chronic + 
                                                      theme(legend.position = 'none'), 
                                                    get_legend(part_clust$pca_plots$north$chronic), 
                                                    ncol = 3, 
                                                    align = 'hv', 
                                                    axis = 'tblr', 
                                                    rel_widths = c(1, 1, 0.28))
  
  suppl_figures$pasc_subsets$middle_panel <- plot_grid(part_count$summary_plot$plots$north_pasc + 
                                                         theme(legend.position = 'none', 
                                                               plot.tag = element_blank()), 
                                                       part_count$summary_plot$plots$south_pasc + 
                                                         theme(legend.position = 'none', 
                                                               plot.tag = element_blank()), 
                                                       get_legend(part_clust$pca_plots$south$chronic), 
                                                       ncol = 3, 
                                                       align = 'hv', 
                                                       axis = 'tblr', 
                                                       rel_widths = c(1, 1, 0.28))
  
  suppl_figures$pasc_subsets$bottom_panel <- plot_grid(part_sympt$heat_maps$north_pasc + 
                                                         theme(legend.position = 'none', 
                                                               plot.tag = element_blank(), 
                                                               strip.background.y = element_blank(), 
                                                               strip.text.y = element_blank()), 
                                                       part_sympt$heat_maps$south_pasc + 
                                                         theme(legend.position = 'none', 
                                                               plot.tag = element_blank(), 
                                                               axis.text.y = element_blank()), 
                                                       ncol = 2, 
                                                       rel_widths = c(0.53, 0.47), 
                                                       align = 'h') %>% 
    plot_grid(get_legend(part_sympt$heat_maps$north + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  suppl_figures$pasc_subsets <- plot_grid(suppl_figures$pasc_subsets$top_panel,
                                          suppl_figures$pasc_subsets$middle_panel,
                                          suppl_figures$pasc_subsets$bottom_panel, 
                                          nrow = 3, 
                                          rel_heights = c(1, 1, 1.3), 
                                          labels = LETTERS, 
                                          label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_S10_pasc_subsets',
                     w = 180, 
                     h = 230)
  
# Figure S11: symptoms in long COVID and PASC participant subsets ------
  
  insert_msg('Figure S11: symptoms in long COVID and PASC')
  
  suppl_figures$long_pasc_symptoms <- part_sympt$freq_plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(get_legend(part_sympt$freq_plots$north), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>% 
    as_figure_object(figure_label = 'figure_S11_sympt_frequency_long_pasc',
                     w = 180, 
                     h = 220)
  
# Figure S12: the most essential features of clinical background of the PASC participant clusters -------
  
  insert_msg('Figure S12: clinical background of the long COVID subsets')
  
  suppl_figures$pasc_clinics <- plot_grid(make_fct_panel(part_clinics$factor_plots$north_pasc$sex, 
                                                         part_clinics$factor_plots$south_pasc$sex), 
                                          make_fct_panel(part_clinics$factor_plots$north_pasc$bmi_class_before, 
                                                         part_clinics$factor_plots$south_pasc$bmi_class_before), 
                                          make_fct_panel(part_clinics$factor_plots$north_pasc$multi_morbidity, 
                                                         part_clinics$factor_plots$south_pasc$multi_morbidity), 
                                          make_fct_panel(part_clinics$factor_plots$north_pasc$therapy_home_antibiotic, 
                                                         part_clinics$factor_plots$south_pasc$therapy_home_antibiotic, 
                                                         show_tag = TRUE), 
                                          nrow = 4, 
                                          labels = LETTERS, 
                                          label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_S12_pasc_clinics', 
                     w = 180, 
                     h = 220)
  
# Figure S13: recovery features in the PASC participant clusters -----
  
  insert_msg('Figure S13: recovery features in the PASC subsets')
  
  suppl_figures$pasc_recovery$top_panel <- plot_grid(part_clinics$numeric_plot$plots$north_pasc + 
                                                       theme(legend.position = 'none'), 
                                                     part_clinics$numeric_plot$plots$south_pasc + 
                                                       theme(legend.position = 'none'), 
                                                     ncol = 2, 
                                                     align = 'hv', 
                                                     axis = 'tblr') %>% 
    plot_grid(get_legend(part_clust$pca_plots$north$chronic + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15))
  
  suppl_figures$pasc_recovery$bottom_panel <- plot_grid(make_fct_panel(part_clinics$factor_plots$north_pasc$complete_covelescence, 
                                                                       part_clinics$factor_plots$south_pasc$complete_covelescence, 
                                                                       show_legend = FALSE), 
                                                        make_fct_panel(part_clinics$factor_plots$north_pasc$relapse, 
                                                                       part_clinics$factor_plots$south_pasc$relapse, 
                                                                       show_legend = FALSE), 
                                                        make_fct_panel(part_clinics$factor_plots$north_pasc$hair_loss, 
                                                                       part_clinics$factor_plots$south_pasc$hair_loss, 
                                                                       show_legend = FALSE), 
                                                        nrow = 3, 
                                                        labels = LETTERS[-1], 
                                                        label_size = 10) %>% 
    plot_grid(get_legend(part_clinics$factor_plots$north_pasc$complete_covelescence), 
              ncol = 2, 
              rel_widths = c(2, 0.3))
  
  suppl_figures$pasc_recovery <- plot_grid(suppl_figures$pasc_recovery$top_panel, 
                                           suppl_figures$pasc_recovery$bottom_panel, 
                                           nrow = 2, 
                                           rel_heights = c(0.4, 0.6), 
                                           labels = c('A', ''), 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_S13_pasc_recovery', 
                     w = 180, 
                     h = 230)
  
  
  
  
# Figure S14: univariate modeling -----
  
  insert_msg('Figure S14: univariate modeling results')
  
  suppl_figures$univariate_risk <- risk_modeling$forest_plots %>% 
    map(~.x$plot) %>% 
    map(~.x + theme(plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              align = 'hv', 
              rel_heights = c(0.8, 1, 1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s14_univariate_modeling', 
                     w = 180, 
                     h = 230)
  
# Figure S15: LASSO sum acute symptoms -----
  
  insert_msg('Figure S15: LASSO acute COVID-19 symptom sum')
  
  suppl_figures$lasso_acute <- plot_grid(plot_grid(lasso$forest_plot_north$sum_symptoms_acute + 
                                                     expand_limits(x = 0.9), 
                                                   ggdraw(), 
                                                   ncol = 2, 
                                                   rel_widths = c(0.6, 0.4)), 
                                         plot_grid(lasso$fit_plots$north, 
                                                   lasso$fit_plots$south, 
                                                   ncol = 2, 
                                                   align = 'hv'), 
                                         nrow = 2, 
                                         rel_heights = c(1.2, 0.8), 
                                         labels = LETTERS, 
                                         label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s15_lasso_acute', 
                     w = 180, 
                     h = 190)
  
# Figure S16: LASSO long COVID and PASC risk -----
  
  insert_msg('Figure S16: LASSO long COVID and PASC risk')
  
  suppl_figures$lasso_long <- plot_grid(plot_grid(lasso$forest_plot_north$long_covid + 
                                                    expand_limits(x = 2), 
                                                  lasso$forest_plot_north$pasc + 
                                                    expand_limits(x = 1.6), 
                                                  ncol = 2, 
                                                  align = 'hv'), 
                                        plot_grid(lasso$roc_plots$long_covid + 
                                                    theme(legend.position = 'none'), 
                                                  lasso$roc_plots$pasc + 
                                                    theme(legend.position = 'none'), 
                                                  get_legend(lasso$roc_plots$long_covid), 
                                                  ncol = 3,
                                                  rel_widths = c(1, 1, 0.2), 
                                                  align = 'hv', 
                                                  axis = 'tblr'), 
                                        nrow = 2, 
                                        rel_heights = c(1, 0.85), 
                                        labels = LETTERS, 
                                        label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s16_lasso_long_pasc', 
                     w = 180, 
                     h = 230)
  
# Saving the figures on the disc ----
  
  insert_msg('Saving the figures')
  
  paper_figures %>% 
    walk(save_figure_object, 
         target_folder = './paper/figures', 
         device = cairo_pdf)
  
  suppl_figures %>% 
    walk(save_figure_object, 
         target_folder = './paper/supplementary figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()