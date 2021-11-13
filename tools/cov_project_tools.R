# This script provides tools for the COV-Questionaire project


# libraries and data ----

   c('./tools/sys_tools.R', 
     './tools/plotting_tools.R', 
     './tools/counting_tools.R', 
     './tools/roc_toolbox.R', 
     './tools/clust_tools2.R', 
     './tools/lm_qc_tools.R', 
     './tools/lasso_selection_tools.R') %>% 
      walk(source)

   library(meta)
   library(UpSetR)
   library(cowplot)
   library(ggvenn)

# feature frequency calculation and plotting ----

   analyze_symptoms <- function(analysis_tbl_lst) {
      
      ## makes complete counting and analysis of the symptom prevalence change in time 
      ## by Chi sQ test for trend
      
      ## analysis objects
      
      analysis_obj <- analysis_tbl_lst %>% 
         map(function(x) map(globals$symptoms, 
                             analyze_feature, 
                             inp_tbl = x, 
                             split_var = 'timepoint')) %>% 
         map(set_names, 
             globals$symptoms)
      
      ## count table
      
      count_tbl <- analysis_obj %>% 
         map(function(x) map_dfr(x, extract_counts)) %>% 
         map(filter, 
             strata == 'yes') %>% 
         map2_dfr(., names(.), 
                  function(x, y) mutate(x, 
                                        cohort = factor(y, c('north', 'south')), 
                                        symptom_label = translate_var(variable)))
      
      ## significance and multiple testing adjustment
      
      signif_tbl <- analysis_obj %>% 
         map(function(x) map_dfr(x, extract_test_summary)) %>% 
         map2_dfr(., names(.), 
                  function(x, y) mutate(x, 
                                        cohort = factor(y, c('north', 'south')), 
                                        p_fdr = p.adjust(p_value, 'BH'), 
                                        significance = ifelse(p_fdr < 0.05, 'yes', 'no'))) %>% 
         select( - split_var)
      
      ## plotting order by the rising prevalence in the acute timepoint in the North cohort
      
      plotting_order <- count_tbl %>%
         filter(cohort == 'north', 
                split_var == 'acute') %>% 
         arrange(percent) %>% 
         mutate(plot_order = 1:nrow(.)) %>% 
         select(variable, 
                plot_order)
      
      ## a summary table for plotting containing the counts and ChiSq testing data
      ## variable plot_lab containing the rounded percent values to be presented in the plots
      
      plotting_tbl <- left_join(count_tbl, 
                                signif_tbl, 
                                by = c('variable', 'cohort')) %>% 
         left_join(., 
                   plotting_order, 
                   by = 'variable') %>% 
         mutate(plot_lab = signif(percent, 2))
      
      return(plotting_tbl)
      
   }

   plot_bubble <- function(inp_tbl, x_var, y_var, order_var = 'plot_order', 
                           fill_size_var = 'percent', label_var = NULL, 
                           plot_title = NULL, plot_subtitle = NULL, 
                           plot_tag = NULL, x_lab = NULL, 
                           legend_name = '% complete\nanswers') {
      
      ## a bubble plot with percents of the features
      
      bubble <- inp_tbl %>% 
         ggplot(aes(x = .data[[x_var]], 
                    y = reorder(.data[[y_var]], 
                                .data[[order_var]]), 
                    size = .data[[fill_size_var]], 
                    fill = .data[[fill_size_var]])) + 
         geom_point(color = 'black', 
                    shape = 21)
      
      if(!is.null(label_var)) {
         
         bubble <- bubble + 
            geom_text(aes(label = .data[[label_var]]), 
                      size = 2.6, 
                      hjust = 0, 
                      vjust = 0.3, 
                      nudge_x = 0.08)
         
      }
      
      bubble <- bubble +
         scale_size_area(breaks = seq(0, 100, by = 20), 
                         max_size = 4, 
                         name = legend_name) + 
         scale_fill_gradient2(high = 'firebrick4', 
                              low = 'steelblue4', 
                              mid = 'white', 
                              midpoint = 50, 
                              name = legend_name, 
                              breaks = seq(0, 100, by = 20)) + 
         globals$common_theme + 
         theme(axis.title.y = element_blank(), 
               panel.grid.major = element_line(color = 'gray90', 
                                               size = 0.25)) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              x = x_lab, 
              tag = plot_tag) + 
         guides(size = guide_legend(), 
                fill = guide_legend())
      
      return(bubble)
      
   }

# trajectory modeling and plotting functions for the number of symptoms -----
   
   model_trajectory <- function(inp_tbl, 
                                response = 'symptom_sum', 
                                fixed_vars = 'timepoint', ...) {
      
      ## models symptom count recovery with mixed-effect Poisson regression
      
      ## model formula
      
      mod_formula <- paste(response, '~', 
                           paste(fixed_vars, collapse = '*'), 
                           '+ (1|ID)') %>% 
         as.formula()
      
      ## model
      
      model <- glmer(formula = mod_formula, 
                     family = 'poisson', 
                     data = inp_tbl, ...)
      
      ## model summary
      
      mod_summary <- model %>% 
         get_glm_results(exponentiate = TRUE) %>% 
         mutate(plot_label = stri_replace(plot_label, regex = ',.*', replacement = ''))
      
      ## symptom count median and IQR
      
      count_stats <- inp_tbl %>% 
         ddply(fixed_vars, 
               variable_stats, variable = 'symptom_sum')
      
      ## output
      
      return(list(model = model, 
                  summary = mod_summary, 
                  stat_tbl = count_stats))
      
      
   }

   plot_trajectory <- function(inp_tbl, x_var, y_var, stat_tbl, split_var = NULL, show_lines = T, x_lab = x_var, 
                               y_lab = y_var, plot_title = NULL, plot_subtitle = NULL, plot_tag = NULL, 
                               line_color = 'gray80', ribbon_color = 'coral3', median_color = 'coral4') {
      
      ## presents the distribution of the given y_var in the strata defined by y_var
      ## as a violin plot. Stat tbl provides quantile and median data to plot and label
      
      
      
      if(is.null(split_var)) {
         
         trajectory <- inp_tbl %>% 
            ggplot(aes(x = .data[[x_var]], 
                       y = .data[[y_var]]))
         
         if(show_lines) {
            
            trajectory <- trajectory + 
               geom_line(aes(group = ID), 
                         alpha = 0.1, 
                         size = 0.1, 
                         color = line_color)
            
         }
         
         trajectory <- trajectory + 
            geom_ribbon(data = stat_tbl, 
                        aes(y = median, 
                            ymin = perc25, 
                            ymax = perc75, 
                            group = factor(n_total)), 
                        alpha = 0.35, 
                        fill = ribbon_color, 
                        color = ribbon_color) + 
            geom_line(data = stat_tbl, 
                      aes(y = median, 
                          group = factor(n_total)), 
                      color = median_color, 
                      size = 1.5) + 
            globals$common_theme + 
            labs(title = plot_title, 
                 subtitle = plot_subtitle, 
                 y = y_lab, 
                 x = x_lab, 
                 tag = plot_tag)
         
      } else {
         
         trajectory <- inp_tbl %>% 
            ggplot(aes(x = .data[[x_var]], 
                       y = .data[[y_var]], 
                       fill = .data[[split_var]], 
                       color = .data[[split_var]])) +  
            geom_ribbon(data = stat_tbl, 
                        aes(y = median, 
                            ymin = perc25, 
                            ymax = perc75, 
                            group = factor(n_total)), 
                        alpha = 0.35) + 
            geom_line(data = stat_tbl, 
                      aes(y = median, 
                          group = factor(n_total)), 
                      size = 1.5)
         
      }
      
      trajectory <- trajectory  + 
         globals$common_theme + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              y = y_lab, 
              x = x_lab, 
              tag = plot_tag)
      
      return(trajectory)
      
   }

# clustering ------
   
   clust_analysis_tbl <- function(inp_tbl, 
                                  symptom_vec = globals$symptoms, 
                                  sympt_suffix = 'acute') {
      
      ## prepares an analysis table containing the symptom variables
      ## and complete cases only. the symptoms are re-coded to 2 for yes and 1 for no
      ## for compatibility with the simple matching distance calculation
      ## The output is a data frame with rows named by the participants' ID
      
      analysis_tbl <- inp_tbl %>% 
         select(ID, 
                all_of(paste(symptom_vec, sympt_suffix, sep = '_'))) %>% 
         set_names(c('ID', 
                     symptom_vec)) %>% 
         complete_cases
      
      ID_vec <- analysis_tbl$ID
      
      analysis_tbl <- analysis_tbl %>% 
         select( - ID) %>% 
         map_dfc(~as.numeric(.x) - 1) %>% 
         set_rownames(ID_vec)
      
      return(analysis_tbl)
      
   }
   
   sum_symptoms <- function(inp_tbl, vars, id_var = 'ID') {
      
      ## calulates sum of the yes/no recoded variables, e.g. symptoms
      
      inp_tbl[vars] %>% 
         map(~as.numeric(.x) - 1) %>% 
         reduce(`+`)
      
   }
   
   compare_clust_var <- function(clust_var_tbl, 
                                 plot_title = NULL, 
                                 plot_subtitle = NULL, 
                                 plot_tag = NULL) {
      
      ## displays fraction of the data set variance 'explained' by clustering
      
      var_bar <- clust_var_tbl %>% 
         mutate(cohort = stri_replace(cohort, fixed = 'analyses_', replacement = '')) %>% 
         ggplot(aes(x = timepoint, 
                    y = frac_var, 
                    fill = cohort)) + 
         geom_bar(stat = 'identity', 
                  color = 'black', 
                  position = position_dodge(width = 0.9)) + 
         geom_text(aes(label = signif(frac_var, 3)), 
                   size = 2.75, 
                   hjust = 0.5, 
                   vjust = -0.8, 
                   position = position_dodge(width = 0.9)) + 
         scale_fill_manual(values = globals$cohort_colors, 
                           labels = globals$cohort_labs, 
                           name = '') + 
         scale_x_discrete(labels = globals$time_labs) + 
         expand_limits(y = 1) + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90'), 
               axis.title.x = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle,
              tag = plot_tag, 
              y = 'between SS/total SS')
      
      return(var_bar)
      
   }
   
# participant cluster characteristic -----
   
   plot_violin_summary <- function(inp_long_tbl, 
                                   normalize = TRUE, 
                                   plot_title = NULL, 
                                   plot_subtitle = NULL, 
                                   plot_tag = NULL, 
                                   x_lab = 'Symptom phenotype', 
                                   y_lab = 'Min/max normalized symp. count', 
                                   fill_lab = 'Participant\nsubset', 
                                   jitter_width = 0.25, 
                                   jitter_height = 0.01, 
                                   box = FALSE) {
      
      ## makes a summary violin plot with the symptom counts
      
      inp_long_tbl <- inp_long_tbl %>% 
         filter(clust_id != 'noise')
      
      if(normalize) {
         
         inp_long_tbl <- inp_long_tbl %>% 
            group_by(phenotype) %>% 
            mutate(count = min_max(count)) %>% 
            ungroup
         
      }

      vio_plot <- inp_long_tbl %>% 
         ggplot(aes(y = count, 
                    x = phenotype, 
                    fill = clust_id))
      
      if(box) {
         
         vio_plot <- vio_plot + 
            geom_boxplot(alpha = 0.25, 
                         position = position_dodge(width = 0.9), 
                         show.legend = FALSE, 
                         outlier.color = NA)
         
      } else {
         
         vio_plot <- vio_plot + 
            geom_violin(alpha = 0.25, 
                        position = position_dodge(width = 0.9), 
                        show.legend = FALSE)
         
      }

      vio_plot <- vio_plot + 
         geom_point(shape = 21, 
                    size = 1, 
                    alpha = 0.5, 
                    position = position_jitterdodge(jitter.width = jitter_width, 
                                                    jitter.height = jitter_height, 
                                                    dodge.width = 0.9)) + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = x_lab, 
              y = y_lab, 
              fill = fill_lab)
      
      return(vio_plot)
      
   }
   
   add_cluster_p <- function(summary_plot, 
                             p_txt) {
      
      out_plot <- summary_plot
      
      for(i in 1:length(p_txt)) {
         
         out_plot <- out_plot + 
            annotate('segment',
                     x = i - 0.25, 
                     xend = i + 0.25, 
                     y = 1.04, 
                     yend = 1.04) + 
            annotate('text', 
                     label = p_txt[i], 
                     x = i, 
                     y = 1.05, 
                     hjust = 0.5, 
                     vjust = 0, 
                     size = 2.75)
         
      }
      
      out_plot <- out_plot  + 
         scale_y_continuous(breaks = seq(0, 1, by = 0.25))
      
      return(out_plot)
   }
   
   plot_sympt_hm <- function(inp_tbl, 
                             vars, 
                             sympt_clustering = symptom_clust$analyses_north$long, 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             plot_tag = NULL) {
      
      ## draws a heat map with the selected symptoms for the participants
      
      ## plotting table
      
      plotting_tbl <- inp_tbl %>% 
         select(ID, observation, clust_id, all_of(vars)) %>% 
         gather(key = 'symptom', 
                value = 'present', 
                all_of(vars)) %>% 
         mutate(present = factor(present), 
                clust_id = factor(clust_id, c('HAP-', 'HAPi', 'HAP+'))) %>% 
         left_join(extract(sympt_clustering, 'assignment') %>% 
                      mutate(symptom = observation), by = 'symptom')
      
      ## heat map
      
      hm <- plotting_tbl %>% 
         ggplot(aes(x = reorder(ID, as.numeric(present)), 
                    y = symptom, 
                    fill = present)) + 
         geom_tile() + 
         scale_fill_manual(values = c(no = 'steelblue', 
                                      yes = 'indianred3'), 
                           labels = c(no = 'absent', 
                                      yes = 'present'), 
                           name = '') + 
         facet_grid(clust_id.y ~ clust_id.x, 
                    scales = 'free', 
                    space = 'free') + 
         globals$common_theme + 
         theme(axis.line = element_blank(), 
               axis.ticks.x = element_blank(), 
               axis.text.x = element_blank(), 
               axis.title.y = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = 'participant')
      
      return(hm)
      
   }
   
   plot_sympt_freq <- function(freq_tbl, 
                               testing_summary, 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL, 
                               x_lab = '% participant cluster') {
      
      ## makes a bar plot with the symptom frequencies in the participant clusters
      
      testing_summary <- testing_summary %>% 
         mutate(p_lab = ifelse(p_adj >= 0.05, 
                               paste0('ns (p = ', signif(p_adj, 2), ')'), 
                               paste('p =', signif(p_adj, 2))))
      
      p_x <- freq_tbl %>% 
         group_by(variable) %>% 
         summarise(x_pos = max(percent, na.rm = T) + 1)
      
      testing_summary <- left_join(testing_summary, 
                                   p_x, 
                                   by = 'variable')
      
      ## plot
      
      freq_plot <- freq_tbl %>% 
         ggplot(aes(x = percent, 
                    y = reorder(variable, percent))) + 
         geom_bar(aes(fill = split_var), 
                  stat = 'identity', 
                  color = 'black', 
                  position = position_dodge(width = 0.9)) + 
         geom_text(data = testing_summary, 
                   aes(label = p_lab, 
                       x = x_pos, 
                       y = variable), 
                   size = 2.75, 
                   hjust = 0) + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90'), 
               axis.title.y = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = x_lab)
      
      return(freq_plot)
      
   }
   
   make_fct_panel <- function(plot_north, 
                              plot_south, 
                              show_tag = FALSE, 
                              show_legend = TRUE) {
      
      ## makes a panel with two factor plots
      
      if(show_tag) {
         
         panel <- plot_grid(plot_north + 
                               labs(title = paste('TY:', tolower(plot_north$labels$title))) + 
                               theme(legend.position = 'none'), 
                            plot_south + 
                               labs(title = paste('STY:', tolower(plot_south$labels$title))) + 
                               theme(legend.position = 'none'), 
                            ncol = 2, 
                            align = 'hv', 
                            axis = 'tblr')
         
      } else {
         
         panel <- plot_grid(plot_north + 
                               labs(title = paste('TY:', tolower(plot_north$labels$title))) + 
                               theme(legend.position = 'none', 
                                     plot.tag = element_blank()), 
                            plot_south + 
                               labs(title = paste('STY:', tolower(plot_south$labels$title))) + 
                               theme(legend.position = 'none', 
                                     plot.tag = element_blank()), 
                            ncol = 2, 
                            align = 'hv', 
                            axis = 'tblr')
         
      }
      
      if(show_legend) {
         
         panel <- plot_grid(panel, 
                            get_legend(plot_north), 
                            ncol = 2, 
                            rel_widths = c(2, 0.3))
         
      }
      
      return(panel)
      
   } 
   
# Forest and Venn plotting of modeling results ----
   
   plot_forest_cohorts <- function(modeling_summary, ref_cohort = 'north', n_top = 20, cut_estimate = 1, 
                                   plot_title = NULL, plot_subtitle = NULL, plot_tag = NULL, 
                                   x_lab = expression('exp '*beta), tag_position = 'bottom', 
                                   x_transf = 'identity', show_intercept = TRUE, filter_ns = TRUE) {
      
      ## plots a forest plot of top positive and negative estimates split by the cohort
      ## The top estimates are identified in the reference cohort
      # if filter_ns is true, only the estimates significant in both cohorts are presented
      
      ## plotting table

      plotting_tbl <- modeling_summary %>% 
         filter(level != 'baseline', 
                p_adj < 0.05)
      
      ## identifying the top estimates
      
      top_estimates <- plotting_tbl %>% 
         filter(cohort == ref_cohort) %>% 
         dlply(.(estimate > cut_estimate), 
               top_n, 
               n = n_top, 
               estimate) %>% 
         map_dfr(arrange, estimate) %>% 
         mutate(plot_order = 1:nrow(.)) %>% 
         select(parameter, 
                plot_order)
      
      ## filtering the plotting table, filtering the estimates significant
      ## only in one cohort, appending with the plot label
      
      plotting_tbl <- plotting_tbl %>% 
         filter(parameter %in% top_estimates$parameter) %>% 
         left_join(top_estimates, 
                   by = 'parameter')
      
      if(filter_ns) {
         
         plotting_tbl <- plotting_tbl %>%  
            ddply(.(parameter), 
                  function(x) if(nrow(x) > 1) x else NULL)
         
      }
      
      plotting_tbl <- plotting_tbl %>% 
         mutate(level = ifelse(is.na(level), '', level), 
                plot_label = translate_var(variable, out_value = 'label_short'), 
                plot_label = ifelse(variable == 'Intercept', 
                                    'Intercept', 
                                    ifelse(level == 'yes', plot_label, 
                                           paste(plot_label, level, sep = ': '))), 
                paper_txt = paste(signif(estimate, 3), 
                                  ' [95%CI: ', 
                                  signif(lower_ci, 3), 
                                  ' - ', 
                                  signif(upper_ci, 3), 
                                  ']', sep = '')) %>% 
         as_tibble
      
      if(!show_intercept) {
         
         plotting_tbl <- plotting_tbl %>% 
            filter(plot_label != 'Intercept')
         
      }
      
      ## plotting

      if(is.null(plot_tag)) {
         
         n_numbers <- plotting_tbl %>% 
            dlply(.(cohort), function(x) range(x$n_complete)) %>% 
            map(function(x) if(diff(x) == 0) x[1] else x)
         
         plot_tag <- paste('\nNorth: n = ', 
                           paste(n_numbers$north, collapse = ' - '), 
                           ', South: n = ', 
                           paste(n_numbers$south, collapse = ' - '), sep = '')
         
      }
      
      forest_plot <- plotting_tbl %>% 
         ggplot(aes(x = estimate, 
                    y = reorder(plot_label, plot_order), 
                    color = cohort)) + 
         geom_errorbarh(aes(xmin = lower_ci,
                            xmax = upper_ci), 
                        height = 0.02) +
         geom_point(shape = 16, 
                    size = 2) + 
         geom_vline(xintercept = cut_estimate, 
                    linetype = 'dashed') + 
         scale_color_manual(values = globals$cohort_colors, 
                            labels = globals$cohort_labs, 
                            name = 'Cohort') + 
         scale_x_continuous(trans = x_transf) + 
         guides(color = F) + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90'), 
               axis.title.y = element_blank(), 
               strip.background = element_rect(fill = 'gray95'), 
               plot.tag.position = tag_position) + 
         facet_grid(. ~ cohort, 
                    labeller = as_labeller(globals$cohort_labs)) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              x = x_lab, 
              tag = plot_tag)
      
      return(list(plot = forest_plot, 
                  plot_data = plotting_tbl))
      
   }

   plot_venn <- function(signif_fct_north, signif_fct_south, 
                         plot_title = NULL, plot_subtitle = NULL) {
      
      ## generates a Venn plot to show an overlap between the symptoms assigned to the same 
      ## cluster in both cohorts
      
      ## plot
      
      plotting_lst <- list(TY = signif_fct_north, 
                           STY = signif_fct_south)
      
      venn_plot <- plotting_lst %>% 
         ggvenn(show_percentage = FALSE, 
                fill_color = unname(globals$cohort_colors), 
                set_name_size = 2.75, 
                text_size = 2.75) + 
         theme(plot.title = element_text(size = 10, face = 'bold'), 
               plot.subtitle = globals$common_text) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle)
      
      ## symptom listing
      
      sympt_txt <- plotting_lst %>% 
         reduce(intersect) %>% 
         translate_var(out_value = 'label_short') %>% 
         wrap_vector(line_length = 4)
      
      ## plot panel
      
      venn_panel <- plot_grid(venn_plot, 
                              ggdraw() + 
                                 draw_text(sympt_txt, 
                                           hjust = 0, 
                                           x = 0.05, 
                                           size = 7) + 
                                 theme(plot.margin = margin(t = 0, b = 0, r = 2, l = 3)), 
                              ncol = 2, 
                              rel_widths = c(0.4, 0.6)) + 
         theme(plot.margin = globals$common_margin)
      
      return(venn_panel)
      
   }
   
   plot_forest_lasso <- function(lasso_summary_tbl, 
                                 est_trans = 'exp', 
                                 show_intercept = FALSE, 
                                 plot_title = NULL, 
                                 plot_subtitle = NULL, 
                                 plot_tag = NULL, 
                                 x_lab = 'OR', 
                                 x_trans = 'log2', 
                                 cutpoint = 1) {
      
      ## represents non-zero LASSO estimates in a plot
      
      lasso_summary_tbl <- lasso_summary_tbl %>% 
         mutate(plot_lab = ifelse(coefficient_name != 'Intercept', 
                                  translate_var(covariate, out_value = 'label_short'), 
                                  'Intercept'), 
                plot_lab = ifelse(level == 'yes' | coefficient_name == 'Intercept', 
                                  plot_lab, 
                                  paste(plot_lab, level, sep = ': ')), 
                plot_lab = stri_replace(plot_lab, fixed = '...', replacement = ' - '), 
                new_est = eval(call2(est_trans, estimate)))
      
      if(!show_intercept) {
         
         lasso_summary_tbl <- lasso_summary_tbl %>% 
            filter(coefficient_name != 'Intercept')
         
      }
      
      forest <- lasso_summary_tbl %>% 
         ggplot(aes(x = new_est, 
                    y = reorder(plot_lab, new_est),
                    fill = estimate)) + 
         geom_vline(xintercept = cutpoint, 
                    linetype = 'dashed') + 
         geom_segment(aes(x = cutpoint, 
                          y = reorder(plot_lab, new_est), 
                          xend = new_est, 
                          yend = reorder(plot_lab, new_est)), 
                      color = 'gray50') + 
         geom_point(aes(size = abs(estimate)), 
                    shape = 21) + 
         geom_text(aes(label = signif(new_est, 3)), 
                   size = 2.75, 
                   hjust = 0.5, 
                   vjust = -1.7) + 
         guides(size = F) + 
         globals$common_theme + 
         theme(axis.title.y = element_blank(), 
               panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = plot_title,
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = x_lab)
      
      return(forest)
      
   }

# Formatting of the modeling result tables -----
   
   common_summary_tbl <- function(fct_vector, modeling_summary_north, modeling_summary_south) {
      
      ## extracts common significant factors and their stats from the modeling summary tables
      
      cmm_tbl <- rbind(modeling_summary_north %>% 
                          mutate(cohort = 'north'), 
                       modeling_summary_south %>% 
                          mutate(cohort = 'south')) %>% 
         filter(variable %in% fct_vector)
      
      return(cmm_tbl)
      
   }
   
   format_res_tbl <- function(modeling_summary, p_val_var = 'p_adj') {
      
      ## nice format modeling result table
      
      nice_tbl <- modeling_summary %>% 
         left_join(globals$var_lexicon %>% 
                      mutate(response = variable) %>% 
                      select(response, family), 
                   by = 'response') %>% 
         mutate(method = car::recode(family,
                                     "'quasipoisson' = 'GLM Poisson'; 
                                'quasibinomial' = 'logistic regression'")) %>% 
         mutate(cohort = car::recode(cohort, 
                                     "'south' = 'South Tyrol'; 
                                'north' = 'North Tyrol'"), 
                response = translate_var(response, out_value = 'label_short'), 
                variable = translate_var(variable, out_value = 'label_short'), 
                variable = paste(variable, level, sep = ': '), 
                significance = ifelse(.data[[p_val_var]] < 0.05, 
                                      paste('p =', 
                                            signif(.data[[p_val_var]], 2)), 
                                      paste0('ns (p = ', signif(.data[[p_val_var]], 2), ')')), 
                estimate = paste0(signif(estimate, 3), 
                                  ' [', signif(lower_ci, 3), 
                                  ' - ', signif(upper_ci, 3), ']')) %>% 
         select(cohort, 
                response, 
                variable, 
                method, 
                n_complete, 
                estimate, 
                significance) %>% 
         set_names(c('Cohort', 
                     'Response', 
                     'Co-variate', 
                     'Method', 
                     'N complete observations', 
                     'Exp. estimate', 
                     'Significance'))

      return(nice_tbl)

   }
   
   aggregate_modeling <- function(modeling_summary, return_models = F, ...) {
      
      ## aggregates the results of univariate modeling in the North and South cohort
      ## The inverse variance method is used for pooling
      ## p value adjustment by Benjamini-Hochberg
      
      pooled_models <- modeling_summary %>% 
         filter(level != 'baseline') %>% 
         dlply(.(parameter), 
               function(x) metagen(TE = estimate, 
                                   seTE = se, 
                                   studlab = cohort, 
                                   data = x))
      
      if(return_models) {
         
         return(pooled_models)
         
      }
      
      pooled_summary <- pooled_models %>% 
         map(summary) %>% 
         map2_dfr(., 
                  names(.), 
                  function(x, y) tibble(parameter = y, 
                                        estimate = x$fixed$TE, 
                                        se = x$fixed$seTE, 
                                        lower_ci = x$fixed$lower, 
                                        upper_ci = x$fixed$upper, 
                                        p_value = x$fixed$p, 
                                        Q = x$Q, 
                                        tau = x$tau$TE, 
                                        tau_sq = x$tau2$TE, 
                                        I_sq = x$I2$TE, 
                                        H = x$H$TE)) %>% 
         left_join(modeling_summary[, c('parameter', 
                                        'variable', 
                                        'response', 
                                        'level', 
                                        'cohort')] %>% 
                      filter(cohort == 'north'), 
                   by = 'parameter') %>% 
         mutate(cohort = 'pooled', 
                p_adj = p.adjust(p_value, 'BH')) %>% 
         select(response, 
                variable, 
                level, 
                parameter, 
                estimate, 
                se, 
                lower_ci, 
                upper_ci, 
                p_value, 
                p_adj, 
                cohort)
      
      return(pooled_summary)
      
   }

# modeling accessory functions -----

   identify_significant <- function(modeling_summary_tbl, confounder = 'obs_time') {
      
      ## identifies significant factors: at least one non-intercept estimate with 
      ## p_adj < 0.05
      
      signif_fcts <- modeling_summary_tbl %>% 
         filter(variable != 'confounder', 
                variable != 'Intercept', 
                level != 'baseline') %>% 
         dlply(.(variable), function(x) if(any(x$p_adj < 0.05)) x$variable[1] else NULL) %>% 
         compact %>% 
         as.character
      
      return(signif_fcts)
      
   }
   
   identify_non_zero <- function(modeling_summary_tbl, confounder = 'obs_time') {
      
      ## identifies significant factors: at least one non-intercept estimate with 
      ## p_boot < 0.05
      
      signif_fcts <- modeling_summary_tbl %>% 
         filter(variable != 'confounder', 
                variable != 'Intercept', 
                level != 'baseline') %>% 
         dlply(.(variable), function(x) if(any(x$estimate != 0)) x$variable[1] else NULL) %>% 
         compact %>% 
         as.character
      
      return(signif_fcts)
      
   }
   
# Text functions ----
   
   split_vec <- function(inp_vector, chunk_size) {
      
      return(split(inp_vector, ceiling(seq_along(inp_vector)/chunk_size)))
      
   }
   
   wrap_vector <- function(txt_vec, line_length = 5) {
      
      split_txt <- split_vec(txt_vec, 
                             line_length) %>% 
         map(paste, 
             collapse = ', ') %>%
         paste(collapse = ',\n')
      
      return(split_txt)
      
   }
   
   get_beta <- function(modeling_summary, variable_name, level_name, cohort_name = 'pooled', signif_digits = 3) {
      
      ## extracts the string with beta and its CI for the paper text
      
      beta_txt <- modeling_summary %>% 
         filter(variable == variable_name, 
                level == level_name, 
                cohort == cohort_name)
      
      beta_txt <- paste(signif(beta_txt$estimate, signif_digits), 
                        ' [95%CI: ', 
                        signif(beta_txt$lower_ci, signif_digits), 
                        ' - ', 
                        signif(beta_txt$upper_ci, signif_digits), 
                        ']', sep = '')
      
      return(beta_txt)
      
      
   }
   
   
# LATEX formatting -----
   
   format_line <- function(inp_tbl) {
      
      ## formats the given table to include percent marks and new lines
      
      require(kableExtra)
      
      new_tbl <- inp_tbl %>% 
         map_dfc(stri_replace_all, 
                 fixed = '%', 
                 replacement = '\\%') %>% 
         map_dfc(linebreak)
      
      return(new_tbl)
      
   }
   
# ROC -----
   
   get_score_formula <- function(modeling_summary) {
      
      ## gets a string with the linear predictor score formula
      
      score_formula <-  modeling_summary %>% 
         filter(estimate != 0) %>% 
         mutate(coeff_label = ifelse(variable != 'Intercept', 
                                     paste(translate_var(variable, short = T), 
                                           level, sep = ': '), 
                                     'Intercept'), 
                coeff_label = paste('(', 
                                    coeff_label, 
                                    ')', sep = ''), 
                coeff_effect = paste(signif(estimate, 3), 
                                     coeff_label, 
                                     sep = ' \u00D7 '))
      
      score_formula <- score_formula$coeff_effect %>% 
         paste(collapse = ' + ') %>% 
         paste('LPS =', .)
      
      return(score_formula)
      
   }

# varia -----
   
   mm_inch <- function(input_mm) {
      
      return(0.0393700787 * input_mm)
      
   }
   
   set_rownames <- function(inp_tbl, new_rownames) {
      
      ## sets custom rownames
      
      out_tbl <- inp_tbl %>% 
         as.data.frame
      
      rownames(out_tbl) <- new_rownames

      return(out_tbl)      
      
   }
   
   strat_quartile <- function(inp_tbl, 
                              numeric_variable, 
                              new_var_name = 'strat_var', 
                              id_index = 'ID', 
                              labels = NULL) {
      
      ## stratifies the given variable by its quartiles
      
      if(length(numeric_variable) > 1) {
         
         out_tbl <- numeric_variable %>% 
            map(strat_quartile, 
                inp_tbl = inp_tbl, 
                new_var_name = 'strat_var', 
                id_index = id_index, 
                labels = labels) %>% 
            reduce(left_join, 
                   by = id_index)
         
         if(length(numeric_variable) == length(new_var_name)) {
            
            out_tbl <- out_tbl %>% 
               set_names(c(id_index, 
                           new_var_name))
            
         }
         
         return(out_tbl)
         
      }
      
      cutoffs <- inp_tbl[[numeric_variable]] %>% 
         quantile(c(0.25, 0.5, 0.75), na.rm = T)
      
      cutoffs <- c(-Inf, cutoffs, Inf)
      
      out_tbl <- inp_tbl %>% 
         mutate(strat_var = cut(.data[[numeric_variable]], 
                                cutoffs, 
                                labels)) %>% 
         select(all_of(c(id_index, 
                         'strat_var'))) %>% 
         set_names(c(id_index, 
                     new_var_name))
      
      return(out_tbl)
      
   }
   
   min_max <- function(numeric_vector) {
      
      (numeric_vector - min(numeric_vector, na.rm = TRUE))/(max(numeric_vector, na.rm = TRUE) - min(numeric_vector, na.rm = TRUE))
      
   }

# END ----