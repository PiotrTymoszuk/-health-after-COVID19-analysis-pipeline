# Elderly in the study and the convalescent population -----

  elderly <- list()
  
  elderly$north$population <- model_wrangling$weights$north %>% 
    mutate(elderly = ifelse(age_strata %in% c('65-74', '75-84', '>84'), 'yes', 'no')) %>% 
    group_by(elderly) %>% 
    summarise(n = sum(number_convalescents)) %>% 
    mutate(percent = n/sum(n) * 100)
  
  elderly$north$study <- cov_data$north %>% 
    mutate(elderly = ifelse(age > 65, 'yes', 'no')) %>% 
    filter(!is.na(elderly)) %>% 
    count(elderly) %>% 
    mutate(percent = n/sum(n) * 100)
  
  elderly$north$test <- elderly$north %>% 
    map(~.x$n) %>% 
    reduce(cbind) %>% 
    chisq.test
  
  elderly$south$population <- model_wrangling$weights$south %>% 
    mutate(elderly = ifelse(age_strata %in% c('60-69', '70-79', '80-89', 'Over 90'), 'yes', 'no')) %>% 
    group_by(elderly) %>% 
    summarise(n = sum(number_convalescents)) %>% 
    mutate(percent = n/sum(n) * 100)
  
  elderly$south$study <- cov_data$south %>% 
    mutate(elderly = ifelse(age > 60, 'yes', 'no')) %>% 
    filter(!is.na(elderly)) %>% 
    count(elderly) %>% 
    mutate(percent = n/sum(n) * 100)
  
  elderly$south$test <- elderly$south %>% 
    map(~.x$n) %>% 
    reduce(cbind) %>% 
    chisq.test
  
# Educational sector in the study and the Austrian and Italian populations -----

  education <- list()
  
  ## Tyrol: http://www.statistik.at/web_de/statistiken/menschen_und_gesellschaft/arbeitsmarkt/erwerbsstatus/index.html
  
  education$north$population <- tibble(education = c('yes', 'no'), 
                                       n = c(289.2 * 1000, 
                                             (4296.9 - 289.2)*1000)) %>% 
    mutate(percent = n/sum(n) * 100)
  
  education$north$study <- cov_data$north %>% 
    mutate(education = ifelse(employment_sector == 'education', 'yes', 'no')) %>% 
    filter(!is.na(education)) %>% 
    count(education) %>% 
    mutate(percent = n/sum(n) * 100)
  
  education$north$test <- education$north %>% 
    map(~.x$n) %>% 
    reduce(cbind) %>% 
    chisq.test
  
  ## Italy: http://dati.istat.it/Index.aspx?QueryId=36872&lang=en
  
  education$south$population <- tibble(education = c('yes', 'no'), 
                                       n = c(102636 + 283827 + 198590 + 301122, 
                                             24974.8*1000 - (102636 + 283827 + 198590 + 301122))) %>% 
    mutate(percent = n/sum(n) * 100)
  
  education$south$study <- cov_data$south %>% 
    mutate(education = ifelse(employment_sector == 'education', 'yes', 'no')) %>% 
    filter(!is.na(education)) %>% 
    count(education) %>% 
    mutate(percent = n/sum(n) * 100)
  
  education$south$test <- education$south %>% 
    map(~.x$n) %>% 
    reduce(cbind) %>% 
    chisq.test
  
# Health care sector in the study and the Austrian and Italian populations -----
  
  health <- list()
  
  ## Tyrol: http://www.statistik.at/web_de/statistiken/menschen_und_gesellschaft/arbeitsmarkt/erwerbsstatus/index.html
  
  health$north$population <- tibble(health = c('yes', 'no'), 
                                    n = c(466.4 * 1000, 
                                          (4296.9 - 466.4)*1000)) %>% 
    mutate(percent = n/sum(n) * 100)
  
  health$north$study <- cov_data$north %>% 
    mutate(health = ifelse(employment_sector == 'health services', 'yes', 'no')) %>% 
    filter(!is.na(health)) %>% 
    count(health) %>% 
    mutate(percent = n/sum(n) * 100)
  
  health$north$test <- health$north %>% 
    map(~.x$n) %>% 
    reduce(cbind) %>% 
    chisq.test
  
  # South Tyrol, sources: http://dati.istat.it/Index.aspx?QueryId=17887&lang=en#
  
  health$south$population <- tibble(health = c('yes', 'no'), 
                                    n = c(2754 + 763 + 3517 + 726 + 444 + 8403 + 939, 
                                          267.3 * 1000 - (2754 + 763 + 3517 + 726 + 444 + 8403 + 939))) %>% 
    mutate(percent = n/sum(n) * 100)
  
  health$south$study <- cov_data$south %>% 
    mutate(health = ifelse(employment_sector == 'health services', 'yes', 'no')) %>% 
    filter(!is.na(health)) %>% 
    count(health) %>% 
    mutate(percent = n/sum(n) * 100)
  
  health$south$test <- health$south %>% 
    map(~.x$n) %>% 
    reduce(cbind) %>% 
    chisq.test
  
  


# Participant cluster fractions -----
  
  part_frac <- list()
  
  part_frac <- part_clust[c('analyses_north', 'analyses_south')] %>% 
    map(~map(.x, ngroups)) %>% 
    unlist(recursive = FALSE) %>% 
    map(mutate, 
        percent = n/sum(n)*100)