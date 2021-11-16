# this script provides tools for wrangling the survey data

# tools ------

  require(plyr)
  require(tidyverse)

# basic formatting functions -----

  pss2date <- function(x) {
    
    ## converts the SPSS date to R
    
    return(as.Date(x/86400, origin = '1582-10-14') )
    
  }
  
  rm_space <- function(x) {
    
    ## removes extra trailing spaces
    
    return(stri_replace(x, 
                        regex = '\\s+$', 
                        replacement = ''))
    
  }
  
  format_na_no <- function(x) {
    
    ## formats the comorbidity (and other data) data
    ## where NA is assumed 'no'
    
    return(ifelse(is.na(x), 'no', 'yes') %>% 
             factor(c('no', 'yes')))
    
    
  }
  
  yn_variable <- function(inp_vec) {
    
    ## recodes 0 to no, 1 to yes
    
    out_vec <- car::recode(inp_vec, 
                           "'0' = 'no'; 
                              '1' = 'yes'") %>% 
      factor(c('no', 'yes'))
    
    return(out_vec)
    
  }
  
  sum_variables <- function(inp_tbl, vars_to_sum) {
    
    ## calculates a sum of the given variables
    ## coded as yes/no (changed to 1 and 0, respectively)
    
    var_sum_vec <- vars_to_sum %>% 
      map(function(x) car::recode(inp_tbl[[x]], "'no' = 0; 'yes' = 1") %>% 
            as.character %>% 
            as.numeric) %>% 
      reduce(function(x, y) x + y)
    
    return(var_sum_vec)
    
  }
  
  binarize_variables <- function(inp_tbl, vars_to_recode) {
    
    ## recodes yes/no to 1/0
    
    tbl_list <- vars_to_recode %>% 
      map(function(x) tibble(ID = inp_tbl[['ID']], 
                             sympt_recoded = car::recode(inp_tbl[[x]], 
                                                                    "'no' = 0; 
                                                                        'yes' = 1") %>% 
                               as.character %>% 
                               as.numeric)) %>% 
      set_names(vars_to_recode)
    
    rec_tbl <- tbl_list %>% 
      map2(., names(.), 
           function(x, y) set_names(x, c('ID', y))) %>% 
      reduce(left_join, 
             by = 'ID')
    
    return(rec_tbl)
    
  }

  translate_var <- function(variable, 
                            key = 'variable', 
                            out_value = 'label', 
                            dict = globals$var_lexicon, 
                            time_lab = T, 
                            unit = F) {
    
    naming_vec <- dict[[out_value]]

    if(unit) {
      
      naming_vec <- ifelse(is.na(dict[['unit']]), 
                           naming_vec, 
                           paste(naming_vec, dict[['unit']], sep = ', '))
      
    }
    
    naming_vec <- set_names(naming_vec, 
                            dict[[key]])
    
    return(naming_vec[variable])
    
  }
  

  format_ja_nein <- function(x) {
    
    ## translates Ja/Nein
    
    return(car::recode(x, "'Ja' = 'yes'; 'Nein' = 'no'") %>% 
             factor(c('no', 'yes')))
    
    
  }

# quarantine duration, co-morbidity, medication, symptom, mental health item re-coding -------
  
  get_quarantine <- function(quara_string) {
    
    ## does its best to obtain the quarantine duration from a text introduced by the participants
    ## converts dates and text only to NAs. Converts 'x Wochen' and 'x Monate' to the respective values
    ## if more numeric values are identified, the maximum is extracted
    
    quara_string <- quara_string %>% 
      rm_space
    
    quara_formatted <- quara_string %>% 
      stri_replace(regex = '^(K|k)eine$|^(N|n)ie$|^(N|n)iemals$', 
                   replacement = '0') %>% 
      stri_replace(regex = '^(G|g)ar\\s{1,4}(N|n)icht$', 
                   replacement = '0') %>% 
      stri_replace_all(regex = '\\d{1,2}\\.\\d{1,2}\\.(2020|2021)', 
                       replacement = '') %>% 
      stri_replace_all(regex = '\\d{1,2}\\.\\d{1,2}', 
                       replacement = '') %>% 
      stri_replace_all(fixed = '\n', 
                       replacement = '') %>% 
      stri_extract_all(regex = '\\d{1,2}\\s{0,4}(tage|Tage|Woche|woche|Wochen|wochen|Monat|monat|giorni|mese|mesi|gg|settimane|\\s{0,1})') %>%
      map(stri_replace_all, 
          regex = '(T|t)ag\\w{0,2}', 
          replacement = '') %>% 
      map(stri_replace_all, 
          regex = 'giorni|gg|Più|di', 
          replacement = '') %>% 
      map(rm_space) %>% 
      map(stri_replace, 
          fixed = '2 settimane', 
          replacement = '14') %>% 
      map(stri_replace, 
          fixed = '3 settimane', 
          replacement = '21') %>% 
      map(stri_replace, 
          fixed = '4 settimane', 
          replacement = '28') %>% 
      map(stri_replace, 
          regex = '(e|E)in (m|M)onat', 
          replacement = '30') %>% 
      map(stri_replace, 
          fixed = '3 settimane', 
          replacement = '21') %>% 
      map(stri_replace, 
          fixed = 'un mese', 
          replacement = '30') %>% 
      map(stri_replace, 
          fixed = '1 mese', 
          replacement = '30') %>% 
      map(stri_replace, 
          fixed = '2 messi', 
          replacement = '60') %>% 
      map(stri_replace_all, 
          regex = '4\\s{0,4}(M|m)onat\\w{0,2}', 
          replacement = '120') %>% 
      map(stri_replace_all, 
          regex = '3\\s{0,4}(M|m)onat\\w{0,2}', 
          replacement = '90') %>% 
      map(stri_replace_all, 
          regex = '2\\s{0,4}(M|m)onat\\w{0,2}', 
          replacement = '60') %>% 
      map(stri_replace_all, 
          regex = '1\\s{0,4}(M|m)onat\\w{0,2}', 
          replacement = '30') %>% 
      map(stri_replace_all, 
          regex = '1\\s{0,4}(W|w)och\\w{0,2}', 
          replacement = '7') %>% 
      map(stri_replace_all, 
          regex = '2\\s{0,4}(W|w)och\\w{0,2}', 
          replacement = '14') %>% 
      map(stri_replace_all, 
          regex = '3\\s{0,4}(W|w)och\\w{0,2}', 
          replacement = '21') %>% 
      map(stri_replace_all, 
          regex = '4\\s{0,4}(W|w)och\\w{0,2}', 
          replacement = '28') %>% 
      map(stri_replace_all, 
          regex = '5\\s{0,4}(W|w)och\\w{0,2}', 
          replacement = '35') %>%
      map(stri_replace_all, 
          regex = '6\\s{0,4}(W|w)och\\w{0,2}', 
          replacement = '42') %>% 
      map(as.numeric) %>% 
      map_dbl(function(x) max(x, na.rm = T)) %>% 
      map_dbl(function(x) if(is.infinite(x)) NA else x)
    
    return(quara_formatted)    
    
  }

  format_comorb <- function(inp_tbl, comorb_var, new_var) {
    
    ## recodes the given comorbidity value: NA assumed no tick
    
    out_tbl <- inp_tbl %>% 
      mutate(ID = rm_space(studien_id), 
             rec_var = format_na_no(.data[[comorb_var]]), 
             rec_var = factor(rec_var, 
                              c('no', 'yes'))) %>% 
      select(ID, 
             rec_var) %>% 
      set_names(c('ID', new_var))
    
    return(out_tbl)
    
  }
  
  format_medication <- function(inp_tbl, med_var, new_var) {
    
    ## same as above for the medication
    
    out_tbl <- inp_tbl %>% 
      mutate(ID = rm_space(studien_id), 
             rec_var = car::recode(.data[[med_var]], 
                                   "'Ja' = 'yes'; 
                                           'Nein' = 'no'; 
                                           'Unbekannt' = NA"), 
             rec_var = factor(rec_var, 
                              c('no', 'yes'))) %>% 
      select(ID, 
             rec_var) %>% 
      set_names(c('ID', new_var))
    
    return(out_tbl)
    
  }
  
  format_symptom <- function(inp_tbl, sympt_var, new_var_prefix) {
    
    ## recodes and classifies the given symptom: acute if present up to 
    ## 4 weeks, chronic if present longer
    
    out_tbl <- inp_tbl %>% 
      mutate(ID = rm_space(studien_id), 
             sympt_duration_class = .data[[sympt_var]], 
             sympt_recoded = car::recode(.data[[sympt_var]], 
                                         "'[b]Nicht aufgetreten[/b]' = 'absent'; 
                                           '1-3 Tage' = '1 - 3 days'; 
                                           'Bis 1 Woche' = 'up to 1 week';
                                           'Bis 2 Wochen' = 'up to 2 weeks'; 
                                           'Bis 4 Wochen' = 'up to 4 weeks'; 
                                           'Bis 3 Monate' = 'up to 3 months'; 
                                           'Bis 6 Monate' = 'up to 6 months'; 
                                           'Über 6 Monate' = 'over 6 months'") %>% 
               factor(c('absent', 
                        '1 - 3 days', 
                        'up to 1 week', 
                        'up to 2 weeks', 
                        'up to 4 weeks', 
                        'up to 3 months', 
                        'up to 6 months',
                        'over 6 months')), 
             symptom_acute = car::recode(.data[[sympt_var]], 
                                         "'[b]Nicht aufgetreten[/b]' = 'no'; 
                                           '1-3 Tage' = 'yes'; 
                                           'Bis 1 Woche' = 'yes';
                                           'Bis 2 Wochen' = 'yes'; 
                                           'Bis 4 Wochen' = 'yes'; 
                                           'Bis 3 Monate' = 'yes'; 
                                           'Bis 6 Monate' = 'yes'; 
                                           'Über 6 Monate' = 'yes'") %>% 
               factor(c('no', 'yes')), 
             symptom_immediate = car::recode(.data[[sympt_var]], 
                                         "'[b]Nicht aufgetreten[/b]' = 'no'; 
                                           '1-3 Tage' = 'no'; 
                                           'Bis 1 Woche' = 'no';
                                           'Bis 2 Wochen' = 'yes'; 
                                           'Bis 4 Wochen' = 'yes'; 
                                           'Bis 3 Monate' = 'yes'; 
                                           'Bis 6 Monate' = 'yes'; 
                                           'Über 6 Monate' = 'yes'") %>% 
               factor(c('no', 'yes')), 
             symptom_int =  car::recode(.data[[sympt_var]], 
                                        "'[b]Nicht aufgetreten[/b]' = 'no'; 
                                           '1-3 Tage' = 'no'; 
                                           'Bis 1 Woche' = 'no';
                                           'Bis 2 Wochen' = 'no'; 
                                           'Bis 4 Wochen' = 'yes'; 
                                           'Bis 3 Monate' = 'yes'; 
                                           'Bis 6 Monate' = 'yes'; 
                                           'Über 6 Monate' = 'yes'")%>% 
               factor(c('no', 'yes')), 
             symptom_long = car::recode(.data[[sympt_var]], 
                                        "'[b]Nicht aufgetreten[/b]' = 'no'; 
                                           '1-3 Tage' = 'no'; 
                                           'Bis 1 Woche' = 'no';
                                           'Bis 2 Wochen' = 'no'; 
                                           'Bis 4 Wochen' = 'no'; 
                                           'Bis 3 Monate' = 'yes'; 
                                           'Bis 6 Monate' = 'yes'; 
                                           'Über 6 Monate' = 'yes'")%>% 
               factor(c('no', 'yes')), 
             symptom_chronic = car::recode(.data[[sympt_var]], 
                                        "'[b]Nicht aufgetreten[/b]' = 'no'; 
                                           '1-3 Tage' = 'no'; 
                                           'Bis 1 Woche' = 'no';
                                           'Bis 2 Wochen' = 'no'; 
                                           'Bis 4 Wochen' = 'no'; 
                                           'Bis 3 Monate' = 'no'; 
                                           'Bis 6 Monate' = 'yes'; 
                                           'Über 6 Monate' = 'yes'")%>% 
               factor(c('no', 'yes'))) %>% 
      select(ID, 
             sympt_recoded, 
             symptom_acute, 
             symptom_immediate, 
             symptom_int, 
             symptom_long, 
             symptom_chronic)
    
    out_tbl <- out_tbl %>% 
      set_names(c('ID', 
                  paste(new_var_prefix, 
                        c('', 
                          '_acute', 
                          '_immediate', 
                          '_subacute', 
                          '_long', 
                          '_chronic'), 
                        sep = '')))
    
    return(out_tbl)
    
  }
  
  format_mental <- function(inp_tbl, mental_var, new_var) {
    
    ## re-codes a mental health variable
    
    out_tbl <- inp_tbl %>% 
      mutate(ID = rm_space(studien_id), 
             rec_var = car::recode(.data[[mental_var]], 
                                   "'Überhaupt nicht' = 'never'; 
                                     'An einzelnen Tagen' = 'some days'; 
                                     'An mehr als der Hälfte der Tage' = 'more than half days'; 
                                     'Beinahe jeden Tag' = 'almost every day'") %>% 
               factor(c('never', 
                        'some days', 
                        'more than half days', 
                        'almost every day')), 
             rec_var_severity = car::recode(.data[[mental_var]], 
                                            "'Überhaupt nicht' = 0;
                                            'An einzelnen Tagen' = 1; 
                                            'An mehr als der Hälfte der Tage' = 2;
                                            'Beinahe jeden Tag' = 3"), 
             rec_var_severity = as.numeric(as.character(rec_var_severity))) %>% 
      select(ID, 
             rec_var, 
             rec_var_severity) %>% 
      set_names(c('ID', paste(new_var, 
                              c('', '_score'), sep = '')))
    
    return(out_tbl)
    
  }
  
  format_psychsoc <- function(inp_tbl, psych_var, new_var) {
    
    ## re-codes a mental health variable
    
    out_tbl <- inp_tbl %>% 
      mutate(ID = rm_space(studien_id), 
             rec_var = car::recode(.data[[psych_var]], 
                                   "'Nicht' = 'no'; 
                                     'Wenig' = 'little'; 
                                     'Mäßig' = 'moderate'; 
                                     'Stark' = 'strong'") %>% 
               factor(c('no', 'little', 'moderate', 'strong')), 
             rec_var_severity = car::recode(.data[[psych_var]], 
                                           "'Nicht' = 0; 
                                     'Wenig' = 1; 
                                     'Mäßig' = 2; 
                                     'Stark' = 3"), 
             rec_var_severity = as.numeric(as.character(rec_var_severity))) %>% 
      select(ID, 
             rec_var, 
             rec_var_severity) %>% 
      set_names(c('ID', paste(new_var, c('', '_score'), sep = '')))
    
    return(out_tbl)
    
  }

# survey part reading functions ------

  extract_demo <- function(raw_survey, 
                           form_fill_var, 
                           region_var, 
                           responder_var, 
                           lang_var, 
                           age_var, 
                           cohabitant_var, 
                           pos_cohabitant_var, 
                           height_var, 
                           w_before_var, 
                           w_recent_var, 
                           emplovment_var, 
                           employment_sec_var) {
    
    ## extracts and clears the given demographic data from the raw survey answers
    
    ## data extraction
    
    extr_data <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             form_filled = pss2date(.data[[form_fill_var]]), 
             number_of_results = number_of_result_dates,  
             center = rm_space(departments), 
             date = pss2date(date), 
             responder = rm_space(.data[[responder_var]]), 
             sex = rm_space(geschlecht), 
             education = rm_space(bildungsabschluss), 
             region = rm_space(.data[[region_var]]), 
             age = .data[[age_var]], 
             cohabitants = .data[[cohabitant_var]], 
             pos_cohabitants = .data[[pos_cohabitant_var]], 
             height = .data[[height_var]], 
             weigth_before = .data[[w_before_var]],
             weigth_recent = .data[[w_recent_var]], 
             employment_before = .data[[emplovment_var]], 
             employment_sector = .data[[employment_sec_var]])
    
    if(lang_var %in% names(raw_survey)) {
      
      extr_data <- extr_data %>% 
        mutate(language = .data[[lang_var]])
      
    } else {
      
      extr_data <- extr_data %>% 
        mutate(language = lang_var)
      
    }
    
    ## data clearing
    
    cleared_data <- extr_data %>%  
      mutate(responder = ifelse(responder == 'Selbst', 
                                'self', 
                                'representative') %>% 
               factor(c('self', 'representative')), 
             sex = car::recode(sex, 
                               "'Weiblich' = 'female'; 
                               'Männlich' = 'male'; 
                               '' = 'NA';
                               'femminile' = 'female'; 
                               'maschile' = 'male'") %>% 
               factor(c('female', 'male')), 
             education = car::recode(education, 
                                     "'Allgemeinbildende höhere Schule' = 'secondary'; 
                                     'Universität' = 'tertiary'; 
                                     'Kolleg' = 'tertiary'; 
                                     'Berufsbildende höhere Schule' = 'secondary'; 
                                     'Lehre' = 'apprenticeship'; 
                                     'Hochschulverwandte Lehranstalt' = 'tertiary'; 
                                     'Pflichtschule' = 'elementary'; 
                                     'Berufsbildende mittlere Schule' = 'secondary'; 
                                     '' = NA; 
                                     'Hochschulabschluss' = 'tertiary'; 
                                     'Matura' = 'secondary'; 
                                     'Mittelschule' = 'secondary'; 
                                     'Berufsschule (2-3 Jahre nach Mittelschulabschluss)' = 'secondary'; 
                                     'Grundschule oder kein Titel' = 'elementary'; 
                                     'Maturità' = 'secondary'; 
                                     'Titolo di istruzione superiore o universitaria' = 'tertiary';
                                     'Scuola elementare o nessun titolo' = 'elementary'; 
                                     'Scuola media' = 'secondary'; 
                                     'Scuola professionale (2-3 anni dopo il diploma della scuola media)' = 'secondary'") %>% 
               factor(c('secondary', 'apprenticeship', 'elementary', 'tertiary'))) %>% 
      mutate(region = car::recode(region, 
                                  "'Brixen' = 'Brixen/Bressanone'; 
                                    'Bressanone' = 'Brixen/Bressanone'; 
                                    'Bozen' = 'Bozen/Bolzano'; 
                                    'Bolzano' = 'Bozen/Bolzano'; 
                                    'Bruneck' = 'Bruneck/Brunico'; 
                                    'Brunico' = 'Bruneck/Brunico'; 
                                    'Meran' = 'Meran/Merano'; 
                                    'Merano' = 'Meran/Merano'"), 
             region_class = ifelse(region %in% c('Bozen/Bolzano', 'Innsbruck-Stadt'), 
                                   'capital', 
                                   'non-capital') %>% 
               factor(c('capital', 'non-capital')), 
             language = car::recode(language, 
                                    "'Deutsch' = 'German'; 
                                      'Ladinisch' = 'Ladin'; 
                                      'Italienisch' = 'Italian'; 
                                      'Andere Sprache' = 'Other'") %>% 
               factor(c('German', 'Italian', 'Ladin', 'Other'))) %>% 
      mutate(age = ifelse(age < 16, 
                          NA, 
                          ifelse(age > 1900, 
                                 2021 - age, 
                                 age)), 
             age_class = cut(age, 
                             c(0, 30, 65, Inf), 
                             c('young', 'middle-aged', 'elderly')), 
             household_size = cut(cohabitants, 
                                  c(-Inf, 1, 2, Inf), 
                                  c('single', 'pair', 'family/community')),
             #pos_cohabitants = ifelse(pos_cohabitants == 0, NA, pos_cohabitants), 
             pos_cohabitants_class = cut(pos_cohabitants, 
                                         c(-Inf, 0, 1, 2, Inf), 
                                         c('0', '1', '2', 'over 2'))) %>% 
      mutate(height = ifelse(height < 100, NA, height), 
             weigth_before = ifelse(weigth_before < 25 | weigth_before > 250, 
                                    NA, 
                                    weigth_before), 
             weigth_recent = ifelse(weigth_recent < 25 | weigth_recent > 250, 
                                    NA, 
                                    weigth_before), 
             bmi_before = weigth_before/(height/100) ^ 2, 
             bmi_recent = weigth_recent/(height/100) ^ 2, 
             bmi_class_before = cut(bmi_before, 
                                    c(-Inf, 25, 30, Inf), 
                                    c('normal', 'overweight', 'obesity')), 
             bmi_class_recent = cut(bmi_recent, 
                                    c(-Inf, 25, 30, Inf), 
                                    c('normal', 'overweight', 'obesity'))) %>%
      mutate(employment_before = car::recode(employment_before, 
                                             "'Ja' = 'employed'; 
                                               'Nein' = 'unemployed'; 
                                               'In Mutterschaft' = 'leave'; 
                                               'In Pension' = 'retired'; 
                                               'In Karenz' = 'leave'") %>% 
               factor(c('employed', 'unemployed', 'leave', 'retired'))) %>% 
      mutate(employment_sector = car::recode(employment_sector, 
                                             "'Gastronomie/Tourismus' = 'gastronomy/tourism'; 
                                               'Gesundheitswesen' = 'health services'; 
                                               'Lebensmittelhandel' = 'food trade'; 
                                               'Öffentliche Verkehrsmittel' = 'public transportation'; 
                                               'Blaulichtorganisation' = 'emergency services'; 
                                               'Baubranche' = 'construction'; 
                                               'Verwaltung/Büro' = 'administration/office'; 
                                               'Industrie' = 'industry'; 
                                               'Landwirtschaft' = 'agriculture'; 
                                               'Bildungswesen' = 'education'; 
                                               'Andere' = 'other';
                                               'öffentl. Sicherheit / Zivilschutz' = 'emergency sector'") %>% 
               factor(c('other',
                        'gastronomy/tourism', 
                        'health services', 
                        'food trade', 
                        'public transportation', 
                        'emergency services', 
                        'construction', 
                        'administration/office', 
                        'industry', 
                        'agriculture', 
                        'education')), 
             completion_season = cut(date, 
                                     c(as.Date('2020-01-01'), 
                                       as.Date('2020-12-31'), 
                                       as.Date('2021-09-30')), 
                                     c('fall 2020', 
                                       'winter/spring 2021')))
    
    ## selecting the variables of interest
    
    cleared_data <- cleared_data %>% 
      select(ID, 
             form_filled, 
             number_of_results, 
             center, 
             date, 
             responder, 
             sex, 
             education, 
             region, 
             region_class, 
             language, 
             age, 
             age_class, 
             cohabitants, 
             household_size, 
             pos_cohabitants, 
             pos_cohabitants_class, 
             height, 
             weigth_before, 
             weigth_recent, 
             bmi_before, 
             bmi_recent, 
             bmi_class_before, 
             bmi_class_recent, 
             employment_before, 
             employment_sector, 
             completion_season)
    
    return(cleared_data)
    
  }
  
  extract_comorb <- function(raw_survey, 
                             comorb_var_prefix) {
    
    ## extracts the co-morbidity, medication and smoking data
    ## from the raw survey answers
    
    ## extraction of the yes/no answers for particular co-morbidities
    
    extr_data <- list(comorb_var = raw_data$north %>%
                        select(starts_with(comorb_var_prefix)) %>% 
                        names, 
                      new_var = c('comorb_absent', 
                                  globals$comorb)) %>% 
      pmap(format_comorb, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID')
    
    ## clearing
    ## derived: co-morbidity sum, multi-morbidity
    
    cleared_data <- extr_data %>% 
      mutate(comorb_sum = sum_variables(inp_tbl = extr_data, 
                                        vars_to_sum = globals$comorb)) %>% 
      mutate(multi_morbidity = cut(comorb_sum, 
                                   c(-Inf, 0, 1, 2, Inf), 
                                   c('absent', '1', '2', '3 and more')))
    
    ## variables of interest
    
    cleared_data <- cleared_data %>% 
      select(ID, 
             all_of(c('comorb_absent', 
                      globals$comorb)), 
             comorb_sum, 
             multi_morbidity)
    
    return(cleared_data)
    
  }
  
  extract_medication <- function(raw_survey, 
                                 drug_type_vars, 
                                 daily_medication_var, 
                                 anti_coagulation_var, 
                                 flu_vacc_var, 
                                 pneumo_vacc_var, 
                                 smoking_var) {
    
    ## extracts the data refering to the daily medication, 
    ## smoking status and vacciantion
    
    ## extracting the particular drug types taken daily
    
    extr_data <- list(med_var = drug_type_vars, 
                      new_var = globals$daily_medication) %>% 
      pmap(format_medication, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID')
    
    ## extracting all other features
    
    extr_data <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             daily_medication = .data[[daily_medication_var]], 
             anti_coagulation_type = .data[[anti_coagulation_var]], 
             flu_vaccination = .data[[flu_vacc_var]], 
             pneumo_vaccination = .data[[pneumo_vacc_var]], 
             smoking = .data[[smoking_var]]) %>%
      left_join(extr_data, 
                by = 'ID')
    
    ## clearing and derived parameters
    
    cleared_data <- extr_data %>% 
      mutate(daily_medication = car::recode(daily_medication, 
                                            "'Keine' = 'absent'; 
                                              'Unter 5' = '1 - 4 drugs'; 
                                              '5 oder mehr' = '5 drugs and more'") %>% 
               factor(c('absent', 
                        '1 - 4 drugs', 
                        '5 drugs and more')), 
             flu_vaccination = car::recode(flu_vaccination, 
                                           "'Ja' = 'yes'; 
                                               'Nein' = 'no'; 
                                               'Jährlich' = 'yearly'; 
                                               'Bin mir nicht sicher' = 'unknown'") %>% 
               factor(c('no', 'yes', 'yearly', 'unknown')), 
             flu_vacc_status = car::recode(flu_vaccination, 
                                           "'yes' = 'yes'; 
                                               'no' = 'no'; 
                                               'yearly' = 'yes'; 
                                               'unknown' = 'no'") %>% 
               factor(c('no', 'yes')), 
             pneumo_vaccination = car::recode(pneumo_vaccination, 
                                              "'Ja' = 'yes'; 
                                                  'Nein' = 'no'; 
                                                  'Bin mir nicht sicher' = 'unknown'") %>% 
               factor(c('no', 'yes', 'unknown')), 
             pneumo_vacc_status= car::recode(pneumo_vaccination, 
                                             "'yes' = 'yes'; 
                                                  'no' = 'no'; 
                                                  'unknown' = 'no'") %>% 
               factor(c('no', 'yes')), 
             smoking = car::recode(smoking, 
                                   "'Raucher' = 'active'; 
                                       'Ex-Raucher' = 'former'; 
                                       'Nie-Raucher' = 'never'") %>% 
               factor(c('never', 'former', 'active')))  %>% 
      select(ID, 
             daily_medication, 
             all_of(globals$daily_medication), 
             anti_coagulation_type, 
             flu_vaccination, 
             flu_vacc_status, 
             pneumo_vaccination, 
             pneumo_vacc_status, 
             smoking)
    
    ## improving the daily drug classes
    
    cleared_data <- cleared_data %>% 
      mutate(daily_immunosuppression = ifelse(daily_medication == 'absent', 
                                              'no', 
                                              as.character(daily_immunosuppression)) %>% 
               factor(c('no', 'yes')), 
             daily_cortison = ifelse(daily_medication == 'absent', 
                                     'no', 
                                     as.character(daily_cortison)) %>% 
               factor(c('no', 'yes')), 
             daily_ace_drugs = ifelse(daily_medication == 'absent', 
                                      'no', 
                                      as.character(daily_ace_drugs)) %>% 
               factor(c('no', 'yes')), 
             daily_pain_killers = ifelse(daily_medication == 'absent', 
                                         'no', 
                                         as.character(daily_pain_killers)) %>% 
               factor(c('no', 'yes')), 
             daily_anti_coagulation = ifelse(daily_medication == 'absent', 
                                             'no', 
                                             as.character(daily_anti_coagulation)) %>% 
               factor(c('no', 'yes')))
    
    return(cleared_data)
    
    
  }
  
  extract_cov_diagnosis <- function(raw_survey, 
                                    test_date_var, 
                                    symptom_onset_date_var, 
                                    contact_var, 
                                    contact_sympt_var, 
                                    quarantine_var, 
                                    test_auth_var, 
                                    time_result_var, 
                                    result_report_var, 
                                    information_quality_var, 
                                    auth_support_var, 
                                    support_who_var, 
                                    suport_quality_var, 
                                    test_pos_week_var, 
                                    ab_test_var, 
                                    diagn_pcr_var, 
                                    diagn_antibodies_var, 
                                    no_symptom_var, 
                                    reporting_body_vars, 
                                    repeated_test_var) {
    
    ## extracts the data concerning the diagnosis of COVID-19
    
    extr_data <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             first_pos_test_date = pss2date(.data[[test_date_var]]), 
             first_symptom_date = pss2date(.data[[symptom_onset_date_var]]), 
             contact_positive = format_ja_nein(.data[[contact_var]]), 
             contact_to_symptom = .data[[contact_sympt_var]], 
             quarantine_days = .data[[quarantine_var]], 
             test_authority = .data[[test_auth_var]], 
             days_to_result = .data[[time_result_var]], 
             result_reported_other_who = .data[[result_report_var]], 
             information_quality = .data[[information_quality_var]], 
             support_authority = format_ja_nein(.data[[auth_support_var]]), 
             support_authority_who = .data[[support_who_var]], 
             support_authority_quality = .data[[suport_quality_var]], 
             test_positive_weeks = .data[[test_pos_week_var]], 
             antibody_test_positive = .data[[ab_test_var]])
    
    extr_data <- list(comorb_var = c(diagn_pcr_var,  
                                     diagn_antibodies_var, 
                                     no_symptom_var, 
                                     reporting_body_vars, 
                                     repeated_test_var), 
                      new_var = c('diagn_pcr', 
                                  'diagn_antibodies', 
                                  'asymptomatic', 
                                  'result_reported_authority', 
                                  'result_reported_hospital', 
                                  'result_reported_other', 
                                  'test_positive_not_repeated')) %>% 
      pmap(format_comorb, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID') %>% 
      left_join(extr_data, 
                ., 
                by = 'ID')
    
    ## data clearing: format and recoding the symptom onset date
    
    cleared_data <- extr_data %>% ## typo correction, where the typos could be unequivocally identified
      mutate(first_pos_test_date = stri_replace(first_pos_test_date, 
                                                regex = '1920|1921|2010|1971|2002|2019|2029|2030|2000|2038|3020', 
                                                replacement = '2020') %>% 
               stri_replace(regex = '2022|1970', 
                            replacement = '2021') %>% 
               stri_replace(fixed = '2021-10', 
                            replacement = '2020-10') %>% 
               stri_replace(fixed = '2021-11', 
                            replacement = '2020-11') %>% 
               stri_replace(fixed = '2021-12', 
                            replacement = '2020-12') %>% 
               as.Date, 
             first_symptom_date = stri_replace(first_symptom_date, 
                                               regex = '1920|1921|2010|1971|2002|2019|2029|2030|2000|2038|3020', 
                                               replacement = '2020') %>% 
               stri_replace(regex = '2022|1970', 
                            replacement = '2021') %>% 
               stri_replace(fixed = '2021-10', 
                            replacement = '2020-10') %>% 
               stri_replace(fixed = '2021-11', 
                            replacement = '2020-11') %>% 
               stri_replace(fixed = '2021-12', 
                            replacement = '2020-12') %>% 
               as.Date) %>% 
      mutate(first_pos_test_date = ifelse(first_pos_test_date < as.Date('2020-02-01'), 
                                          NA, 
                                          first_pos_test_date), ## filtering out those declaring the test before Feb. 2020
             first_symptom_date = ifelse(first_symptom_date < as.Date('2020-02-01'), 
                                         NA, 
                                         first_pos_test_date), 
             cov_outbreak = cut(first_pos_test_date, 
                                c(as.Date('2020-01-01'), 
                                  as.Date('2020-06-30'), 
                                  as.Date('2020-12-31'), 
                                  as.Date('2021-12-31')), 
                                c('spring 2020', 
                                  'summer/fall 2020', 
                                  'winter/spring 2021'))) 

    cleared_data <- cleared_data %>% 
      mutate(contact_to_symptom = rm_space(contact_to_symptom) %>% 
               as.numeric, 
             contact_to_symptom_class = cut(contact_to_symptom, 
                                            c(-Inf, 4, Inf), 
                                            c('0 - 4 days', 
                                              'over 5 days')), 
             quarantine_days = suppressWarnings(get_quarantine(quarantine_days)), 
             quarantine_days_class = cut(quarantine_days, 
                                         c(-Inf, 10, 14, Inf), 
                                         c('0 - 10 days', 
                                           '11 - 14 days', 
                                           'over 15 days')), 
             test_authority = car::recode(test_authority, 
                                          "'Zu Hause durch fahrenden Dienst' = 'mobile team'; 
                                          'Screeningstraße' = 'screening station'; 
                                          'Beim Hausarzt' = 'GP'; 
                                          'Im Krankenhaus' = 'hospital'; 
                                          'Drive-in' = 'screening station'") %>% 
               factor(c('screening station', 
                        'mobile team', 
                        'GP', 
                        'hospital')), 
             days_to_result = car::recode(days_to_result, 
                                          "'Am selben Tag' = 'same day'; 
                                          'Am nächsten Tag' = 'next day'; 
                                          'Nach 2 Tagen' = 'in 2 days'; 
                                          'Nach 3-4 Tagen' = 'in 3 - 4 days'; 
                                          'Nach 5 Tagen und mehr' = 'in 5 days or longer'") %>% 
               factor(c('same day', 
                        'next day', 
                        'in 2 days', 
                        'in 3 - 4 days', 
                        'in 5 days or longer')), 
             information_quality = car::recode(information_quality, 
                                               "'Ja' = 'yes'; 
                                               'Nein' = 'no'; 
                                               'Teilweise' = 'partially'") %>% 
               factor(c('no', 'partially', 'yes')), 
             support_authority_who = car::recode(support_authority_who, 
                                                 "'Hygienedienst' = 'authority'; 
                                                 'Hausarzt' = 'GP'") %>% 
               factor(c('authority', 'GP')), 
             support_authority_quality = car::recode(support_authority_quality, 
                                                     "'Nein' = 'no'; 
                                                     'Weniger' = 'little'; 
                                                     'Moderat' = 'fairly'; 
                                                     'Sehr' = 'much'") %>% 
               factor(c('no', 'little', 'fairly', 'much')), 
             test_positive_weeks = rm_space(test_positive_weeks) %>% 
               as.numeric, 
             antibody_test_positive = car::recode(antibody_test_positive, 
                                                  "'Nein' = 'no'; 
                                                  'Ja' = 'yes'; 
                                                  'Bin mir nicht sicher' = 'no'") %>% 
               factor(c('no', 'yes')))
      
    cleared_data <- cleared_data %>% 
      select(ID, 
             first_pos_test_date, 
             first_symptom_date, 
             contact_positive, 
             contact_to_symptom, 
             contact_to_symptom_class, 
             quarantine_days, 
             quarantine_days_class, 
             test_authority, 
             days_to_result, 
             result_reported_other_who, 
             information_quality, 
             support_authority, 
             support_authority_who, 
             support_authority_quality, 
             diagn_pcr, 
             diagn_antibodies, 
             asymptomatic, 
             result_reported_authority, 
             result_reported_hospital, 
             result_reported_other, 
             test_positive_weeks, 
             test_positive_not_repeated, 
             antibody_test_positive, 
             cov_outbreak)
    
    return(cleared_data)
    
  }
  
  extract_course <- function(raw_survey, 
                             sympt_free_var, 
                             hair_loss_var, 
                             other_sympt_var, 
                             w_loss_index, 
                             w_loss_kg_var, 
                             surgery_infection_var, 
                             surgery_infection_type_var, 
                             illness_feeling_var, 
                             relapse_var, 
                             subj_inf_var, 
                             gp_contact_var, 
                             hosp_var, 
                             hosp_days_var, 
                             icu_var, 
                             pregn_problems_var, 
                             pregn_problems_desc, 
                             birth_problems_var, 
                             birth_problems_desc, 
                             baby_sep_var, 
                             feeding_problems_var, 
                             feeding_problems_desc, 
                             bonding_problems_var, 
                             bonding_problems_desc, 
                             partner_birth_var, 
                             partner_birth_vis_var, 
                             contact_gp_var, 
                             contact_internist_var, 
                             contact_pulmo_var, 
                             contact_neurologist_var, 
                             contact_other_phys_var, 
                             contacted_hotline_var, 
                             contacted_emergency_var, 
                             cov_therapy_none_var,
                             cov_therapy_antipyretic, 
                             cov_therapy_antibiotic, 
                             cov_pregnancy, 
                             cov_birth,
                             cov_breast_feeding, 
                             cov_no_circumst) {
    
    ## extracts and clears the data on disease course
    
    course_data <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             symptom_free = format_ja_nein(.data[[sympt_free_var]]), 
             hair_loss = format_ja_nein(.data[[hair_loss_var]]), 
             other_symptoms = .data[[other_sympt_var]], 
             weight_loss = format_ja_nein(.data[[w_loss_index]]), 
             weight_loss_kg = rm_space(.data[[w_loss_kg_var]]) %>% 
               as.numeric, 
             surgery_infection = format_ja_nein(.data[[surgery_infection_var]]), 
             surgery_infection_type = .data[[surgery_infection_type_var]], 
             illness_feeling = car::recode(.data[[illness_feeling_var]], 
                                           "'weniger als 1 Woche' = '1 week'; 
                                           '1 Woche' = '1 week'; 
                                           '2 Wochen' = '2 weeks'; 
                                           '3 Wochen' = '3 weeks'; 
                                           '4 Wochen' = '4 weeks'; 
                                           'mehr als 4 Wochen' = 'over 4 weeks'") %>% 
               factor(c('1 week', '2 weeks', '3 weeks', '4 weeks', 'over 4 weeks')), 
             illness_feeling_long = car::recode(.data[[illness_feeling_var]], 
                                                "'weniger als 1 Woche' = 'no'; 
                                                 '1 Woche' = 'no'; 
                                                 '2 Wochen' = 'no';
                                                 '3 Wochen' = 'yes'; 
                                                 '4 Wochen' = 'yes'; 
                                                 'mehr als 4 Wochen' = 'yes'") %>% 
               factor(c('no', 'yes')), 
             relapse = format_ja_nein(.data[[relapse_var]]), 
             subj_infection = car::recode(.data[[subj_inf_var]], 
                                          "'Mildem Atemwegsinfekt' = 'cold-like'; 
                                          'Magen-Darm-Grippe' = 'gastroenteritis';
                                          'Schwere Grippe' = 'flu-like'; 
                                          'Noch nie erlebt' = 'not experienced before'") %>% 
               factor(c('cold-like', 
                        'gastroenteritis', 
                        'flu-like', 
                        'not experienced before')), 
             physician_contact = format_ja_nein(.data[[gp_contact_var]]), 
             hospitalization = format_ja_nein(.data[[hosp_var]]), 
             hospitalization_days = rm_space(.data[[hosp_days_var]]) %>% 
               as.numeric, 
             icu = format_ja_nein(.data[[icu_var]]), 
             problems_pregnancy = format_ja_nein(.data[[pregn_problems_var]]), 
             problems_pregnancy_what = .data[[pregn_problems_desc]], 
             problems_birth = format_ja_nein(.data[[birth_problems_var]]), 
             problems_birth_what = .data[[birth_problems_desc]], 
             baby_separated = format_ja_nein(.data[[baby_sep_var]]), 
             breast_feeding_problems = format_ja_nein(.data[[feeding_problems_var]]), 
             breast_feeding_problems_what = .data[[feeding_problems_desc]], 
             bonding_problems = format_ja_nein(.data[[bonding_problems_var]]), 
             bonding_problems_what = .data[[bonding_problems_desc]], 
             partner_birth = format_ja_nein(.data[[partner_birth_var]]), 
             partner_visit_birth = format_ja_nein(.data[[partner_birth_vis_var]])) %>% 
      mutate(weight_loss_kg = ifelse(weight_loss == 'no', 0, weight_loss_kg), 
             weight_loss_class = cut(weight_loss_kg, 
                                     c(-Inf, 0, 2, 5, Inf), 
                                     c('none', 'mild', 'moderate', 'severe'))) %>% 
      select(ID,
             symptom_free, 
             hair_loss, 
             weight_loss, 
             weight_loss_kg, 
             weight_loss_class, 
             surgery_infection, 
             surgery_infection_type, 
             illness_feeling, 
             illness_feeling_long, 
             relapse, 
             subj_infection, 
             physician_contact, 
             hospitalization, 
             hospitalization_days, 
             icu, 
             problems_pregnancy, 
             problems_pregnancy_what, 
             problems_birth, 
             problems_birth_what, 
             baby_separated, 
             breast_feeding_problems,
             breast_feeding_problems_what, 
             bonding_problems, 
             bonding_problems_what, 
             partner_birth, 
             partner_visit_birth)
    
    ## other features connected to the course of COVID-19
    
    course_data <- list(comorb_var = c(contact_gp_var, 
                                       contact_internist_var, 
                                       contact_pulmo_var, 
                                       contact_neurologist_var, 
                                       contact_other_phys_var, 
                                       contacted_hotline_var, 
                                       contacted_emergency_var, 
                                       cov_therapy_none_var,
                                       cov_therapy_antipyretic, 
                                       cov_therapy_antibiotic, 
                                       cov_pregnancy, 
                                       cov_birth,
                                       cov_breast_feeding, 
                                       cov_no_circumst), 
                        new_var = c('contacted_gp', 
                                    'contacted_internist', 
                                    'contacted_pulmonologist', 
                                    'contacted_neurologist', 
                                    'contacted_other_physician', 
                                    'contacted_healthline', 
                                    'contacted_emergency', 
                                    'therapy_covid_none', 
                                    'therapy_covid_antipyretic', 
                                    'therapy_home_antibiotic', 
                                    'pregnancy_while_cov', 
                                    'birth_while_cov',
                                    'breast_feeding_while_cov', 
                                    'no_special_circum_while_cov')) %>% 
      pmap(format_comorb, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID') %>% 
      left_join(course_data, 
                ., 
                by = 'ID')
    
    return(course_data)
    
  }
  
  
  extract_symptoms <- function(raw_survey, 
                               symptom_vars) {
    
    ## extracts and transforms the symptom duration data
    
    extr_data <- list(sympt_var = symptom_vars, 
                      new_var_prefix = globals$symptoms) %>% 
      pmap(format_symptom, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID')
    
    ## derived parameters: number of flash, acute, sub-acute, long and chronic symptoms
    
    cleared_data <- extr_data %>% 
      mutate(sum_symptoms_acute = sum_variables(inp_tbl = extr_data,
                                                vars_to_sum = paste(globals$symptoms, 
                                                                    'acute', 
                                                                    sep = '_')), 
             sum_symptoms_immediate = sum_variables(inp_tbl = extr_data, 
                                                    vars_to_sum = paste(globals$symptoms, 
                                                                        'immediate', 
                                                                        sep = '_')), 
             sum_symptoms_subacute = sum_variables(inp_tbl = extr_data, 
                                                   vars_to_sum = paste(globals$symptoms, 
                                                                       'subacute', 
                                                                       sep = '_')), 
             sum_symptoms_long = sum_variables(inp_tbl = extr_data, 
                                               vars_to_sum = paste(globals$symptoms, 
                                                                   'long', 
                                                                   sep = '_')), 
             sum_symptoms_chronic = sum_variables(inp_tbl = extr_data, 
                                               vars_to_sum = paste(globals$symptoms, 
                                                                   'chronic', 
                                                                   sep = '_')))

    return(cleared_data)
    
  }
  
  extract_mental <- function(raw_survey, 
                             mental_health_vars, 
                             mental_health_var, 
                             life_quality_var) {
    
    ## extracts and clears mental health data
    
    mental_tbl <- list(mental_var = mental_health_vars, 
                       new_var = c('disinterest', 
                                   'depression', 
                                   'anxiety', 
                                   'nervosity', 
                                   'overwhelming_concern', 
                                   'stigma')) %>% 
      pmap(format_mental, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID')
    
    mental_tbl <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             mental_health = car::recode(.data[[mental_health_var]], 
                                         "'Schlecht' = 'poor'; 
                                      'Ausreichend' = 'fair';
                                      'Gut' = 'good'; 
                                      'Ausgezeichnet' = 'excellent'") %>% 
               factor(c('poor', 
                        'fair', 
                        'good', 
                        'excellent')), 
             mental_health_score = car::recode(.data[[mental_health_var]], 
                                               "'Schlecht' = 3; 
                                      'Ausreichend' = 2;
                                      'Gut' = 1; 
                                      'Ausgezeichnet' = 0") %>% 
               as.character %>% 
               as.numeric, 
             life_quality = car::recode(.data[[life_quality_var]], 
                                        "'Schlecht' = 'poor'; 
                                      'Ausreichend' = 'fair';
                                      'Gut' = 'good'; 
                                      'Ausgezeichnet' = 'excellent'") %>% 
               factor(c('poor', 
                        'fair', 
                        'good', 
                        'excellent')), 
             life_quality_score = car::recode(.data[[life_quality_var]], 
                                              "'Schlecht' = 3; 
                                      'Ausreichend' = 2;
                                      'Gut' = 1; 
                                      'Ausgezeichnet' = 0", 
                                              as.numeric = T) %>% 
               as.character %>% 
               as.numeric) %>% 
      select(ID, 
             mental_health, 
             mental_health_score, 
             life_quality, 
             life_quality_score) %>% 
      left_join(mental_tbl, 
                ., 
                by = 'ID')
    
    ## derived parameters: PHQ4 scoring for depression and anxiety, cutoff 3 for single items
    ## cutoff 6 for the common D/A screening
    
    mental_tbl <- mental_tbl %>% 
      mutate(phq_depression_score = disinterest_score + depression_score, 
             phq_anxiety_score = anxiety_score + overwhelming_concern_score, 
             phq_depression_positive = ifelse(phq_depression_score >= 3, 'yes', 'no') %>% 
               factor(c('no', 'yes')), 
             phq_anxiety_positive = ifelse(phq_anxiety_score >= 3, 'yes', 'no') %>% 
               factor(c('no', 'yes')), 
             da_score = depression_score + anxiety_score, 
             da_positive = ifelse(da_score >= 6, 'yes', 'no') %>% 
               factor(c('no', 'yes')))
    
    return(mental_tbl)
    
  }
  
  extract_psychsoc <- function(raw_survey, 
                               psychsoc_vars, 
                               wokplace_change_var, 
                               workplace_changes_descr, 
                               workplace_changes_quality) {
    
    ## extracts the psychsocial features
    
    psychosoc_tbl <- list(psych_var = psychsoc_vars, 
                          new_var = c('health_concerns', 
                                      'relationship_problems', 
                                      'care_problems', 
                                      'work_education_stress', 
                                      'financial_concerns', 
                                      'workplace_concerns', 
                                      'covid_thoughts', 
                                      'missing_contact_person', 
                                      'missing_closeness')) %>% 
      pmap(format_psychsoc, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID')
    
    ## calculating the PHQ stress score
    
    psychosoc_tbl <- psychosoc_tbl %>% 
      mutate(stress_score = health_concerns_score + 
               relationship_problems_score + 
               care_problems_score + 
               work_education_stress_score + 
               financial_concerns_score + 
               covid_thoughts_score + 
               missing_contact_person_score, 
             stress_positive = ifelse(stress_score >= 7, 'yes', 'no') %>% 
               factor(c('no', 'yes')))
    
    ## workplace changes
    
    workplace_tbl <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             workplace_changes_post = format_ja_nein(.data[[wokplace_change_var]]), 
             workplace_changes_post_quality = car::recode(.data[[workplace_changes_quality]], 
                                                          "'positive' = 'positive'; 
                                                          'negativ' = 'negative'"), 
             workplace_changes_post_what = .data[[workplace_changes_descr]], 
             workplace_changes_post_quality = ifelse(workplace_changes_post == 'no', 
                                                     'no change', 
                                                     ifelse(workplace_changes_post_quality == 'negative', 
                                                            'negative', 
                                                            ifelse(!is.na(workplace_changes_post_quality), 
                                                                   'positive', 
                                                                   NA))) %>% 
               factor(c('negative', 'no change', 'positive')), 
             workplace_changes_post_score = car::recode(workplace_changes_post_quality, 
                                                        "'negative' = 1; 
                                                        'no change' = 2; 
                                                        'positive' = 3"), 
             workplace_changes_post_score = as.numeric(as.character(workplace_changes_post_score))) %>% 
      select(ID, 
             workplace_changes_post, 
             workplace_changes_post_quality, 
             workplace_changes_post_what, 
             workplace_changes_post_score)
    
    psychosoc_tbl <- left_join(psychosoc_tbl, 
                               workplace_tbl, 
                               by = 'ID')
 
    
    return(psychosoc_tbl)
    
  }
  
  extract_followup <- function(raw_survey, 
                               fup_heart_infr_var, 
                               fup_stroke_var, 
                               fup_embo_var, 
                               fup_infect_var, 
                               fup_infect_desc, 
                               fup_infect_number, 
                               fup_no_conditions_var, 
                               acute_surgery_fup_var, 
                               acute_surgery_desc, 
                               new_med_fup_var, 
                               new_med_fup_descr, 
                               postp_treatment_var,
                               postp_treatment_desc, 
                               treatment_madeup_var, 
                               treatment_madeup_delay, 
                               new_symptoms_covindep_var, 
                               new_symptoms_covindep_desc, 
                               rehabilitation_var, 
                               rehabilitation_type, 
                               rehabilitation_advised_var, 
                               performance_var, 
                               perf_percent, 
                               rehabilitation_need_var, 
                               convalescence_var, 
                               convalescence_date) {
    
    ## extracts and clears the follow-up data
    
    ## follow-up diseases and surgeries
    
    followup_tbl <- list(comorb_var = c(fup_heart_infr_var, 
                                        fup_stroke_var, 
                                        fup_embo_var, 
                                        fup_infect_var, 
                                        fup_no_conditions_var), 
                         new_var = c('infarct_fup', 
                                     'stroke_fup', 
                                     'emoblism_fup', 
                                     'other_infections_fup', 
                                     'no_diseses_fup')) %>% 
      pmap(format_comorb, 
           inp_tbl = raw_survey) %>% 
      reduce(left_join, 
             by = 'ID')
    
    followup_tbl <- raw_survey %>% 
      mutate(ID = rm_space(studien_id), 
             other_infections_fup_what = .data[[fup_infect_desc]], 
             other_infections_fup_number = as.numeric(rm_space(.data[[fup_infect_number]])), 
             acute_surgery_fup = format_ja_nein(.data[[acute_surgery_fup_var]]), 
             acute_surgery_fup_what = .data[[acute_surgery_desc]],
             new_medication_fup =  format_ja_nein(.data[[new_med_fup_var]]), 
             new_medication_fup_what = .data[[new_med_fup_descr]], 
             treatment_postponed_fup = format_ja_nein(.data[[postp_treatment_var]]), 
             treatment_postponed_fup_what = .data[[postp_treatment_desc]], 
             treatment_madeup_fup = format_ja_nein(.data[[treatment_madeup_var]]), 
             treatment_madeup_fup_delay = rm_space(.data[[treatment_madeup_delay]]), 
             new_symptoms_covindep_fup = format_ja_nein(.data[[new_symptoms_covindep_var]]), 
             new_symptoms_covindep_fup_what = .data[[new_symptoms_covindep_desc]], 
             rehabilitation_fup = format_ja_nein(.data[[rehabilitation_var]]), 
             rehabilitation_fup_type = car::recode(.data[[rehabilitation_type]], 
                                                   "'stationär' = 'stationary'; 
                                                 'ambulant' = 'ambulatory'"), 
             rehabilitation_fup_advised = format_ja_nein(.data[[rehabilitation_advised_var]]), 
             performance_fup = car::recode(.data[[performance_var]], 
                                           "'Wieder wie vor Infektion' = 'as before'; 
                                         'Gering eingeschränkt' = 'little impairment'; 
                                         'Deutlich eingeschränkt' = 'moderate impairment'; 
                                         'Sehr eingeschränkt' = 'severe impairment'") %>% 
               factor(c('as before', 
                        'little impairment', 
                        'moderate impairment', 
                        'severe impairment')), 
             perf_impairment = 100 - as.numeric(rm_space(.data[[perf_percent]])), 
             rehabilitation_fup_needed = format_ja_nein(.data[[rehabilitation_need_var]]), 
             complete_covelescence = format_ja_nein(.data[[convalescence_var]]), 
             covalescence_date = .data[[convalescence_date]]) %>% 
      select(ID, 
             other_infections_fup_what, 
             other_infections_fup_number, 
             acute_surgery_fup, 
             acute_surgery_fup_what,
             new_medication_fup, 
             new_medication_fup_what, 
             treatment_postponed_fup, 
             treatment_postponed_fup_what, 
             treatment_madeup_fup, 
             treatment_madeup_fup_delay, 
             new_symptoms_covindep_fup, 
             new_symptoms_covindep_fup_what, 
             rehabilitation_fup, 
             rehabilitation_fup_type, 
             rehabilitation_fup_advised, 
             performance_fup, 
             perf_impairment, 
             rehabilitation_fup_needed, 
             complete_covelescence, 
             covalescence_date) %>% 
      left_join(followup_tbl, 
                ., 
                by = 'ID')
    
    ## derived parameter: stratification of the percentual performance impairment
    
    followup_tbl <- followup_tbl %>% 
      mutate(perf_impairment_class = cut(perf_impairment, 
                                         c(-Inf, 25, 50, 75, Inf), 
                                         c('0 - 25%', 
                                           '26 - 50%', 
                                           '51% - 75%', 
                                           '76 - 100%')), 
             perf_impairment_50 = ifelse(perf_impairment > 50, 'yes', 'no') %>% 
               factor(c('no', 'yes')))
    
    
    return(followup_tbl)
    
  }
  
  extract_vacc_readiness <- function(raw_survey, 
                                     flu_vacc_var, 
                                     flu_vacc_desc, 
                                     covid_vacc_var, 
                                     covid_vacc_desc) {
    
    ## extracts and clears the vaccination readiness: flue and COVID-19
    
    vacc_tbl <- raw_survey %>% 
      mutate(ID = rm_space(studien_id),
             flu_vaccination_readiness = format_ja_nein(.data[[flu_vacc_var]]), 
             flu_vaccination_what = .data[[flu_vacc_desc]], 
             covid_vaccination_readiness = format_ja_nein(.data[[covid_vacc_var]]), 
             covid_vaccination_what = .data[[covid_vacc_desc]]) %>% 
      select(ID, 
             flu_vaccination_readiness, 
             flu_vaccination_what, 
             covid_vaccination_readiness,
             covid_vaccination_what)
    
    return(vacc_tbl)
    
  }
  
# calculation of the maximal symptom duration ------
  
  find_max_duration <- function(inp_tbl, sympt_var_vec) {
    
    ## finds the maximal time with at least one symptom
    
    sympt_tbl <- inp_tbl %>% 
      select(all_of(c('ID', sympt_var_vec))) %>% 
      column_to_rownames('ID')
    
    ID_vec <- rownames(sympt_tbl)
    
    ## re-coding the duration to the numbers corresponding to the duration class
    
    sympt_tbl <- sympt_tbl %>% 
      map_dfc(function(x) car::recode(x, 
                                      "'absent' = 0; 
                                      '1 - 3 days' = 1;
                                      'up to 1 week' = 2;
                                      'up to 2 weeks' = 3;
                                      'up to 4 weeks' = 4;
                                      'up to 3 months' = 5;
                                      'up to 6 months' = 6;
                                      'over 6 months' = 7") %>% 
                as.character %>% 
                as.numeric) %>% 
      as.data.frame
    
    rownames(sympt_tbl) <- ID_vec
    
    ## finding the maximum for each ID
    
    max_vec <- ID_vec %>% 
      map_dbl(function(x) max(sympt_tbl[x, ], na.rm = T)) %>% 
      set_names(ID_vec)
    
    max_tbl <- tibble(ID = names(max_vec), 
                      sympt_duration_class = ifelse(is.infinite(max_vec), NA, max_vec)) %>% 
      mutate(max_sympt_duration = car::recode(sympt_duration_class, 
                                              "'0' = 'absent'; 
                                           '1' = '1 - 3 days'; 
                                           '2' = 'up to 1 week';
                                           '3' = 'up to 2 weeks'; 
                                           '4' = 'up to 4 weeks'; 
                                           '5' = 'up to 3 months'; 
                                           '6' = 'up to 6 months'; 
                                           '7' = 'over 6 months'"), 
             max_sympt_duration_class = car::recode(sympt_duration_class, 
                                                    "'0' = 'absent'; 
                                           '1' = '1 - 3 days'; 
                                           '2' = 'up to 1 week';
                                           '3' = 'up to 2 weeks'; 
                                           '4' = 'up to 4 weeks'; 
                                           '5' = 'over 4 weeks'; 
                                           '6' = 'over 4 weeks'; 
                                           '7' = 'over 4 weeks'")) %>% 
      select(- sympt_duration_class)
    
    return(max_tbl)
    
  }
  
# END ------