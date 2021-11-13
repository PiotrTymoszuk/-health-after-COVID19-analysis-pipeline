# this script performs additional data wrangling tasks: stratification of the symptom sums
# for the risk modeling
# source of weights: AGES (https://covid19-dashboard.ages.at/dashboard.html), Ministerio di Saluto
# (https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_7-luglio-2021.pdf)

  insert_head()
  
# data container -----
  
  model_wrangling <- list()

# overall acute symptom and phenotype symptom sums, quartile stratification -----
  
  insert_msg('Acute syptom count, phenotype symptom number, quartile stratification')
  
  model_wrangling$acute_sympt <- cov_data %>% 
    map(strat_quartile, 
        numeric_variable = 'sum_symptoms_acute', 
        new_var_name = 'sum_symptoms_acute_class', 
        labels = c('1Q', '2Q', '3Q', '4Q'))
  
  model_wrangling$acute_pheno <- symptom_clust$symptom_sums %>% 
    map(strat_quartile, 
        numeric_variable = c('acute.NIP', 'acute.MOP'), 
        new_var_name = c('acute_nip_class', 'acute_mop_class'), 
        labels = c('1Q', '2Q', '3Q', '4Q'))
  
  ## marging with the source data sets
  
  cov_data <- map2(cov_data, 
                   model_wrangling$acute_sympt,
                   left_join, 
                   by = 'ID') %>% 
    map2(., model_wrangling$acute_pheno, 
         left_join, 
         by = 'ID')

# reading the data and defining the weights based on age and sex of all Tyrolean and Italian convalescents ------
  
  insert_msg('Defining the modeling weights')

  ## north
  
  model_wrangling$weights$north <- read_delim('./input data/CovidFaelle_Altersgruppe.csv', 
                                              delim = ';')  %>% 
    filter(Bundesland == 'Tirol') %>% 
    mutate(age_strata = Altersgruppe , 
           age_weight_class = paste('age', AltersgruppeID , sep = '_'), 
           sex = ifelse(Geschlecht == 'M', 'male', 'female'), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_'), 
           number_convalescents = AnzahlGeheilt, 
           freq_weight = number_convalescents/sum(number_convalescents)) %>% 
    select(age_weight_class, 
           age_strata, 
           sex, 
           number_convalescents, 
           weight_strata_id, 
           freq_weight)
  
  ## south
  
  model_wrangling$weights$south <- read_excel('./input data/age_sex_dist_italy.xlsx') %>% 
    mutate(number_convalescents_male = male_cases - male_deaths, 
           number_convalescents_female = female_cases - female_deaths, 
           age_weight_class = paste('age', 1:nrow(.) , sep = '_')) %>% 
    gather(key = 'sex', 
           value = 'number_convalescents', 
           number_convalescents_male, 
           number_convalescents_female) %>% 
    mutate(sex = stri_extract(sex, regex = 'male|female'), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_'), 
           freq_weight = number_convalescents/sum(number_convalescents)) %>% 
    select(age_weight_class, 
           age_strata, 
           sex, 
           number_convalescents, 
           weight_strata_id, 
           freq_weight)
  
# merging the weights with the study data -----
  
  ## north
  
  model_wrangling$weight_tbls$north <- cov_data$north %>% 
    select(ID, 
           sex, 
           age) %>% 
    mutate(age_weight_class = cut(age, 
                                  c(-Inf, 4, 14, 24, 34, 44, 54, 64, 74, 84, Inf), 
                                  paste('age', 1:10, sep = '_')), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_')) %>% 
    select(ID, 
           weight_strata_id) %>% 
    left_join(model_wrangling$weights$north %>% 
                select(weight_strata_id, 
                       freq_weight), 
              by = 'weight_strata_id')
  
  ## south
  
  model_wrangling$weight_tbls$south <- cov_data$south %>% 
    select(ID, 
           sex, 
           age) %>% 
    mutate(age_weight_class = cut(age, 
                                  c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, 89, Inf), 
                                  paste('age', 1:10, sep = '_')), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_')) %>% 
    select(ID, 
           weight_strata_id) %>% 
    left_join(model_wrangling$weights$south %>% 
                select(weight_strata_id, 
                       freq_weight), 
              by = 'weight_strata_id')
  
  ## weights for the PASC subset
  
  model_wrangling$weight_tbls$north_pasc <- model_wrangling$weight_tbls$north
  
  model_wrangling$weight_tbls$south_pasc <- model_wrangling$weight_tbls$south
  
  
  ## merging
  
  cov_data <- map2(cov_data, 
                   model_wrangling$weight_tbls, 
                   left_join, 
                   by = 'ID')
  
# END -----
  
  insert_tail()