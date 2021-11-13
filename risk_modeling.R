# This script performs risk modeling tasks

# tools -----

  c('./tools/sys_tools.R', 
    './tools/cov_project_tools.R', 
    './tools/cov_project_globals.R', 
    './tools/counting_tools.R', 
    './tools/lm_qc_tools.R', 
    './tools/lasso_selection_tools.R') %>% 
    walk(source)

# executable scripts

  c('./risk modeling scripts/data_clearing.R', ## stratifying the symptom sums and frequency weights for modeling
    './risk modeling scripts/univariate_modeling.R', ## univariable risk modeling
    './risk modeling scripts/lasso_modeling.R') %>% ## LASSO multiparameter modeling
    walk(source)

# END -----