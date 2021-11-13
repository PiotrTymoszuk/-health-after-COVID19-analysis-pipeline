# A mother script executing the table, figure and paper rendering scripts

  insert_head()

# executable scripts ------
  
  insert_msg('Rendering the paper files')

  c('./paper scripts/cohort_characteristic.R', ## compares the cohorts
    './paper scripts/paper_tables.R', 
    './paper scripts/paper_figures.R', 
    './paper scripts/deploy_paper.R') %>% 
    walk(source)
 
# END -----