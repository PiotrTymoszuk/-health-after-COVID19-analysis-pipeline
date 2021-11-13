# this script renders a word file with the figures and supplementary material

  insert_head()

# tools -----

  library(bookdown)
  library(knitr)
  library(rmarkdown)
  library(kableExtra)

# rendering the figures and tables ------

  insert_msg('Rendering the figures and legends')
  
  render('./paper/markdown/figures_and_tables.Rmd', 
         output_format = pdf_document2(number_sections = FALSE), 
         output_dir = './paper/') 
  
# rendering the supplementary material -----
  
  insert_msg('Rendering the supplementary material')
  
  render('./paper/markdown/supplementary_material.Rmd', 
        output_format = pdf_document2(number_sections = FALSE), 
        output_dir = './paper/') 
  
# END ------
  
  insert_tail()