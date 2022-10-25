# Install missing packages
if (!"devtools" %in% installed.packages()){
  install.packages("devtools")
} 

devtools::install_deps()

# Execute plan in clean session
drake::r_make()

## just render text
library(rmarkdown)
library(drake)
rmarkdown::render(knitr_in("text/main.Rmd"), 
                output_format = "word_document", 
               output_dir = "./output/text/",
               output_file = "main.docx")
