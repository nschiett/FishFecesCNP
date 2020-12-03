# Install missing packages
if (!"devtools" %in% installed.packages()){
  install.packages("devtools")
} 

devtools::install_deps()

# Execute plan in clean session
drake::r_make()
