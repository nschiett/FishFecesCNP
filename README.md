
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The Role of Fish Feces for Nutrient Cycling on Coral Reefs :tropical_fish: :poop:

The goal of this project is to reproduce all results and figures
presented in the paper “The Role of Fish Feces for Nutrient Cycling on
Coral Reefs”, currently under revision at **Oikos**. The project uses
[targets](https://docs.ropensci.org/targets/), a pipeline tool for
statistics and data science in R. Packages needed to reproduce this
analysis and code to install them can be found in [make.R](make.R).

## Content

The directory contains:

-   [:file_folder: R](/R): Folder containing all functions, packages and
    drake plan.  
    - **functions.R**: Contains functions used to reproduce analysis.  
    - **functions_plots.R**: Contains functions to create figures
    presenting the results.  
-   [:file_folder: data](/data): Folder containing all data.
-   [:file_folder: stan](/stan): Folder containing stan code.
-   [:file_folder: output](/output): All output including results and
    figures for publication.
-   \_targets.R: Script containing workflow
-   make.R File: Script to run the whole project.

## Session info

    #> R version 4.1.3 (2022-03-10)
    #> Platform: x86_64-pc-linux-gnu (64-bit)
    #> Running under: Ubuntu 20.04.4 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
    #> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_US.UTF-8    
    #>  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_US.UTF-8   
    #>  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.1.3  magrittr_2.0.3  fastmap_1.1.0   cli_3.6.0      
    #>  [5] tools_4.1.3     htmltools_0.5.2 rstudioapi_0.13 yaml_2.2.1     
    #>  [9] stringi_1.7.6   rmarkdown_2.20  knitr_1.37      stringr_1.4.0  
    #> [13] xfun_0.36       digest_0.6.29   rlang_1.0.6     evaluate_0.14
