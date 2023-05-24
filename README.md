
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The Role of Fish Feces for Nutrient Cycling on Coral Reefs :tropical_fish: :poop:

The goal of this project is to reproduce all results and figures
presented in the paper “The Role of Fish Feces for Nutrient Cycling on
Coral Reefs”, currently under revision at **Oikos**. The project uses
[targets](https://docs.ropensci.org/targets/), a pipeline tool for
statistics and data science in R. Packages needed to reproduce this
analysis can be found in [\_targets.R](_targets.R). To reproduce all
analyses, run the code `targets::tar_make()`.

## Content

The directory contains:

- [:file_folder: R](/R): Folder containing all functions, packages and
  drake plan.  
  - **functions.R**: Contains functions used to reproduce analysis.  
  - **functions_plots.R**: Contains functions to create figures
  presenting the results.  
- [:file_folder: data](/data): Folder containing all data.
- [:file_folder: stan](/stan): Folder containing stan code.
- [:file_folder: output](/output): All output including results and
  figures for publication.
- \_targets.R: Script containing workflow
- make.R File: Script to run the whole project.

## Data

**results_aecnp.csv** - id: identification code of individual  
- key: AE1 (= gut content) or AE2 (= feces)  
- c: carbon content (in %)  
- n: nitrogen content (in %)  
- p: phosphorus content (in %)  
- location: island name  
- lat: latitude  
- sitenumber: id of the site  
- long: longitude  
- habitat: slope, lagoon, or channel  
- date  
- time  
- species: species name  
- weight: wet weight of fish in g  
- tl: total length (in cm)  
- sl: standard length (in cm)  
- sex  
- ae1: 1 if sample was taken from stomach for that individual  
- ae2: 1 if sample was taken from feces for that individual  
- family  
- year

**ash_species_estimate.csv**: Ash ratios per species - species: species
name  
- diet2: diet category used in this study  
- ash_ratio: ash of gut content divided by ash content of feces  
- source_ash: source of the ash values (either this study or a paper
reference)

**coprophagy_robertson1982**: Data extracted from Robertson 1982  
- troph_robertson: trophic guild as defined by robertson (1982)  
- family: family of species  
- species: species name  
- habitat: habitat where observation was made  
- prop_eaten: proportion of feces that were eaten  
- n_feces: number of defecations observed  
- coprophage: is the species considered coprophagic? 1 = yes, 0 = no

**extrapolation_trophic_guilds**: Produced data from Parravicini et
al. (2020)  
Please see paper and supplementary info for details.  
*Parravicini V, Casey JM, Schiettekatte NMD, Brandl SJ, Pozas-Schacre C,
Carlot J, et al. (2020) Delineating reef fish trophic guilds with global
gut content data synthesis and phylogeny. PLoS Biol 18(12): e3000702.
<https://doi.org/10.1371/journal.pbio.3000702>*

**intestine_dataset**: Data on intestine morphology from Ghilardi et
al. (2021)  
All length measurements are in mm and weights in g.  
Please see paper and supplementary info for details about this
dataset.  
*Ghilardi, M., Schiettekatte, N. M. D., Casey, J. M., Brandl, S. J.,
Degregori, S., Mercière, A., Morat, F., Letourneur, Y., Bejarano, S., &
Parravicini, V. (2021). Phylogeny, body morphology, and trophic level
shape intestinal traits in coral reef fishes. Ecology and Evolution, 11,
13218– 13231. <https://doi.org/10.1002/ece3.8045>*

**Moorea_coastline**: shapefiles of Moorea’s coastline used to create
map.

**Moorea_uvc.csv**: Underwater visual census conducted and shared by
CRIOBE (Centre of Island Research and Environmental Observatory).  
- Lat: latitude  
- Long: longitude  
- TransectLength: length of transect (in m)  
- TransectWidth: width of transect (in m)  
- Method_area: area of uvc (in m²)  
- Taxon: Species name  
- Abundance: number of individuals  
- Size: total length of individuals (in cm)

**params_sst_glob.csv**: parameters used for bioenergetic modeling,
extracted from Schiettekatte et al. (2022)  
Please see paper and supplementary info for details about these
parameters.  
*Schiettekatte, N.M.D., Brandl, S.J., Casey, J.M. et al. Biological
trade-offs underpin coral reef ecosystem functioning. Nat Ecol Evol 6,
701–708 (2022). <https://doi.org/10.1038/s41559-022-01710-5>*

## Session info

    #> R version 4.3.0 (2023-04-21)
    #> Platform: x86_64-pc-linux-gnu (64-bit)
    #> Running under: Ubuntu 20.04.6 LTS
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
    #> time zone: Pacific/Honolulu
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.3.0  fastmap_1.1.1   cli_3.6.1       tools_4.3.0    
    #>  [5] htmltools_0.5.5 rstudioapi_0.14 yaml_2.3.7      rmarkdown_2.21 
    #>  [9] knitr_1.42      xfun_0.39       digest_0.6.30   rlang_1.1.0    
    #> [13] evaluate_0.20
