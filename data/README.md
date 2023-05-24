
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data from ‘The Role of Fish Feces for Nutrient Cycling on Coral Reefs’ :tropical_fish: :poop:

This folder contains all data needed to reproduce the results of the
paper “The Role of Fish Feces for Nutrient Cycling on Coral Reefs”.

Data files:

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
