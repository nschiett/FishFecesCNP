dat <- result_ext %>%
  mutate(cdif1 = Dc_lqn > Wc_uqn | Dc_uqn < Wc_lqn,
         ndif1 = Dn_lqn > Wn_uqn | Dn_uqn < Wn_lqn,
         pdif1 = Dp_lqn > Wp_uqn | Dp_uqn < Wp_lqn,
         cndif1 = Dcn_lqn > Wcn_uqn | Dcn_uqn < Wcn_lqn,
         npdif1 = Dnp_lqn > Wnp_uqn | Dnp_uqn < Wnp_lqn,
         cpdif1 = Dcp_lqn > Wcp_uqn | Dcp_uqn < Wcp_lqn,
         cdif2 = Dc_lqr > Wc_uqr | Dc_uqr < Wc_lqr,
         ndif2 = Dn_lqr > Wn_uqr | Dn_uqr < Wn_lqr,
         pdif2 = Dp_lqr > Wp_uqr | Dp_uqr < Wp_lqr,
         cndif2 = Dcn_lqr > Wcn_uqr | Dcn_uqr < Wcn_lqr,
         npdif2 = Dnp_lqr > Wnp_uqr | Dnp_uqr < Wnp_lqr,
         cpdif2 = Dcp_lqr > Wcp_uqr | Dcp_uqr < Wcp_lqr) %>%
  filter(location == "Moorea")

library(ggrepel)
library(tidyverse)
p1 <- ggplot(dat) +
  geom_abline(slope = 1) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.25) +
  geom_abline(slope = 0.75) +
  geom_point(aes(x = Dc_median, y = Wc_median, color = diet2), alpha = 1) +
  # geom_point(aes(x = Dc_median, y = Wc_median, color = diet2),
  #            size = 1, data = dat[dat$cdif2 == TRUE,]) +
  # geom_point(aes(x = Dc_median, y = Wc_median, color = diet2),
  #            size = 2, data = dat[dat$cdif1 == TRUE,]) +
  # geom_text_repel(aes(x = Dc_median, y = Wc_median, label = species), 
  #            size = 3, data = dat[dat$cdif1 == TRUE,]) +
  scale_color_fish_d(option = "Pseudocheilinus_tetrataenia") +
  theme_bw()

p2 <- ggplot(dat) +
  geom_abline(slope = 1) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.25) +
  geom_abline(slope = 0.75) +
  geom_point(aes(x = Dn_median, y = Wn_median), alpha = 0.5) +
  geom_point(aes(x = Dn_median, y = Wn_median), color = "orange",
             size = 2, data = dat[dat$ndif2 == TRUE,]) +
  geom_point(aes(x = Dn_median, y = Wn_median), color = "red",
             size = 2, data = dat[dat$ndif1 == TRUE,]) +
  geom_text_repel(aes(x = Dn_median, y = Wn_median, label = species), 
             size = 2, data = dat[dat$ndif1 == TRUE,]) +
  theme_bw()


p3 <- ggplot(dat) +
  geom_abline(slope = 1) +
  geom_abline(slope = 0.5) +
  #geom_abline(slope = 0.25) +
  geom_abline(slope = 0.75) +
  geom_point(aes(x = Dp_median, y = Wp_median), alpha = 0.5) +
  geom_point(aes(x = Dp_median, y = Wp_median), color = "orange", 
                  size = 2, data = dat[dat$pdif2 == TRUE,]) +
  geom_point(aes(x = Dp_median, y = Wp_median), color = "red", 
             size = 2, data = dat[dat$pdif1 == TRUE,]) +
  geom_text_repel(aes(x = Dp_median, y = Wp_median, label = species), 
             size = 2, data = dat[dat$pdif1 == TRUE,]) +
  theme_bw()

library(patchwork)
p1 + p2 + p3 + plot_layout(nrow = 3)
ggsave("diet_poo_comparison.png", width = 8, height = 10)

ggplot(result_cnp) +
  geom_abline(slope = 1) +
  geom_abline(slope = 2) +
  geom_abline(slope = 0.5) +
  geom_point(aes(x = Dcn_median, y = Wcn_median), alpha = 0.5) +
  geom_point(aes(x = Dcn_median, y = Wcn_median),
             size = 2, color = "orange",
             data = dat[dat$Acn_uqr <1 | dat$Acn_lqr >1, ]) +
  geom_point(aes(x = Dcn_median, y = Wcn_median),
             size = 2, color = "red",
             data = dat[dat$Acn_uqn <1 | dat$Acn_lqn >1, ]) +
  geom_text_repel(aes(x = Dcn_median, y = Wcn_median, label = species),
             size = 2,
             data = dat[dat$Acn_uqn <1 | dat$Acn_lqn >1, ]) +
  theme_bw() +
  xlim(2.5,20)

ggplot(result_cnp) +
  geom_abline(slope = 1) +
  geom_abline(slope = 2) +
  geom_abline(slope = 0.5) +
  geom_point(aes(x = Dnp_median, y = Wnp_median), alpha = 0.5) +
  geom_point(aes(x = Dnp_median, y = Wnp_median),
             size = 2, color = "orange",
             data =  dat[dat$npdif2 == TRUE,]) +
  geom_point(aes(x = Dnp_median, y = Wnp_median),
             size = 2, color = "red",
             data = dat[dat$npdif1 == TRUE,]) +
  geom_text_repel(aes(x = Dnp_median, y = Wnp_median, label = species),
                  size = 2,
                  data = dat[dat$npdif2 == TRUE,]) +
  theme_bw()


ggplot(result_cnp) +
  geom_density(aes(x = Dcn_median)) +
  geom_density(aes(x = Wcn_median), fill = "blue", alpha = 0.5) +
  theme_custom()  
ggplot(data) +
  geom_density(aes(x = Dnp_median)) +
  geom_density(aes(x = Wnp_median), fill = "blue", alpha = 0.5) +
  theme_custom()  
ggplot(data) +
  geom_density(aes(x = Dcp_median)) +
  geom_density(aes(x = Wcp_median), fill = "blue", alpha = 0.5) +
  #scale_x_continuous(trans = "log")
  theme_custom()  
  
ggplot(result_cnp) +
  geom_abline(slope = 1) +
  geom_abline(slope = 2, alpha = 0.5) +
  geom_abline(slope = 0.5) +
  geom_point(aes(x = Dcp_median, y = Wcp_median), alpha = 0.5) +
  geom_point(aes(x = Dcp_median, y = Wcp_median),
             size = 2, color = "orange",
             data = dat[dat$Acp_uqr <1 | dat$Acp_lqr >1, ]) +
  geom_point(aes(x = Dcp_median, y = Wcp_median),
             size = 2, color = "red",
             data = dat[dat$Acp_uqn <1 | dat$Acp_lqn >1, ]) +
  geom_text_repel(aes(x = Dcp_median, y = Wcp_median, label = species),
                  size = 3,
                  data = dat[dat$Acp_uqr <1 | dat$Acp_lqr >1, ]) +
  theme_custom() +
  labs(x = "C:P diet", y = "C:P feces")

ggplot(dat[dat$cdif2 == TRUE,]) +
  geom_point(aes(x = Dc_median, y = Rc_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat[dat$ndif2 == TRUE,]) +
  geom_point(aes(x = (Dn_median), y = (Rn_median))) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat[dat$cdif2 == TRUE,]) +
  geom_point(aes(x = Dp_median, y = Rp_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = Dn_median, y = Rn_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = Dp_median, y = Rp_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = Dc_median, y = Rc_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = Dcn_median, y = Acn_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = Dcp_median, y = Acp_median)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = Dn_median, y = Acn_median, color = diet2)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  
ggplot(dat) +
  geom_point(aes(x = Dp_median, y = Acp_median, color = diet2)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  


lin <- lm(log(Acn_median) ~ log(Dn_median), data = dat)
summary(lin)

ggplot(dat[dat$ndif2 == TRUE,]) +
  geom_point(aes(x = (Dn_median), y = (Rn_median))) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw()  

ggplot(dat) +
  geom_point(aes(x = (Dp_median), y = (Rp_median))) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_abline(slope = 1) +
  #scale_x_continuous(trans = "log") +
  #scale_y_continuous(trans = "log") +
  theme_bw()  


####### supplemental #######
ggplot(dat, aes(y = forcats::fct_rev(species), yend = forcats::fct_rev(species))) +
  geom_hline(aes(yintercept = forcats::fct_rev(species), color = (Rc_median)), 
             alpha = 0.3, size = 5, data = dat[dat$cdif2 == TRUE,]) +
  geom_segment(aes(x = Dc_lqn, xend = Dc_uqn), size = 0.5, color = "grey40",
               linetype = 1, position = position_nudge(y = 0.2)) +
  geom_segment(aes(x = Dc_lqr, xend = Dc_uqr), size = 1.5, color = "grey30",
               linetype = 1, position = position_nudge(y = 0.2)) +
  geom_point(aes(x = Dc_median), size = 2,
             position = position_nudge(y = 0.2)) +
  geom_segment(aes(x = Wc_lqn, xend = Wc_uqn), color = "grey40",
               linetype = 1, position = position_nudge(y = -0.2)) +
  geom_segment(aes(x = Wc_lqr, xend = Wc_uqr),  size = 1.5, color = "grey30",
               linetype = 1, position = position_nudge(y = -0.2)) +
  geom_point(aes(x = Wc_median), size = 2, shape = 17,
             position = position_nudge(y = -0.2)) +
  fishualize::scale_color_fish(option = "Hypsypops_rubicundus",
                               trans = "log", breaks = c(0.5, 1, 2, 4)) +
  scale_shape_discrete(name = "", labels = c("gut", "feces")) +
  theme_custom() +
  labs(x = "C %", y = "", color = "feces:diet") 


ggplot(dat, aes(y = forcats::fct_rev(species), yend = forcats::fct_rev(species))) +
  geom_hline(aes(yintercept = forcats::fct_rev(species), color = (Rn_median)), 
             alpha = 0.3, size = 5, data = dat[dat$ndif2 == TRUE,]) +
  geom_segment(aes(x = Dn_lqn, xend = Dn_uqn), size = 0.5, color = "grey40",
               linetype = 1, position = position_nudge(y = 0.2)) +
  geom_segment(aes(x = Dn_lqr, xend = Dn_uqr), size = 1.5, color = "grey30",
               linetype = 1, position = position_nudge(y = 0.2)) +
  geom_point(aes(x = Dn_median), size = 2,
             position = position_nudge(y = 0.2)) +
  geom_segment(aes(x = Wn_lqn, xend = Wn_uqn), color = "grey40",
               linetype = 1, position = position_nudge(y = -0.2)) +
  geom_segment(aes(x = Wn_lqr, xend = Wn_uqr),  size = 1.5, color = "grey30",
               linetype = 1, position = position_nudge(y = -0.2)) +
  geom_point(aes(x = Wn_median), size = 2, shape = 17,
             position = position_nudge(y = -0.2)) +
  fishualize::scale_color_fish(option = "Hypsypops_rubicundus", 
                               trans = "log", breaks = c(0.5, 1, 2, 4)) +
  scale_shape_discrete(name = "", labels = c("gut", "feces")) +
  theme_custom() +
  labs(x = "N %", y = "", color = "feces:diet") 

ggplot(dat, aes(y = forcats::fct_rev(species), yend = forcats::fct_rev(species))) +
  geom_hline(aes(yintercept = forcats::fct_rev(species), color = (Rp_median)), 
             alpha = 0.3, size = 5, data = dat[dat$pdif2 == TRUE,]) +
  geom_segment(aes(x = Dp_lqn, xend = Dp_uqn), size = 0.5, color = "grey40",
               linetype = 1, position = position_nudge(y = 0.2)) +
  geom_segment(aes(x = Dp_lqr, xend = Dp_uqr), size = 1.5, color = "grey30",
               linetype = 1, position = position_nudge(y = 0.2)) +
  geom_point(aes(x = Dp_median), size = 2,
             position = position_nudge(y = 0.2)) +
  geom_segment(aes(x = Wp_lqn, xend = Wp_uqn), color = "grey40",
               linetype = 1, position = position_nudge(y = -0.2)) +
  geom_segment(aes(x = Wp_lqr, xend = Wp_uqr),  size = 1.5, color = "grey30",
               linetype = 1, position = position_nudge(y = -0.2)) +
  geom_point(aes(x = Wp_median), size = 2, shape = 17,
             position = position_nudge(y = -0.2)) +
  fishualize::scale_color_fish(option = "Hypsypops_rubicundus",
                               trans = "log", breaks = c(0.5, 1, 2, 4)) +
  scale_shape_discrete(name = "", labels = c("gut", "feces")) +
  theme_custom() +
  labs(x = "P %", y = "", color = "feces:diet") 


##### ash deron ######
library(tidyverse)
ash <- readr::read_csv("data/ash_summary_moorea.csv") %>%
  drop_na(fish_id) %>%
  drop_na(ash_percent, genus_species) %>%
  select(-1) %>%
  group_by(genus_species) %>%
  dplyr::mutate(n = n()) %>%
  filter(n>3) %>%
  mutate(species = gsub(" ", "_", genus_species)) %>%
  filter(species %in% result_cnp$species)

data <- drake::readd(result_cnp) %>%
  mutate(ash_fec = species %in% ash$species) %>%
  filter(location == "Moorea")

(sum(result_cnp$species %in% ash$species))/52

ggplot(ash) +
  geom_boxplot(aes(x = genus_species, y = ash_percent)) +
  coord_flip()




####### clust######
??clust
library(ggbiplot)
drake::loadd(result_ext)

dat <- dplyr::inner_join(dat, select(result_ext, species, diet)) %>%
  dplyr::mutate(diet2 = dplyr::case_when(
    diet %in% c(1, 5, 6) ~ "2_imix",
    diet == 3 ~ "3_cor",
    diet == 2 ~ "1_hmd",
    diet %in% c(7, 4) ~ "5_carn",
    diet == 8 ~ "4_plank"
  )) 

pca <- prcomp((dat[,c("Dc_median", "Dn_median", "Dp_median")]), center = TRUE, scale = TRUE)

ggbiplot(pca, labels = dat$species, 
         groups = as.character(dat$diet2), ellipse = TRUE)
ggbiplot(pca,  groups = dat$diet2, ellipse = TRUE)
shapiro.test((dat$Dc_median))
shapiro.test(dat$Dn_median)
shapiro.test(dat$Dp_median)


####### select samples #####
closestn <- function(x, m, n){
  sort((abs(x-m)))[1:n]
  sel <- x[which(abs(x-m) %in% sort((abs(x-m)))[1:n])] 
  x %in% sel}

samples <- read_csv("data/samples_18.csv") %>%
  filter(location == "Moorea") %>%
  mutate(species = gsub(" ", "_", species)) %>%
  filter(species %in% data$species) %>%
  filter(key %in% c("AE1", "AE2")) %>%
  filter(!(key == "AE2" & species %in% ash$species)) %>%
  drop_na(n) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(species, key) %>%
  dplyr::mutate(nrep = n(), n_median = median(n)) %>%
  dplyr::mutate(priority = closestn(n, unique(n_median), n = 5))

x <- sample(1:20, size = 10)
m = 10



x
closestn(x, 10, 3)



unique(samples$species)

ggplot(samples) +
  geom_boxplot(aes(x = species, y = n, color = key)) +
  coord_flip()

unique(c(ash$species, samples$species))
unique(c(samples$species))

sum(samples$priority)

sample_ash <- samples %>%
  select(key, id, species, nrep, priority) %>%
  arrange(key, id)
write_csv(sample_ash, "sample_selection_ash.csv")


##################
ggplot(result_ext) +
  geom_segment(aes(x = ap_lqn, xend = ap_uqn, y = species, yend = species)) +
  xlim(c(-0.5, 1))


sum <- data_ae %>%
  dplyr::select(species, location, n_c1, n_c2, n_n1, n_n2, n_p1, n_p2) %>%
  unique()
data <- dplyr::left_join(result_ext, sum)

ggplot(data) +
  geom_point(aes(x = Dn_mean, Dn_sd))

ggplot(data) +
  geom_point(aes(x = n_n1, Dn_sd))+
  scale_x_continuous(trans ="log") +
  scale_y_continuous(trans ="log") +
  theme_custom()  

ggplot(data) +
  geom_point(aes(x = n_p1, Dp_sd)) +
  scale_x_continuous(trans ="log") +
  scale_y_continuous(trans ="log") +
  theme_custom()

ggplot(data) +
  geom_point(aes(x = Dp_mean, Dp_sd, color = log(n_p1))) +
  scale_x_continuous(trans ="log") +
  scale_y_continuous(trans ="log") +
  theme_custom()

f <- lm(Dp_sd ~ log(n_p1) + Dp_mean, data = data)
summary(f)
data$test <- residuals(f)

ggplot(data) +
  geom_point(aes(x = test, y = species))

####### test ######
library(brms)

x1 <- data.frame(
  x = rnorm(100, 50, 10)
)
x2 <- data.frame(
  x = rnorm(50, 50, 10)
)
x3 <- data.frame(
  x = rnorm(20, 50, 10)
)

fit1 <- brm(x ~ 1, data = x1)
fit2 <- update(fit1, newdata = x2)
fit3 <- update(fit1, newdata = x3)
summary(fit1)
summary(fit2)
summary(fit3)

#### MAP #####


library(ggplot2)
library(ggmap)
library(ggrepel)

library(rgdal)
library(raster)
library(sf)

projcrs <- "+proj=longzlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# load data
data <- drake::readd(data_ae) %>%
  tidyr::drop_na(lat, long) %>%
  dplyr::filter(location == "Moorea") %>%
  dplyr::mutate(lat = dplyr::case_when(lat>0 ~ lat*-1, TRUE ~ lat),
         long = dplyr::case_when(long>0 ~ long*-1, TRUE ~ long)) %>%
  dplyr::select(location, sitename, lat, long, sitenumber, habitat) %>%
  dplyr::filter(!is.na(sitename)) %>%
  unique() %>%
  #correct error
  dplyr::mutate(lat = dplyr::case_when(
    lat == -17.47033 ~ -17.485,
    TRUE ~ lat
  ))


library(sf)
summary(coast)
st_crs(coast)

coast <- readOGR("data/Moorea_coastline.shp")
plot(coast)
coast@data$id = rownames(coast@data)
CRS.new <- CRS("+init=epsg:4326")
coast <- spTransform(coast, CRS.new)

coastline = fortify(coast, region="id")
coastline = plyr::join(coastline, coast@data, by="id")
coastline <- coastline[!coastline$group=="3.1",]

group <- unique(coastline$group)
g2 <- c("2","5","4","1","3")
gg <- data.frame(group,g2)
coastline <- dplyr::left_join(coastline, gg)

coastline <- coastline[order(coastline$g2, coastline$order), ]
coastline[coastline$g2 == "2", "order"] <- 195:1
coastline[coastline$g2 == "4", "order"] <- 52:1
coastline[coastline$g2 == "5", "order"] <- 93:1


coastline <- coastline[order(coastline$g2, coastline$order), ]

coastline$f <- 1:912

ggplot(coastline, aes(x=long, y = lat))+
  geom_polygon(aes(), size =3, fill = "grey") +
  geom_point(aes(x = long, y = lat), data = data, color = "blue") +
  coord_map()

coastline <- coastline[order(coastline$g2, coastline$order), ]

library(fishualize)
map <-
ggplot()+
  geom_polygon(aes(x = long, y = lat), data = coastline, fill = "grey")+
  geom_point(aes(x =long, y = lat, color = habitat),
             data = data[!is.na(data$sitenumber),],
             size = 3,
             alpha = 0.8) +
  scale_color_fish_d(end = 0.8) +
  theme_minimal() +
  theme_custom() +
  annotate(geom = "text", label = "Mo'orea", x = -149.835, y = -17.538,
           size = 8) +
  coord_map() +
  labs(x = "Longitude", y = "Latitude", color = "Habitat") +
  theme(legend.position = "top") +
  guides(color = guide_legend(title.position = 'top', 
                              override.aes = list(size = 4 )))

save_plot(map, "map_moorea", 8, 6)


map2 <- 
  ggplot()+
  geom_polygon(aes(x = long, y = lat), data = coastline, fill = "grey")+
  geom_point(aes(x =long, y = lat, color = habitat),
             data = data[!is.na(data$sitenumber),],
             size = 3,
             alpha = 0.9) +
  scale_color_manual(values = cols()[c(5,3,4)]) +
  theme_minimal() +
  annotate(geom = "text", label = "Mo'orea", x = -149.835, y = -17.538,
           size = 8) +
  coord_map() +
  labs(x = "Longitude", y = "Latitude", color = "Habitat") +
  theme(legend.position = "top") +
  guides(color = guide_legend(title.position = 'top', 
                              override.aes = list(size = 4 ))) +
  theme_ppt()
map2
save_plot(map2, "map_moorea", width = 14)

