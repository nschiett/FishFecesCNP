#### test ####
A <- matrix(ncol = 3, nrow = 3,
            data = c(NA, 2, 3,
                     NA, NA, 2,
                     NA, NA, NA))
A

colnames(A) <-  c("sp1", "sp2", "sp3")
rownames(A) <-  c("sp1", "sp2", "sp3")

species1 <- data.frame(
  species1 = colnames(A),
  x1 = 1:3,
  y1 = rep(4, 3))
species2 <- data.frame(
  species2 = colnames(A),
  x2 = 1:3,
  y2 = rep(2, 3))

d <- as.data.frame(A) %>%
  mutate(species2 = colnames(A)) %>%
  pivot_longer(cols = 1:nrow(A), names_to = "species1") %>%
  left_join(species1) %>%
  left_join(species2)

ggplot(d) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, size = value)) +
  geom_point(aes(x = x1, y = y1), size = 10, color = "grey") +
  geom_point(aes(x = x2, y = y2), size = 10, color = "grey") +
  geom_text(aes(x = x1, y = y1, label = species1), 
            size = 5, angle = 90, hjust = -1) +
  geom_text(aes(x = x2, y = y2, label = species2), 
            size = 5, angle = 90, hjust = 2) +
  ylim(c(1,5)) +
  theme_void()

##### c #####

library(tidyverse)
drake::loadd(result_ext)

result_cnp <- filter(result_ext, location == "Moorea")

oc <- outer(result_cnp$Wc_median, result_cnp$Dc_median, "-") 
oc2 <- outer(result_cnp$Wc_lqr, result_cnp$Dc_uqr, ">") 
oc <- oc*oc2

colnames(oc) <-  result_cnp$species
rownames(oc) <-  result_cnp$species
oc <- as.data.frame(oc) %>%
  mutate(sp_feces = result_cnp$species) %>%
  pivot_longer(cols = 1:51, names_to = "sp_diet") %>%
  mutate(value = case_when(value <= 0 ~ NA_real_,
                           sp_feces == sp_diet ~ NA_real_,
                           TRUE ~ value))


ggplot(oc) +
  geom_tile(aes(x = sp_feces, y = sp_diet, fill = value), na.rm = TRUE) +
  fishualize::scale_fill_fish(option = "Trimma_lantana", name = "C diff") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
  

 species <- select(result_cnp, species, Dc_median, diet) %>%
   dplyr::mutate(diet2 = dplyr::case_when(
     diet %in% c(1, 3, 5, 6) ~ "2_imix",
     diet == 2 ~ "1_hmd",
     diet %in% c(7, 4) ~ "4_carn",
     diet == 8 ~ "3_plank"
   )) %>%
  arrange(diet2, Dc_median) %>%
   mutate(x = 1:n()) 
 
 

species1 <- species %>% 
  select(sp_diet = species,
         x1 = x, diet2) %>%
  mutate(y1 = rep(4, nrow(species)))
        
         
species2 <- species %>% 
  select(sp_feces = species,
         x2 = x) %>%
  mutate(y2 = rep(2, nrow(species)))
 

oc <- oc %>% left_join(species1) %>% left_join(species2) %>%
  drop_na(value)

nlinks1 <- oc %>% 
  group_by(sp_diet) %>%
  summarise(n = n())

species1 <- left_join(species1, nlinks1) %>%
  mutate(n = replace_na(n, 0))

nlinks2 <- oc %>% 
  group_by(sp_feces) %>%
  summarise(n = n())

species2 <- left_join(species2, nlinks2) %>%
  mutate(n = replace_na(n, 0)) %>%
  left_join(select(species1, sp_diet, diet2), by = c("sp_feces"= "sp_diet"))

p1 <-
ggplot(oc) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = (value), alpha = value), 
               size = 1) +
  geom_point(aes(x = x1, y = y1, shape = diet2, size = n, fill = n), 
              color = "grey", fill = "grey", data = species1) +
  geom_point(aes(x = x2, y = y2, shape = diet2, size = n), 
             color = "grey", fill = "grey",  data = species2) +  
  geom_text(aes(x = x1, y = y1+0.1, label = sp_diet),
            size = 3, angle = 90, vjust = 0.5, hjust = 0,
            data = species1) +
  geom_text(aes(x = x2, y = y2-0.1, label = sp_feces), 
            size = 3, angle = 90, vjust = 0.5, hjust = 1,
            data = species2) +
  scale_size_continuous(range = c(1, 5), guide = FALSE) +
  scale_alpha_continuous(guide = FALSE) +
  ylim(c(0,6)) +
  fishualize::scale_color_fish(option = "Trimma_lantana", name = "delta C%") +
  scale_shape_manual(values = 21:24, name = "diet") +
  theme_void()


##### n #####
on <- outer(result_cnp$Wn_median, result_cnp$Dn_median, "-") 
on2 <- outer(result_cnp$Wn_lqr, result_cnp$Dn_uqr, ">") 
on <- on*on2

colnames(on) <-  result_cnp$species
rownames(on) <-  result_cnp$species
on <- as.data.frame(on) %>%
  mutate(sp_feces = result_cnp$species) %>%
  pivot_longer(cols = 1:51, names_to = "sp_diet") %>%
  mutate(value = case_when(value <= 0 ~ NA_real_,
                           sp_feces == sp_diet ~ NA_real_,
                           TRUE ~ value))

on <- mutate(on,
             sp_feces = as.factor(sp_feces),
             sp_diet = as.factor(sp_diet)) %>%
  mutate(sp_feces =  fct_relevel(sp_feces, species2$sp_feces),
         sp_diet =  fct_relevel(sp_diet, species2$sp_feces))
ggplot(on) +
  geom_tile(aes(x = (sp_feces), 
                y = fct_rev(sp_diet), 
                fill = value)) +
  fishualize::scale_fill_fish(option = "Trimma_lantana", 
                              name = "N diff (%)", na.value = "white") +
  theme_bw() + 
  facet_grid(diet2~., scales = "free_y", 
             space = "free_y", switch = "y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title = element_blank())  



species <- select(result_cnp, species, Dn_median, diet) %>%
  dplyr::mutate(diet2 = dplyr::case_when(
    diet %in% c(1, 3, 5, 6) ~ "2_imix",
    diet == 2 ~ "1_hmd",
    diet %in% c(7, 4) ~ "4_carn",
    diet == 8 ~ "3_plank"
  )) %>%
  arrange(diet2, Dn_median) %>%
  mutate(x = 1:n()) 

species1 <- species %>% 
  select(sp_diet = species,
         x1 = x, diet2) %>%
  mutate(y1 = rep(4, nrow(species)))

species2 <- species %>% 
  select(sp_feces = species,
         x2 = x) %>%
  mutate(y2 = rep(2, nrow(species)))


on <- on %>% left_join(species1) %>% left_join(species2) %>%
  drop_na(value)

nlinks1 <- on %>% 
  group_by(sp_diet) %>%
  summarise(n = n())

species1 <- left_join(species1, nlinks1) %>%
  mutate(n = replace_na(n, 0))

nlinks2 <- on %>% 
  group_by(sp_feces) %>%
  summarise(n = n())

species2 <- left_join(species2, nlinks2) %>%
  mutate(n = replace_na(n, 0)) %>%
  left_join(select(species1, sp_diet, diet2), by = c("sp_feces"= "sp_diet"))

p2 <-
ggplot(on) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = (value), alpha = value), 
               size = 1) +
  geom_point(aes(x = x1, y = y1, shape = diet2, size = n, fill = n), 
             color = "grey", fill = "grey", data = species1) +
  geom_point(aes(x = x2, y = y2, shape = diet2, size = n), 
             color = "grey", fill = "grey",  data = species2) +  
  geom_text(aes(x = x1, y = y1+0.1, label = sp_diet),
            size = 3, angle = 90, vjust = 0.5, hjust = 0,
            data = species1) +
  geom_text(aes(x = x2, y = y2-0.1, label = sp_feces), 
            size = 3, angle = 90, vjust = 0.5, hjust = 1,
            data = species2) +
  scale_size_continuous(range = c(1, 5), guide = FALSE) +
  scale_alpha_continuous(guide = FALSE) +
  ylim(c(0,6)) +
  fishualize::scale_color_fish(option = "Trimma_lantana", name = "delta N%") +
  scale_shape_manual(values = 21:24, name = "diet") +
  theme_void()


##### p #####
op <- outer(result_cnp$Wp_median, result_cnp$Dp_median, "-") 
op2 <- outer(result_cnp$Wp_lqn, result_cnp$Dp_uqn, ">") 
op <- op*op2

colnames(op) <-  result_cnp$species
rownames(op) <-  result_cnp$species
op <- as.data.frame(op) %>%
  mutate(sp_feces = result_cnp$species) %>%
  pivot_longer(cols = 1:51, names_to = "sp_diet") %>%
  mutate(value = case_when(value <= 0 ~ NA_real_,
                           sp_feces == sp_diet ~ NA_real_,
                           TRUE ~ value)) 
  


species <- select(result_cnp, species, Dp_median, diet) %>%
  dplyr::mutate(diet2 = dplyr::case_when(
    diet %in% c(1, 3, 5, 6) ~ "2_imix",
    diet == 2 ~ "1_hmd",
    diet %in% c(7, 4) ~ "4_carn",
    diet == 8 ~ "3_plank"
  )) %>%
  arrange(diet2, Dp_median) %>%
  mutate(x = 1:n()) 

species1 <- species %>% 
  select(sp_diet = species,
         x1 = x, diet2) %>%
  mutate(y1 = rep(4, nrow(species)))


species2 <- species %>% 
  select(sp_feces = species,
         x2 = x) %>%
  mutate(y2 = rep(2, nrow(species)))


op <- op %>% left_join(species1) %>% left_join(species2) %>%
  drop_na(value)

nlinks1 <- op %>% 
  group_by(sp_diet) %>%
  summarise(n = n())

species1 <- left_join(species1, nlinks1) %>%
  mutate(n = replace_na(n, 0))

nlinks2 <- op %>% 
  group_by(sp_feces) %>%
  summarise(n = n())

species2 <- left_join(species2, nlinks2) %>%
  mutate(n = replace_na(n, 0)) %>%
  left_join(select(species1, sp_diet, diet2), by = c("sp_feces"= "sp_diet"))

p3 <-
ggplot(op) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = (value), alpha = value), 
               size = 1) +
  geom_point(aes(x = x1, y = y1, shape = diet2, size = n, fill = n), 
             color = "grey", fill = "grey", data = species1) +
  geom_point(aes(x = x2, y = y2, shape = diet2, size = n), 
             color = "grey", fill = "grey",  data = species2) +  
  geom_text(aes(x = x1, y = y1+0.1, label = sp_diet),
            size = 3, angle = 90, vjust = 0.5, hjust = 0,
            data = species1) +
  geom_text(aes(x = x2, y = y2-0.1, label = sp_feces), 
            size = 3, angle = 90, vjust = 0.5, hjust = 1,
            data = species2) +
  scale_size_continuous(range = c(1, 5), guide = FALSE) +
  scale_alpha_continuous(guide = FALSE) +
  ylim(c(0,6)) +
  fishualize::scale_color_fish(option = "Trimma_lantana", name = "delta P%") +
  scale_shape_manual(values = 21:24, name = "diet") +
  theme_void()
p3


library(patchwork)

p <- p1 + p2 + p3 + plot_layout(nrow = 3)

ggsave( "copr_links.pdf", p, height = 16, width = 12)



x <- rnorm(100, 0.6, 0.01)


summary(x)
sd(x)

hist(x)
hist(0.7*x)
sd(0.7*x)
0.7*sd(x)
summary(1-(0.7*x))
1-(0.7*summary(x))

