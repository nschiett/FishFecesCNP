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

species <- result_cnp %>%
  select(species, diet2) %>%
  unique() %>%
  arrange(diet2, species)

sp1 <- species %>%
  rename(sp_diet = species, diet1 = diet2)
sp2 <- species %>%
  rename(sp_feces = species)

oc <- oc %>% left_join(sp1) %>% left_join(sp2) %>%
  mutate(sp_diet = as.factor(sp_diet),
         sp_feces = as.factor(sp_feces)) %>%
  mutate(sp_diet = fct_relevel(sp_diet, species$species),
         sp_feces = fct_relevel(sp_feces, species$species))
c <-
ggplot(oc) +
  geom_tile(aes(x = (sp_feces),
                y = fct_rev(sp_diet),
                fill = value)) +
  fishualize::scale_fill_fish(option = "Trimma_lantana",
                              name = "C diff (%)", na.value = NA,
                              trans = "sqrt") +
  theme_bw() +
  facet_grid(diet1~diet2, scales = "free",
             space = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_blank(), 
        axis.title = element_blank()) 
c
##### N #####
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

species <- result_cnp %>%
  select(species, diet2) %>%
  unique() %>%
  arrange(diet2, species)

sp1 <- species %>%
  rename(sp_diet = species, diet1 = diet2)
sp2 <- species %>%
  rename(sp_feces = species)

on <- on %>% left_join(sp1) %>% left_join(sp2) %>%
  mutate(sp_diet = as.factor(sp_diet),
         sp_feces = as.factor(sp_feces)) %>%
  mutate(sp_diet = fct_relevel(sp_diet, species$species),
         sp_feces = fct_relevel(sp_feces, species$species))
n <- 
ggplot(on) +
  geom_tile(aes(x = (sp_feces),
                y = fct_rev(sp_diet),
                fill = value)) +
  fishualize::scale_fill_fish(option = "Trimma_lantana",
                              name = "N diff (%)", na.value = NA,
                              trans = "sqrt") +
  theme_bw() +
  facet_grid(diet1~diet2, scales = "free",
             space = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y = "diet")
n
##### P #######
op <- outer(result_cnp$Wp_median, result_cnp$Dp_median, "-") 
op2 <- outer(result_cnp$Wp_lqr, result_cnp$Dp_uqr, ">") 
op <- op*op2


colnames(op) <-  result_cnp$species
rownames(op) <-  result_cnp$species
op <- as.data.frame(op) %>%
  mutate(sp_feces = result_cnp$species) %>%
  pivot_longer(cols = 1:51, names_to = "sp_diet") %>%
  mutate(value = case_when(value <= 0 ~ NA_real_,
                           sp_feces == sp_diet ~ NA_real_,
                           TRUE ~ value))

species <- result_cnp %>%
  select(species, diet2) %>%
  unique() %>%
  arrange(diet2, species)

sp1 <- species %>%
  rename(sp_diet = species, diet1 = diet2)
sp2 <- species %>%
  rename(sp_feces = species)

op <- op %>% left_join(sp1) %>% left_join(sp2) %>%
  mutate(sp_diet = as.factor(sp_diet),
         sp_feces = as.factor(sp_feces)) %>%
  mutate(sp_diet = fct_relevel(sp_diet, species$species),
         sp_feces = fct_relevel(sp_feces, species$species))

p <-
ggplot(op) +
  geom_tile(aes(x = (sp_feces),
                y = fct_rev(sp_diet),
                fill = value)) +
  fishualize::scale_fill_fish(option = "Trimma_lantana",
                              name = "P diff (%)", na.value = NA,
                              trans = "sqrt") +
  theme_bw() +
  facet_grid(diet1~diet2, scales = "free",
             space = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_blank())+
  labs(x = "feces")
p
library(patchwork)
plot <-
n + p + plot_layout(ncol = 1) & theme(strip.placement = NULL)
plot
ggsave("output/plots/diet_poo_pairwise.png", plot, height = 18, width = 12)




