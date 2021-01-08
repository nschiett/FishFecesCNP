
theme_custom <- function(){

    font <- "Helvetica"   #assign font family up front
    
    theme_bw() %+replace%    #replace elements we want to change
      
      theme(
        
        #grid elements
        #panel.grid.major = element_blank(),    #strip major gridlines
        #panel.grid.minor = element_blank(),    #strip minor gridlines
        #axis.ticks = element_blank(),          #strip axis ticks
        
        #since theme_minimal() already strips axis lines, 
        #we don't need to do that again
        
        #text elements
        plot.title = element_text(             #title
          family = font,            #set font family
          size = 20,                #set font size
          face = 'bold',            #bold typeface
          hjust = 0,                #left align
          vjust = 2,
          color = "black"),               #raise slightly
        
        plot.subtitle = element_text(          #subtitle
          family = font,            #font family
          size = 10,
          color = "black"),               #font size
        
        plot.caption = element_text(           #caption
          family = font,            #font family
          size = 9,                 #font size
          hjust = 1,
          color = "black"),               #right align
        
        axis.title = element_text(             #axis titles
          family = font,            #font family
          size = 12,
          color = "black"),               #font size
        
        axis.text = element_text(              #axis text
          family = font,            #axis famuly
          size = 10,
          color = "black"),                #font size
        
        axis.text.x = element_text(            #margin for axis text
          margin = margin(5, b = 10)),
        
        legend.text = element_text(
          family = font,
          size = 10
        ),
        legend.title = element_text(
          family = font,
          size = 12
        )
    
      )
}

#' Title
#'
#' @param plot 
#' @param name 
#' @param width 
#' @param height 
#'
#' @return
#' @export
#'
#' @examples
save_plot <- function(plot, name, width = 8, height = 6){
  
  ggsave(plot, filename = paste0("output/plots/", name, ".png"), 
         width = width, height = height)
}

#' fig 1
#'
#' @param data 
#'
#' @return
#' @export
#' 
#' @import ggplot2
#' @import fishualize
#' @import patchwork
#'
#' @examples
make_fig1 <- function(data){
  
  data_sub <- data %>%
    dplyr::group_by(family) %>%
    dplyr::mutate(nfam = dplyr::n()) %>%
    dplyr::filter(nfam > 1)
  
  p1 <-
  ggplot(data) +
    geom_abline(slope = 0.3, alpha = 0.5) +
    geom_abline(slope = 0.5, alpha = 0.5) +
    geom_abline(slope = 0.7, alpha = 0.5) +
    geom_abline(slope = 0.9, alpha = 0.5) +
    geom_label(aes(x = 50, y = 0.3 * 50, label = "AE = 0.7")) +
    geom_label(aes(x = 50, y = 0.5 * 50, label = "AE = 0.5")) +
    geom_label(aes(x = 50, y = 0.7 * 50, label = "AE = 0.3")) +
    geom_label(aes(x = 50, y = 0.9 * 50, label = "AE = 0.1")) +
    geom_point(aes(x = c_mu1_m, y = c_mu2_m ),
               size = 4, color = "lightgrey") +
    geom_point(aes(x = c_mu1_m, y = c_mu2_m, color =  family),
               size = 4, data = data_sub) +
    # geom_text_repel(aes(x = c_mu1_m, y = c_mu2_m, label = species, color = family), 
    #                 size = 3, alpha = 0.7) +
    theme_custom() +
    labs(x = "Gut content C%", y = "Feces C%", color = "Family") +
    theme(legend.position = "none") +
    scale_color_fish_d(option = "Pseudocheilinus_tetrataenia")
  
  p2 <-
    ggplot(data) +
    geom_abline(slope = 0.3, alpha = 0.5) +
    geom_abline(slope = 0.5, alpha = 0.5) +
    geom_abline(slope = 0.7, alpha = 0.5) +
    geom_abline(slope = 0.9, alpha = 0.5) +
    geom_label(aes(x = 13, y = 0.3 * 13, label = "AE = 0.7")) +
    geom_label(aes(x = 13, y = 0.5 * 13, label = "AE = 0.5")) +
    geom_label(aes(x = 13, y = 0.7 * 13, label = "AE = 0.3")) +
    geom_label(aes(x = 13, y = 0.9 * 13, label = "AE = 0.1")) +
    geom_point(aes(x = n_mu1_m, y = n_mu2_m ),
               size = 4, color = "lightgrey") +
    geom_point(aes(x = n_mu1_m, y = n_mu2_m, color =  family),
               size = 4, data = data_sub) +
    # geom_text_repel(aes(x = n_mu1_m, y = n_mu2_m, label = species, color = family), 
    #                 size = 3, alpha = 0.7) +
    theme_custom() +
    labs(x = "Gut content N%", y = "Feces N%", color = "Family")+
    theme(legend.position = "none") +
    scale_color_fish_d(option = "Pseudocheilinus_tetrataenia")
  
  
 
  p3 <-
    ggplot(data) +
    geom_abline(slope = 0.3, alpha = 0.5) +
    geom_abline(slope = 0.5, alpha = 0.5) +
    geom_abline(slope = 0.7, alpha = 0.5) +
    geom_abline(slope = 0.9, alpha = 0.5) +
    geom_label(aes(x = 2.3, y = 0.3 * 2.5, label = "AE = 0.7")) +
    geom_label(aes(x = 2.3, y = 0.5 * 2.5, label = "AE = 0.5")) +
    geom_label(aes(x = 2.3, y = 0.7 * 2.5, label = "AE = 0.3")) +
    geom_label(aes(x = 2.3, y = 0.9 * 2.5, label = "AE = 0.1")) +
    geom_point(aes(x = p_mu1_m, y = p_mu2_m ),
               size = 4, color = "lightgrey") +
    geom_point(aes(x = p_mu1_m, y = p_mu2_m, color =  family),
               size = 4, data = data_sub) +
    # geom_text_repel(aes(x = p_mu1_m, y = p_mu2_m, label = species, color = dn),
    #                 size = 3, alpha = 0.7) +
    theme_custom() +
    labs(x = "Gut content P%", y = "Feces P%", color = "") +
    theme(legend.position = "bottom") +
    scale_color_fish_d(option = "Pseudocheilinus_tetrataenia")
  
  
  plot <-
  p1 + p2 + p3 +
    plot_layout(nrow = 3)
  
  return(plot)
  
}

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#' 
#' @import ggplot2
#' @import fishualize
#' @import ggrepel
#' @import patchwork
#'
#' @examples
make_figs1 <- function(data){
 
  ## with errorbars
  
  p1 <-
  ggplot(data) +
    geom_abline(slope = 0.3, alpha = 0.7) +
    geom_abline(slope = 0.5, alpha = 0.7) +
    geom_abline(slope = 0.7, alpha = 0.7) +
    geom_abline(slope = 0.9, alpha = 0.7) +
    geom_point(aes(x = c_mu1_m, y = c_mu2_m),
               size = 3) +
    geom_text_repel(aes(x = c_mu1_m, y = c_mu2_m, label = species),
                    size = 3, alpha = 0.8, color = "black") +
    theme_custom() +
    labs(x = "Gut content C%", y = "Feces C%", color = "Trophic guild")
  
  p2 <-
    ggplot(data) +
    geom_abline(slope = 0.3, alpha = 0.7) +
    geom_abline(slope = 0.5, alpha = 0.7) +
    geom_abline(slope = 0.7, alpha = 0.7) +
    geom_abline(slope = 0.9, alpha = 0.7) +
    geom_point(aes(x = n_mu1_m, y = n_mu2_m),
               size = 3) +
    geom_text_repel(aes(x = n_mu1_m, y = n_mu2_m, label = species),
                    size = 3, alpha = 0.8, color = "black") +
    theme_custom() +
    labs(x = "Gut content N%", y = "Feces N%")
  
  p3 <-
    ggplot(data) +
    geom_abline(slope = 0.3, alpha = 0.7) +
    geom_abline(slope = 0.5, alpha = 0.7) +
    geom_abline(slope = 0.7, alpha = 0.7) +
    geom_abline(slope = 0.9, alpha = 0.7) +
    geom_point(aes(x = p_mu1_m, y = p_mu2_m),
               size = 3) +
    geom_text_repel(aes(x = p_mu1_m, y = p_mu2_m, label = species),
                    size = 3, alpha = 0.8, color = "black") +
    theme_custom() +
    labs(x = "Gut content P%", y = "Feces P%")

  plot <-
  p1 + p2 + p3 +
    plot_layout(nrow = 3)
  
  return(plot)
}


#' Title
#'
#' @param data 
#'
#' @return
#' @export
#' 
#' @import ggplot2
#' @import fishualize
#' @import ggstance
#' @import patchwork
#'
#' @examples
make_figs2 <- function(data){
  
  data_long <- data %>%
    tidyr::pivot_longer(cols = 3:56, names_sep = "_", names_to = c("element", "key", "type")) %>%
    tidyr::pivot_wider(names_from = type, values_from = value) %>%
    dplyr::group_by(species, element) %>%
    dplyr::mutate(a_m = m[key == "a"]) %>%
    dplyr::filter(key %in% c("mu1", "mu2")) %>%
    dplyr::ungroup() 
  
  p1 <- 
    ggplot(data_long) +
    geom_hline(aes(yintercept = forcats::fct_rev(species), color = a_m), alpha = 0.5, size = 4) +
    geom_errorbarh(aes(y = forcats::fct_rev(species), xmin = `25`, xmax = `75`, linetype = key), width = 0) +
    geom_point(aes(x = m, y = forcats::fct_rev(species), shape = key), size = 2) +
    facet_wrap(~element, scales = "free_x") +
    scale_color_fish(option = "Hypsypops_rubicundus") +
    scale_shape_discrete(name = "", labels = c("gut", "feces")) +
    scale_linetype_discrete(name = "", labels = c("gut", "feces")) +
    theme_custom() +
    labs(x = "% content", y = "", color = "AE") 
  
  p1
  
}


#' Title
#'
#' @param model_ae_diet 
#'
#' @return
#' @export
#' @import ggplot2
#' @import fishualize
#' @import patchwork
#'
make_fig2 <- function(model_ae_diet) {

  pred_data <- model_ae_diet[[2]]
  data <- model_ae_diet[[1]]$data
  
  a <- 
  ggplot(pred_data) +
    geom_ribbon(aes(x = mu1_st, ymin= y_a_lb, ymax = y_a_ub, fill = element), 
                alpha = 0.3) +
    geom_line(aes(x = mu1_st, y = y_a_m, color = element), size = 1) +
    geom_point(aes(x = mu1_st, y = ae, color = element), data = data) +
    scale_color_fish_d(end = 0.8) + scale_fill_fish_d(end = 0.8) +
    labs(y = "Assimilation efficiency", x = "Standardised gut content %", 
         color = "", fill = "") +
    theme_custom() +
    theme(legend.position = "top")

  b <-
  ggplot(pred_data) +
    geom_ribbon(aes(x = dn, ymin= y_mu1_lb, ymax = y_mu1_ub, fill = element), 
                alpha = 0.3) +
    geom_line(aes(x = dn, y = y_mu1_m, color = element), size = 1) +
    geom_point(aes(x = dn, y = mu1_st, color = element), data = data) +
    scale_color_fish_d(end = 0.8) + scale_fill_fish_d(end = 0.8) +
    labs(x = "%dN", y = "Standardised gut content %") +
    theme_custom() + theme(legend.position = "none")
  
  a + b + plot_layout(nrow = 2)
  
  }
