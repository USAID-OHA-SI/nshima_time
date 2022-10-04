# PROJECT: Visualize Portfolio Coverage by mechanism
# PURPOSE: Munge and Analysis of portfolio data
# AUTHOR: Tim Essam | SI
# REF ID:   e7a70610
# LICENSE: MIT
# DATE: 2022-10-04
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(googlesheets4)
    library(tidytext)
    
    
  # SI specific paths/functions  
    load_secrets()
      
  # Grab metadata
  # cascade::get_file_metadata()
  
  # REF ID for plots
    ref_id <- "e7a70610"
    
  # Functions  
  link <- "1uPFL57VxyaZaAnsLrVPQmtkc2jAW2Stn3A3ftovj52k"

# LOAD DATA ============================================================================  

  df <- read_sheet(ss = link) %>% 
    mutate(pcode = as.factor(si_poc))

# MUNGE ============================================================================
  # Partner order based on TX_CURR targets
  
  df_viz <- 
    df %>% 
    mutate(sort_var = case_when(
      indic_type == "prevention" ~ 1, 
      indic_type == "testing" ~ 2, 
      indic_type == "treatment" ~ 3,
      ),
      indic_order = reorder_within(indicator, sort_var, targets),
      mech_order = fct_reorder(mech_name, targets)) 
  
  df_viz %>% 
    filter(partner_type == "C&T") %>% 
    ggplot(aes(y = indic_order, x = pcode)) +
    geom_tile(aes(fill = si_poc), color = "white") +
    facet_wrap(~mech_name, scales = "free_y") +
    scale_y_reordered() +
    si_style(facet_space = 0.5, text_scale = 0.75)
  
  # Need to complete the dataframe around mech_code + indicator + si_poc
  df_viz %>% 
    #group_by(indic_type) %>% 
    # complete(indicator, mech_code) %>%
    # ungroup() %>% View()
    ggplot(aes(y = indicator, x = as.factor(mech_code))) +
    geom_tile(aes(fill = si_poc), color = "white") +
    #geom_point(aes(color = si_poc, size = targets), shape = 15) +
    si_style(facet_space = 0.5, text_scale = 0.75) +
    scale_fill_viridis_d(na.value = grey10k) +
    facet_grid(indic_type ~ partner_type, scales = "free", space = "fixed") +
    si_style_nolines(facet_space = 0.5, text_scale = 0.75) 
    
  
  
  # IDEA is this 
  # ALL BOXES ARE GRAYed out
  # REVEAL one-by one where each SI advisor falls in the composition
  # SHOW each staff's footprint one column at a time
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

