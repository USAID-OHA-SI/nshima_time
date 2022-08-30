# PROJECT: BOBs Indicator Review
# PURPOSE: Munge and Analysis of BOBs indicators
# AUTHOR: Tim Essam | SI
# REF ID:   f938c4ca
# LICENSE: MIT
# DATE: 2022-08-29
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
   library(gagglr)
   library(tidyverse)
   library(readxl)
   library(extrafont)


  # REF ID for plots
    ref_id <- "f938c4ca"
  

# LOAD DATA ============================================================================  

  bob_surge_path <- file.path("../../../Downloads/BOB_Peds Surge_2022_revised 03-25-2022.xlsx")
  bob_path <- file.path("../../../Downloads/FY22 bob template 06082022.xlsx")
    
  df_tab1 <- read_excel(bob_surge_path, skip = 3)
  
  df_tab4 <- read_excel(bob_surge_path, sheet = "Site Level Tracer Indicators", skip = 6)
  names(df_tab4) %>% as.data.frame()
  
  df_bob <- read_excel(bob_path) %>% 
    filter(!is.na(Indicator)) %>% 
    janitor::clean_names()
  
  df_indic <- df_bob %>% 
    count(indicator_group, indicator)
  
  # Loop over each indicator group and write as separate CSV for vizzzzzing
  df_indic %>% 
    mutate(indic = indicator_group) %>% 
    nest(., data = -indic) %>% 
    pwalk(~write_csv(x = .y, path = paste0("Dataout/", .x, ".csv")))
    
  df_indic %>% 
    write_csv(., "Dataout/bobs_indicators.csv")
    
  
   
# VIZ ============================================================================
  
  # show how ridiculous this is
  df_bob %>% 
    filter(indicator_group == "Testing") %>% 
    ggplot(aes(y = indicator, x = 0)) +
    geom_text(aes(label = indicator), size = 7/.pt, hjust = 0, 
              family = "Source Sans Pro Light") +
    facet_wrap(~indicator_group, 
               scales = "free") +
    theme(axis.text.y = element_blank()) +
    si_style_void() +
    scale_x_continuous(limits = c(0, 1))
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

