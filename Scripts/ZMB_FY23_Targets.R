# PROJECT: Pull all mechs/indicators for which targets are reported
# PURPOSE: Munge and Analysis of PSNU X IM
# AUTHOR: Tim Essam | SI
# REF ID:   38db0514
# LICENSE: MIT
# DATE: 2022-09-16
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(tidyverse)
  
  
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "PSNU_IM_FY20-23_20220812_v1_1_Zambia.zip")
  
  # REF ID for plots
  ref_id <- "38db0514"
  
  # Functions  
  cascade::get_file_metadata(file_path)


# PULL TARGETS ------------------------------------------------------------

  df <- read_msd(file_path)  
  
  df_targ <- df %>% 
    filter(fiscal_year == 2023, 
           standardizeddisaggregate == "Total Numerator", 
           funding_agency == "USAID") %>% 
    group_by(mech_name, mech_code, indicator) %>% 
    summarise(across(matches("targets"), sum, na.rm = T)) 

  # Write this to a google sheet for updating by team
  gd_id <- "1uPFL57VxyaZaAnsLrVPQmtkc2jAW2Stn3A3ftovj52k"
  
  googlesheets4::write_sheet(df_targ, ss = gd_id, sheet = "targets")
  
  # Can we make a target table of the possible disaggregates associated with mechs
  # for Peds, AYP, KP and DREAMS
  
  
  