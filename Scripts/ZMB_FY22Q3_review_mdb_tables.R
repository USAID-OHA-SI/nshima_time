# PROJECT: Zambia Q3 Data Reivew
# PURPOSE: Munge and Analysis of Q3 Data before MSD drops
# AUTHOR: Tim Essam | SI
# REF ID:   1ebbf63c
# LICENSE: MIT
# DATE: 2022-08-15
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(gt)
    library(selfdestructin5)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                               pattern = "Zambia-Daily-2022-08-15")
      
  # Grab metadata
   msd_source <- source_info(file_path)
   curr_pd <- source_info(file_path, return = "period")
   pd <- source_info(file_path, return = "period")
   fy <- source_info(file_path, return = "fiscal_year")
   qtr <- source_info(file_path, return = "quarter")  
  
  # REF ID for plots
    ref_id <- "1ebbf63c"
    
  # Functions  
    source("Scripts/fy22_partner_reivew_functions.R")
    
    swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
      # Using EQUIP as default as this has to be done each time in FY21
      .data %>%
        mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
               mech_name = ifelse(mech_code == {{mech2}}, "Action HIV", mech_name))
    }
  

# MDB TABLES PREP ============================================================================  
# MDB ---------------------------------------------------------------------
    
    psnu_im <- read_msd(file_path, convert_to_old_names = F)
    
    mdb_df   <- make_mdb_df(psnu_im)
    mdb_tbl  <- reshape_mdb_df(mdb_df, curr_pd)  
    
    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(psnu_im)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd) 
    
# Create Tables -----------------------------------------------------------
    

    mdb_tbl %>% 
      # filter(indicator != "GEND_GBV") %>%
      create_mdb(ou = "Zambia", type = "main", curr_pd, msd_source) %>% 
      gtsave(path = "Images", filename = glue::glue("Zambia_{curr_pd}_mdb_main.png"))  
    
    
    create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", pd, msd_source) %>% 
      bold_column(., Q3) %>% 
      embiggen() %>% 
      gtsave(., path = "Images", filename = glue::glue("{pd}_Zambia_MMD_VL_MD.png"))  
    
    mdb_tbl_zmb <- psnu_im %>%
      mutate(funding_agency = "USAID") %>% 
      make_mdb_df() %>% 
      reshape_mdb_df(curr_pd) %>% 
      filter(#indicator != "GEND_GBV",
        operatingunit == "Zambia") %>% 
      mutate(agency = "PEPFAR")
    
    mdb_tbl_zmb %>% 
      gt(groupname_col = "agency") %>%
      mdb_main_theme(curr_pd, msd_source) %>% 
      tab_header(
        title = "ZAMBIA PEPFAR PERFORMANCE SUMMARY") %>% 
      gtsave(path = "Images", filename = glue::glue("Zambia_PEPFAR_{curr_pd}_mdb_main.png"))
    

# PARTNER TABLE? ----------------------------------------------------------

  mk_ptr_tbl <- function(df, mech_id)  {
  
    ip_mdb <- 
      df %>% 
      filter(mech_code == mech_id) %>% 
      make_mdb_df() %>% 
      reshape_mdb_df(., curr_pd) 
    
   mech_name <-  
     df %>% 
      filter(mech_code == mech_id) %>%
      distinct(mech_name) %>% 
      pull(mech_name)
    
    
  ip_mdb %>%   
    create_mdb(ou = "Zambia", type = "main", curr_pd, msd_source) %>% 
    tab_header(
      title = glue::glue("{mech_name} PERFORMANCE SUMMARY")
      ) %>% 
    gtsave(path = "Images", filename = glue::glue("{mech_name}_mdb_main.png"))
}

    mk_ptr_tbl(psnu_im %>% swap_targets(), 82075) 
    mk_ptr_tbl(psnu_im, 17413)
    mk_ptr_tbl(psnu_im %>% 
                 mutate(mech_name = ifelse(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)), 17399)
    
    
             