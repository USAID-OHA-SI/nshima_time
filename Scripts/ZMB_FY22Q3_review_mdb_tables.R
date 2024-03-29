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
    library(gtExtras)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    # file_path <- return_latest(folderpath = merdata,
    #                            pattern = "Zambia-Daily-2022-08-15")
    file_path <- return_latest(folderpath = merdata, 
                               pattern = "PSNU_IM_FY20-23_20220812")
      
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
 
    

# ADD CUSTOM TABLE FOR ZAMHEALTH ------------------------------------------

       
    mk_ptr_tbl(psnu_im, 82086)
    
    
    ip_mdb <- 
      psnu_im %>% 
      filter(mech_code == 82086) %>% 
      make_mdb_df() %>% 
      filter(fiscal_year == fy, operatingunit == "Global") %>% 
      select(agency, indicator, indicator_plain, qtr1, qtr2, qtr3, cumulative, targets) %>% 
      mutate(achv = cumulative / targets)
    
    ip_mdb %>% 
      gt(groupname_col ="agency") %>% 
      gt_merge_stack(col1 = indicator, col2 = indicator_plain,
                     palette = c(grey90k, grey80k),
                     font_size = c("12px", "11px"),
                     font_weight = c("normal", "normal")) %>% 
      cols_align(columns = 2, 
                 align = c("left")) %>% 
      cols_label(
        indicator = ""
      ) %>% 
      fmt_number(
        columns = 4:8, 
        decimals = 0
      ) %>% 
      fmt_percent(
        columns = 9,
        decimals = 0
      ) %>% 
      tab_header(title = md("Zambia Accessible Markets for Health Performance Summary")) %>% 
      tab_source_note(
        source_note = gt::md(glue::glue("**Source**: {authors_footnote(msd_source)} | si.coreanalytics@usaid.gov"))
      ) %>% 
      gt::tab_style(
        style = list("font-variant: small-caps;"),
        locations = gt::cells_column_labels(columns = tidyselect::everything())
      ) %>% 
      tab_options(
        source_notes.font.size = 8,
        source_notes.padding = px(1),
        data_row.padding = px(5),
        table.font.size = 13
      ) %>% 
      tab_spanner(
        label = glue::glue("FY22 Summary"),
        columns = 4:9
      ) %>% 
      gtsave(path = "Images", filename = glue::glue("ZamHealth_mdb_main.png"))
      
    