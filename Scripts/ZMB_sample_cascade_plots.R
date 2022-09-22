# PROJECT: Show power of R
# PURPOSE: Munge and Analysis of PSNU X IM data
# AUTHOR: Tim Essam | SI
# REF ID:   7493c5ce
# LICENSE: MIT
# DATE: 2022-09-15
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(extrafont)
  library(tidyverse)
  library(cascade)
  library(extrafont)
  library(selfdestructin5)
  library(gt)
  
  load_secrets()
    
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  
  # REF ID for plots
    ref_id <- "7493c5ce"
    

# CUSTOM FUNCTIONS --------------------------------------------------------

    swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
      # Using EQUIP as default as this has to be done each time in FY21
      .data %>%
        mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
               mech_name = ifelse(mech_code == {{mech2}}, "Action HIV", mech_name))
    }
    
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
    
    bold_column <- function(gt_obj, col){
      gt_obj %>% 
        tab_style(
          style = list(
            gt::cell_fill(color = "#e6e7e8", alpha = 0.5),
            gt::cell_text(weight = 700)
          ),
          locations = cells_body(
            columns = {{col}},
          )
        )
    }

    embiggen <- function(gt_obj){
      gt_obj %>% 
        tab_options(
          source_notes.font.size = 10,
          table.font.size = 15,
          footnotes.font.size = 10)
    }
    
# DATA SOURCES  --------------------------------------------------------

  site_msd_path <- return_latest(si_path(), "Site_IM_FY20-23_20220812.*Zambia")
  psnuim_msd_path <-  return_latest(si_path(), "_PSNU_IM_FY20-23_20220812_v1_1_Zambia.zip")

# PULL METATDATA AND GENERATE OBJECTS -------------------------------------

  get_file_metadata(psnuim_msd_path)
  df <- read_msd(psnuim_msd_path)
  
  # Reading in site level to look at VLS/VLC dips in SAFE sites
  df_site <- read_msd(site_msd_path)


# CREATE CASCADES ---------------------------------------------------------

  plot_name
  return_cascade(df, cscd_num = 5) %>% prinf()
  return_cascade_plot(df, export = F)
  
  batch_cascade_plot(df_sub, imgtype = ".png", imgpath = "Images")

# What if we wanted to limit this to only SAFE? 17413 
df %>% 
  filter(funding_agency == "USAID") %>% 
  count(mech_code, mech_name) %>% 
  prinf()

# df_safe only
df_safe <-
  df %>% 
  filter(mech_code == 17413)

  batch_cascade_plot(df_safe, imgtype = str_c("_", .x, ".png"), imgpath = "Images")
  return_cascade(df_safe, 1)
  

# CREATE QUARTERLY SUMMARY TABLES -----------------------------------------

  # Grab metadata
  pd <- source_info(psnuim_msd_path, return = "period")
  fy <- source_info(psnuim_msd_path, return = "fiscal_year")
  qtr <- source_info(psnuim_msd_path, return = "quarter")  
  
  # Prepare the data 
  mdb_df   <- make_mdb_df(df)
  mdb_tbl  <- reshape_mdb_df(mdb_df, curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd) 
  
  # CREATE THE TABLES
  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", curr_pd, msd_source)
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", pd, msd_source) 

  # Make Tables just for IPs
  mk_ptr_tbl(df %>% swap_targets(), 82075) 
  mk_ptr_tbl(psnu_im, 17413)
  mk_ptr_tbl(psnu_im %>% 
               mutate(mech_name = ifelse(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)), 17399)


    