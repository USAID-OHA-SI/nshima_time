# Purpose: Create performance and targets from Genie For Zambia
# Author: Tim Essam | Karishma Srinkanth
# Date: 2022-05-17
# Notes: FY22 Q2 Review for Mission


# GLOBALS -----------------------------------------------------------------
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(scales)
library(extrafont)
library(tidytext)
library(ggtext)
library(here)
library(readxl)
library(glue)
library(googlesheets4)
library(getrdone)
library(selfdestructin5)
library(gt)


# DATIM data as of: 05/13/2022 21:50:51 UTC
# Genie report updated: 05/14/2022 05:13:35 UTC
# Operating Unit: Zambia,
# Fiscal Year: 2023,2022,2021,
# 

# GLOBALS -----------------------------------------------------------------

load_secrets()
source("Scripts/fy22_partner_reivew_functions.R")

  merdata <- file.path(glamr::si_path("path_msd"), "Genie")
  
  file_genie <- return_latest(folderpath = merdata,
                              #pattern = "Genie-PSNUByIMs-Zambia-Daily-2021-08-05")
                              pattern = "Genie-PSNUByIMs-Zambia-Daily-")

# Grab metadata
  msd_source <- source_info(file_genie)
  curr_pd <- source_info(file_genie, return = "period")
  pd <- source_info(file_genie, return = "period")
  fy <- source_info(file_genie, return = "fiscal_year")
  qtr <- source_info(file_genie, return = "quarter")  


# Run MDBs --------------------------------------------------------------

  psnu_im <- read_msd(file_genie, convert_to_old_names = T)

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
    bold_column(., Q1) %>% 
    bold_rowgroup(.) %>% 
    embiggen() %>% 
    gtsave(., path = mdb_out, filename = glue::glue("{pd}_Zambia_MMD_VL_MD.png"))  

mdb_tbl_zmb <- psnu_im %>%
  mutate(fundingagency = "USAID") %>% 
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
