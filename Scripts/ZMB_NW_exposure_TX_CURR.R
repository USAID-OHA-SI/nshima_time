# PROJECT: Q3 Clean Data Review for HO Leadership
# PURPOSE: Sythesize data for Q3, focus on NW prov
# AUTHOR: Tim Essam | SI
# REF ID:   c3274783
# LICENSE: MIT
# DATE: 2022-09-20
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(cascade)
    library(scales)
    library(extrafont)
    library(gt)
    library(gtExtras)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    site_path <- return_latest(folderpath = merdata,
      pattern = "Site.*_FY20-23.*Zambia.txt")
    file_path <- return_latest(folderpath = merdata, pattern = "PSNU.*_FY20-23.*Zambia")
    
    subnat_path <- return_latest(folderpath = merdata, pattern = "NAT_SUBNAT")
      
  # Grab metadata
    get_file_metadata(file_path)
    shpdata <- glamr::si_path("path_vector")
  
    
  # REF ID for plots
    ref_id <- "c3274783"
    
  # Functions  
  summarise_targets <- function(df, ...) {
    df %>% 
      group_by(snu1, fiscal_year, ...) %>% 
      summarise(targets = sum(targets, na.rm = T)) %>% 
      group_by(snu1, ...) %>% 
      mutate(growth = (targets/lag(targets, n = 1)) - 1) %>% 
      ungroup() %>% 
      mutate(snu_order = fct_reorder(snu1, targets, .desc = T),
             flag = case_when(
               str_detect(snu1, "NorthWestern") & fiscal_year == max(fiscal_year) ~ 1,
               TRUE ~ 0
             )
      )
  }
  

# LOAD DATA ============================================================================  

  site_im <- read_msd(site_path)
  
  site_im %>% 
    filter(snu1 == "NorthWestern Province", 
           fiscal_year %in% c(2021, 2022, 2023)) %>% 
    count(mech_name, prime_partner_name)
  
  psnu_im <- read_msd(file_path)
      
  subnat <- read_msd(subnat_path) %>% 
    filter(operatingunit == "Zambia")  %>% 
    mutate(snu1 = gsub(" Province", "", snu1)) %>% 
    clean_psnu()
  

# ASKED FOR A COMPARISON OF PROV TARGET COV -------------------------------

  # What does overall target growth look like by province?
  subnat_snu <- 
    subnat %>% 
    filter(fiscal_year > 2020,
           indicator %in% c("TX_CURR_SUBNAT"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    summarise_targets()
  
  # What does USAID Targets from MSD look like?
  tx_tgt_subnat <- psnu_im %>% 
    mutate(snu1 = gsub(" Province", "", snu1)) %>% 
    clean_psnu() %>% 
    filter(fiscal_year > 2020,
           indicator == "TX_CURR", 
           standardizeddisaggregate == "Total Numerator",
           str_detect(snu1, "Military", negate = T)) %>% 
    summarise_targets() %>% 
    rename(tx_targets = targets, tx_growth = growth)
    
  tx_combo <- subnat_snu %>% 
    left_join(tx_tgt_subnat) %>% 
    mutate(gap = targets - tx_targets,
           pepfar_cov = tx_targets/targets)
    
  tx_snu_combo_base <- 
    tx_combo %>% 
    select(snu1, fiscal_year, TX_CURR_SUBNAT = targets, PEPFAR_TARGETS = tx_targets, 
           GAP = gap, COVERAGE = pepfar_cov) 
  
  tx_snu_combo_list <- 
    tx_snu_combo_base %>% 
    group_by(snu1) %>% 
    summarize(TX_CURR_SUBNAT = list(TX_CURR_SUBNAT), 
              PEPFAR_TARGETS = list(PEPFAR_TARGETS),
              GAP = list(GAP),
              COVERAGE = list(COVERAGE)) %>% 
    pivot_longer(TX_CURR_SUBNAT:COVERAGE) %>% 
    mutate(name = fct_relevel(name, c("PEPFAR_TARGETS", "TX_CURR_SUBNAT", "GAP", "COVERAGE"))) %>% 
    arrange(name)
  
  tx_snu_combo_base %>% 
    pivot_longer(TX_CURR_SUBNAT:COVERAGE) %>% 
    spread(fiscal_year, value) %>% 
    mutate(name = fct_relevel(name, c("PEPFAR_TARGETS", "TX_CURR_SUBNAT", "GAP", "COVERAGE"))) %>% 
    arrange(name) %>% 
    left_join(tx_snu_combo_list) %>% 
    filter(snu1 %in% c("NorthWestern")) %>% 
    gt(groupname_col = "snu1") %>% 
    gt_plt_sparkline(value, same_limit = F, type = "shaded", 
                     fig_dim = c(3, 30),
                     palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                     label = F) %>% 
    fmt_number(columns = is.numeric,
               rows = name %ni% "COVERAGE",
               decimals = 0) %>% 
    fmt_percent(columns = is.numeric,
                rows = name %in% "COVERAGE",
                decimals = 0) %>% 
    cols_align(name,
               align = "left") %>% 
    cols_label(name = "", 
               value = "") %>% 
    tab_style(
      style = list(
        cell_text(weight = 620)
      ),
      locations = cells_body(
        rows = name == "GAP"
      )
    ) %>% 
    tab_header(title = "NORTHWESTERN PROVINCE TREATMENT COVERAGE COP21-23") %>% 
  gtsave(., "Images/NWPROV_TX_CURR_SUBNAT_summary.png")
  

# REDO WITH PSNUS INFO ----------------------------------------------------

  
  # REDO ALL THIS FOR ALL PSNUS in NW
  # TABLE WILL GET COMPRESSED
    
  # What does overall target growth look like by province?
  subnat_psnu <- 
    subnat %>% 
    filter(fiscal_year > 2020,
           indicator %in% c("TX_CURR_SUBNAT"),
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           snu1 == "NorthWestern") %>% 
    summarise_targets(psnu)
  
  # What does USAID Targets from MSD look like?
  tx_tgt_psnu <- psnu_im %>% 
    mutate(snu1 = gsub(" Province", "", snu1)) %>% 
    clean_psnu() %>% 
    filter(fiscal_year > 2020,
           indicator == "TX_CURR", 
           standardizeddisaggregate == "Total Numerator",
           snu1 == "NorthWestern") %>% 
    summarise_targets(psnu) %>% 
    rename(tx_targets = targets, tx_growth = growth) %>% 
    complete(psnu, fiscal_year)
  
  tx_combo_psnu <- subnat_psnu %>% 
    left_join(tx_tgt_psnu) %>% 
    mutate(gap = targets - tx_targets,
           pepfar_cov = tx_targets/targets)
  
  
  tx_snu_combo_psnu_base <- 
    tx_combo_psnu %>% 
    select(psnu, fiscal_year, TX_CURR_SUBNAT = targets, PEPFAR_TARGETS = tx_targets, 
           GAP = gap, COVERAGE = pepfar_cov) 
  
  tx_snu_combo_list <- 
    tx_snu_combo_psnu_base %>% 
    group_by(psnu) %>% 
    summarize(TX_CURR_SUBNAT = list(TX_CURR_SUBNAT), 
              PEPFAR_TARGETS = list(PEPFAR_TARGETS),
              GAP = list(GAP),
              COVERAGE = list(COVERAGE)) %>% 
    pivot_longer(TX_CURR_SUBNAT:COVERAGE) %>% 
    mutate(name = fct_relevel(name, c("PEPFAR_TARGETS", "TX_CURR_SUBNAT", "GAP", "COVERAGE"))) %>% 
    arrange(name)
  
  
# Pull out psnu list
  psnus <- subnat_psnu %>% distinct(psnu) %>% 
    filter(psnu %ni% c("Kabompo", "Kasempa", "Manyinga")) %>% 
             pull()
  # So, a few PSNUs do not have FY23 targets
  # Kabompo, Kasempa, Manyinga, 
  
  tx_snu_combo_psnu_base %>% 
    pivot_longer(TX_CURR_SUBNAT:COVERAGE) %>% 
    spread(fiscal_year, value) %>% 
    mutate(name = fct_relevel(name, c("PEPFAR_TARGETS", "TX_CURR_SUBNAT", "GAP", "COVERAGE"))) %>% 
    arrange(name) %>%
    mutate(`FY23 % Growth` = case_when(
      name %ni% c("COVERAGE", "GAP") ~ (`2023`/`2022`) - 1, 
      TRUE ~ NA_real_)
    ) %>% 
    filter(psnu %in% psnus[1:4]) %>% 
    gt(groupname_col = "psnu") %>% 
    fmt_number(columns = 3:5,
               rows = name %ni% "COVERAGE",
               decimals = 0) %>% 
    fmt_percent(columns = 3:5,
                rows = name %in% "COVERAGE",
                decimals = 0) %>% 
    fmt_percent(columns = 6,
                decimals = 0) %>% 
    sub_missing(
                missing_text = "-") %>% 
    cols_align(name,
               align = "left") %>% 
    cols_label(name = "") %>% 
    tab_style(
      style = list(
        cell_text(weight = 620)
      ),
      locations = cells_body(
        rows = name == "GAP"
      )
    ) %>%
    tab_style(
          style = list(
           cell_text(weight = 600)
          ),
          locations = cells_row_groups(groups = everything())
      ) %>% 
    tab_header(title = "NORTHWESTERN PROVINCE DISTRICT ") %>% 
    #tab_header(title = "TREATMENT COVERAGE COP21-23    ") %>% 
    tab_options(table.font.size = 10,
                row_group.padding = 0,
                data_row.padding = 1) %>% 
    gtsave(., "Images/NWPROV_TX_CURR_SUBNAT_psnu0_summary.png")

# SHOW THE GROWTH OF TX_CURR TARGETS --------------------------------------
  

  
    subnat_snu %>% 
    ggplot(aes(x = factor(fiscal_year), y = targets)) +
    geom_col(fill = grey50k) +
    geom_col(data = . %>% filter(flag == 1), fill = old_rose) +
    # geom_hline(yintercept = c(1e5, 2e5), color = "white", size = 0.25) +
    geom_text(aes(label = percent(growth, 1)), 
              size = 9/.pt,
              color = grey90k,
              family = "Source Sans Pro",
              vjust = -0.25) +
    facet_wrap(~snu_order, nrow = 2) +
    scale_y_continuous(labels = comma) +
    si_style_ygrid(facet_space = 0.5) +
    labs(y = "TX_CURR_SUBNAT Targets", x = NULL,
         title = "TX_CURR SUBNAT TARGETS FOR PAST THREE FISCAL YEARS",
         subtitle = "Percentage change in targets from previous year listed above target level")
    
  si_save("Images/ZMB_TX_CURR_SUBNAT_target_growth_snu1.png", scale = 1.25)
  
  # Where did all this growth occur within NW province? BY AGE SEX?
  
  subnat_nw <- 
    subnat %>% 
    filter(indicator == "TX_CURR_SUBNAT",
           snu1 == "NorthWestern", 
           fiscal_year > 2019,
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    group_by(psnu, fiscal_year, sex, trendscoarse) %>% 
    summarise(targets = sum(targets, na.rm = T)) %>% 
    group_by(psnu, sex, trendscoarse) %>% 
    mutate(growth = (targets/lag(targets, n = 1)) - 1) 
  
  subnat_nw %>% 
    ggplot(aes(x = factor(fiscal_year), y = targets)) +
    geom_col(aes(fill = sex)) +
    # geom_hline(yintercept = c(1e5, 2e5), color = "white", size = 0.25) +
    geom_text(aes(label = percent(growth, 1)), 
              size = 8/.pt,
              color = grey90k,
              family = "Source Sans Pro",
              vjust = -0.25, 
              position = position_stack(vjust = 0.1)) +
    facet_wrap(~psnu + trendscoarse, scales = "free_y", nrow = 3) +
    scale_y_continuous(labels = comma) +
    si_style_ygrid(facet_space = 0.5) +
    scale_fill_manual(values = c("Female" = moody_blue_light, "Male" = genoa_light)) +
    labs(y = "TX_CURR_SUBNAT Targets", x = NULL,
         title = "TX_CURR SUBNAT TARGETS FOR PAST THREE FISCAL YEARS IN NORTHWESTERN PROVINCE",
         subtitle = "Percentage change in targets from previous year listed above target level")
  
  si_save("Images/ZMB_TX_CURR_SUBNAT_target_growth_NW_age_sex.png", scale = 1.25)
  # BASIC HEAT MAP OF USAID COVERAGE by PSNU in NWPROV

# COVERAGE for USAID ------------------------------------------------------


  usaid_cov_nw <- psnu_im %>% 
    filter(snu1 == "NorthWestern Province", 
           !is.na(targets), 
           standardizeddisaggregate == "Total Numerator",
           indicator %in% c("HTS_TST", "TX_CURR")
           ) %>% 
    clean_psnu() %>% 
    group_by(mech_name, mech_code, indicator, fiscal_year, psnu) %>% 
    summarise(tot = sum(targets)) %>% 
    ungroup() 
  
  usaid_cov_nw %>% 
    complete(psnu, indicator, fiscal_year, mech_name) %>% 
    mutate(mech_name = case_when(
      str_detect(mech_name, "DISCOVER") ~ "DISCOVER",
      str_detect(mech_name, "CHEK") ~ "CHEKUP I",
      str_detect(mech_name, "Placeholder") ~ "Zambia Integrated Health",
      TRUE ~ mech_name
    )) %>% 
    # filter(indicator == "TX_CURR") %>% 
    ggplot(aes(x = fiscal_year, y = psnu)) +
    geom_tile(aes(fill = tot), color = "white", size = 0.25) +
    geom_text(aes(label = comma(tot), 
                  color = ifelse(tot > 9000, "white", grey90k)), size = 7/.pt)+
    facet_wrap(mech_name~indicator, nrow = 5, 
               labeller = label_wrap_gen(multi_line = FALSE, width = 100)) +
    scale_fill_si(palette = "scooters", na.value = grey10k) +
    si_style_nolines(facet_space = 0.4) +
    scale_color_identity() +
    theme(legend.position = "none") +
    labs(x = NULL, y= NULL,
         title = "SUMMARY OF USAID COVERAGE IN NORTHWESTERN PROVINCE DISTRICTS",
         subitle = "Targets listed by psnu, mechanism and indicator. Gray box indicates no coverage.", 
         caption = glue::glue("Source:{msd_source} | ref:{ref_id}")) 
  
  si_save("Images/ZMB_NWPROV_target_coverage_cascade.png", scale = 1.25)
  

# Targets to results performance     
  #NW TX_CURR
    tx_curr_summary <- 
      site_im %>% 
      filter(snu1 == "NorthWestern Province", 
             fiscal_year %in% c(2020, 2021, 2022, 2023)) %>% 
      filter(indicator %in% c("TX_CURR"),
             mech_name != "Dedup", 
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(psnu, mech_code, mech_name, fiscal_year) %>% 
      summarise(across(matches("target|cumul"), sum, na.rm = T)) %>% 
      mutate(achv = cumulative / targets) %>% 
      clean_psnu() %>% 
      mutate(results_flag = ifelse(cumulative > 1, 1, NA_integer_)) %>% 
      group_by(psnu, mech_code) %>% 
      fill(results_flag)

# Targets to results performance   ----------------------------------------

    
    tx_curr_summary %>% ungroup() %>%
      group_by(fiscal_year) %>% 
      summarize(targets = sum(targets))
    
    tx_curr_summary %>% 
      mutate(mech_name = case_when(
        str_detect(mech_name, "DISCOVER") ~ "DISCOVER", 
        str_detect(mech_name, "Placeholder") ~ "Zambia Integrated Health", 
        TRUE ~ mech_name)
        ) %>% 
      filter(results_flag == 1) %>% 
      ggplot(aes(x = factor(fiscal_year), fill = str_c(psnu, mech_name))) +
      geom_col(aes(y = targets), fill = grey20k, width = 0.5) +
      geom_col(aes(y = cumulative), fill = scooter_med,
               position = position_nudge(x = 0.1), width = 0.5) +
      geom_text(data = . %>% filter(fiscal_year < 2023), 
                aes(label = scales::percent(achv, 1), y = cumulative),
                color = grey90k, 
                family = "Source Sans Pro", 
                size = 9/.pt, 
                position = position_nudge(x = 0.1),
                vjust = -0.25) +
      facet_wrap(~psnu + mech_name) +
      scale_y_continuous(labels = comma) +
      si_style_ygrid(facet_space = 0.5)+
      labs(title = str_to_upper("TX_CURR summary for Districts covered by USAID in NorthWestern Province FY20-FY23"),
           subtitle = "Targets in gray, results in light blue, achievement listed above", 
           caption = glue::glue("Source:{msd_source} | ref:{ref_id}"), 
           x = NULL,
           y = NULL)
    si_save("Images/ZMB_TX_CURR_summary_NW_prov.png", scale = 1.25)
        
        
    sites <- site_im %>% 
      filter(fiscal_year == "2022") %>% 
      distinct(orgunituid, mech_name, mech_code, snu1, psnu, funding_agency, sitename, facility) %>% 
      clean_agency() %>% 
      filter(facility != "Data reported above Facility level", mech_name != "Dedup")
    
    sites %>% 
      group_by(psnu, mech_name, mech_code) %>% 
      tally()
    
    sites %>% distinct(orgunituid)
    
    # Extract factilities
    cntry <- "Zambia"
    level_fac <- grabr::get_ouorglevel(cntry, org_type = "facility")
    df_facs <- extract_locations(cntry, level_fac) %>% extract_facilities()
    
    sites_geo <- left_join(sites, df_facs, by = c("orgunituid" = "id"))
    
    
  # Pull in shapefiles

    spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
    zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% 
                            gisr::extract_boundaries(country = cntry, level = .x))
    names(zmb_geo) <- list("adm0", "snu1", "psnu")
    

  df_usaid <- msd %>% 
    filter(funding_agency == "USAID") %>% 
    resolve_knownissues()

# MUNGE ============================================================================
  
  # What does the cascade look like for NW prov?
  psnu_im_nw <- psnu_im %>% 
    filter(snu1 == "NorthWestern Province")
  
  plot_name
  
  batch_cascade_plot(psnu_im_nw, 
                     imgtype = ".svg", imgpath = "Images/cascade/")
  
  
  # Let's make a MDB table on this info as well
  library(selfdestructin5)
  library(gt)
  pd <- source_info(file_path, return = "period")
  
  mdb_df   <- make_mdb_df(psnu_im_nw %>% filter(fiscal_year != 2023))
  mdb_tbl  <- reshape_mdb_df(mdb_df, curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(psnu_im_nw %>% filter(fiscal_year != 2023))
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd) 
  
  source("Scripts/fy22_partner_reivew_functions.R")
  
  # Create tables
  mdb_tbl  %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", curr_pd, msd_source) %>% 
    tab_header(title = "NorthWestern Province Performance Summary") %>% 
    gtsave(path = "Images", filename = glue::glue("ZMB_NWPROV_{curr_pd}_mdb_main.png"))  
  
  
    create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", pd, msd_source) %>%
      tab_header(title = "NorthWestern Province Treatment Performance Summary") %>% 
    bold_column(., Q3) %>% 
    embiggen() %>% 
    gtsave(., path = "Images", filename = glue::glue("{pd}_ZMB_NWPROV_MMD_VL_MD.png")) 
  
# VIZ ============================================================================

 
  

# SPINDOWN ============================================================================

