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
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    site_path <- return_latest(folderpath = merdata,
      pattern = "Site.*_FY20-23.*Zambia.zip")
    file_path <- return_latest(folderpath = merdata, pattern = "PSNU.*_FY20-23.*Zambia")
    
    subnat_path <- return_latest(folderpath = merdata, pattern = "NAT_SUBNAT")
      
  # Grab metadata
    get_file_metadata(file_path)
    shpdata <- glamr::si_path("path_vector")
  
    
  # REF ID for plots
    ref_id <- "c3274783"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  site_im <- read_msd(site_path2) %>% filter(snu1 == "NorthWestern Province")
    
  subnat <- read_msd(subnat_path) %>% filter(operatingunit == "Zambia")  
  subnat %>% 
    filter(fiscal_year > 2019,
           indicator %in% c("TX_CURR_SUBNAT"), 
           snu1 == "NorthWestern Province") %>% 
    group_by(snu1, fiscal_year) %>% 
    summarise(targets = sum(targets, na.rm = T))
    
  #NW TX_CURR
    tx_curr_summary <- 
      site_im %>% 
      filter(indicator == "TX_CURR", mech_name != "Dedup", 
             standardizeddisaggregate == "Total Numerator",) %>% 
      group_by(psnu, mech_code, mech_name, fiscal_year) %>% 
      summarise(across(matches("target|cumul"), sum, na.rm = T)) %>% 
      mutate(achv = cumulative / targets) %>% 
      clean_psnu()
    
    tx_curr_summary %>% ungroup() %>%
      group_by(fiscal_year) %>% 
      summarize(targets = sum(targets))
    
    tx_curr_summary %>% 
      mutate(mech_name = ifelse(str_detect(mech_name, "SAFE", negate = T), "DISCOVER", mech_name)) %>% 
      ggplot(aes(x = factor(fiscal_year), fill = str_c(psnu, mech_name))) +
      geom_col(aes(y = targets), fill = grey20k, width = 0.5) +
      geom_col(aes(y = cumulative), fill = scooter_med,
               position = position_nudge(x = 0.1), width = 0.5) +
      geom_text(aes(label = scales::percent(achv, 1), y = cumulative),
                color = grey90k, 
                family = "Source Sans Pro", 
                size = 9/.pt, 
                position = position_nudge(x = 0.1),
                vjust = -0.25) +
      facet_wrap(~psnu + mech_name) +
      scale_y_continuous(labels = comma) +
      si_style_ygrid(facet_space = 0.5)+
      labs(title = "TX_CURR summary for Districts coverec by USAID in NorthWestern Province FY20-FY22",
           subtitle = "Targets in gray, results in light blue, achievement listed above", 
           caption = glue::glue("Source:{msd_source} | ref:{ref_id}"), 
           x = NULL,
           y = NULL)
    si_save("Images/ZMB_TX_CURR_summary_NW_prov.png")
        
        
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
  
  #  check the cascade for me
  plot_name
  return_cascade(msd, 3)
  
# VIZ ============================================================================

  #  plot cascade
  return_cascade_plot(df_usaid, export = T, path = "Images/cascade/")
  
  batch_cascade_plot(df_usaid %>% filter(mech_code == 17399), imgtype = ".svg", imgpath = "Images/cascade/")
  

# SPINDOWN ============================================================================

