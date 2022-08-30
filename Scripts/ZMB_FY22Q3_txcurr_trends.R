# PROJECT: 
# PURPOSE: Munge and Analysis of Genie Data for Q3
# AUTHOR: Tim Essam | SI
# REF ID:   71fd3daa
# LICENSE: MIT
# DATE: 2022-08-15
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(selfdestructin5)
    library(lubridate)
    library(glue)
    library(gt)
    
      
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
    ref_id <- "71fd3daa"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df <- read_msd(file_path)
    
# PERIODS -----------------------------------------------------------------
    
    full_pds <- (min(df$fiscal_year) - 1) %>% 
      paste0("-10-01") %>% 
      as_date() %>% 
      seq.Date(convert_qtr_to_date(curr_pd), by = "3 months") %>% 
      convert_date_to_qtr()
    
    pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")    

# MUNGE ============================================================================
  
# PEDS
    #Peds - OU Level -----
    
    df_tx <- df %>% 
      filter(funding_agency == "USAID",
             indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      group_by(indicator, fiscal_year, trendscoarse, snu1) %>% 
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd("quarters") %>% 
      select(-results_cumulative)
    
    df_tx <- df_tx %>% 
      group_by(snu1) %>%
      mutate(decline = results < lag(results, 1),
             decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
             fill_color = case_when(fiscal_year < fy ~ trolley_grey,
                                    decline == TRUE ~ golden_sand,
                                    TRUE ~ scooter),
             fill_alpha = ifelse(fiscal_year < fy, .6, .9),
             results_latest = case_when(period == max(period) ~ results),
             decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
      fill(results_latest,decline_latest, .direction = "up") %>% 
      mutate(disp_name = glue("{snu1} {decline_latest}")) %>% 
      ungroup() 
    
    v_tx_lrg <- df_tx %>% 
      filter(period == max(period),
             trendscoarse == "<15") %>% 
      arrange(desc(results)) %>% 
      mutate(cumsum = cumsum(results)/sum(results)) %>% 
      slice_head(n = 11) %>% 
      pull(snu1)
    
    df_tx %>%
      filter(
        snu1 %in% v_tx_lrg,
        trendscoarse == "<15") %>% 
      ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
      geom_col() +
      geom_text(data = . %>% filter(period == max(period)), 
                aes(label = label_number_si()(results_latest)), 
                vjust = -.7, color = matterhorn,
                family = "Source Sans Pro") +
      facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_y_continuous(label = label_number_si()) +
      scale_x_discrete(labels = pd_brks) +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL, 
           title = "OVERALL PEDIATRIC TX_CURR GROWING IN FY22Q2",
           subtitle = glue("FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
           caption = glue("Source: {msd_source}")) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown())
    
    si_save(glue("Images/{curr_pd}_snu_ZAM_ou_txcurr_peds.png"))
    
#Peds - by SNU -----
    
    df_tx <- df %>% 
      filter(funding_agency == "USAID",
             indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      
      group_by(snu1, indicator, fiscal_year, trendscoarse) %>% 
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd("quarters") %>% 
      select(-results_cumulative)
    
    df_tx <- df_tx %>% 
      group_by(snu1) %>%
      mutate(decline = results < lag(results, 1),
             decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
             fill_color = case_when(fiscal_year < fy ~ trolley_grey,
                                    decline == TRUE ~ golden_sand,
                                    TRUE ~ scooter),
             fill_alpha = ifelse(fiscal_year < fy, .6, .9),
             results_latest = case_when(period == max(period) ~ results),
             decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
      fill(results_latest,decline_latest, .direction = "up") %>% 
      mutate(disp_name = glue("{snu1} {decline_latest}")) %>% 
      ungroup() 
    
    v_tx_lrg <- df_tx %>% 
      filter(period == max(period),
             trendscoarse == "<15") %>% 
      arrange(desc(results)) %>% 
      mutate(cumsum = cumsum(results)/sum(results)) %>% 
      slice_head(n = 11) %>% 
      pull(snu1)
    
    df_tx %>%
      filter(snu1 %in% v_tx_lrg,
             trendscoarse == "<15") %>% 
      ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
      geom_col() +
      geom_text(data = . %>% filter(period == max(period)), 
                aes(label = label_number_si()(results_latest)), 
                vjust = -.7, color = matterhorn,
                family = "Source Sans Pro") +
      facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_y_continuous(label = label_number_si()) +
      scale_x_discrete(labels = pd_brks) +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL, 
           title = "DECLINES IN PEDIATRIC TX_CURR REVERSED COURSE IN FY22Q2 IN MANY REGIONS",
           subtitle = glue("TX_CURR<15 trends in largest {length(v_tx_lrg)} regions | FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
           caption = glue("Source: {msd_source}")) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown())
    
    si_save(glue("Images/{curr_pd}_ZAM_region_txcurr_peds.png"))    
  
# Peds by Partner ------
    
    swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
      # Using EQUIP as default as this has to be done each time in FY21
      .data %>%
        mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
               mech_name = ifelse(mech_code == {{mech2}}, "Action HIV", mech_name))
    }
    
    df_tx <- df %>% 
      filter(funding_agency == "USAID",
             indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      swap_targets() %>% 
      group_by(mech_code, mech_name, indicator, fiscal_year, trendscoarse) %>% 
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd("quarters") %>% 
      select(-results_cumulative) %>% 
      mutate(mech_name = ifelse(mech_code == 17399, "DISCOVER-H", mech_name))
    
    df_tx <- df_tx %>% 
      group_by(mech_code) %>%
      mutate(decline = results < lag(results, 1),
             decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
             fill_color = case_when(fiscal_year < fy ~ trolley_grey,
                                    decline == TRUE ~ golden_sand,
                                    TRUE ~ scooter),
             fill_alpha = ifelse(fiscal_year < fy, .6, .9),
             results_latest = case_when(period == max(period) ~ results),
             decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
      fill(results_latest,decline_latest, .direction = "up") %>% 
      mutate(disp_name = glue("{mech_code} - {mech_name} {decline_latest}")) %>% 
      ungroup() 
    
    v_tx_lrg_partner <- df_tx %>% 
      filter(period == max(period),
             trendscoarse == "<15") %>% 
      arrange(desc(results)) %>% 
      mutate(cumsum = cumsum(results)/sum(results)) %>% 
      slice_head(n = 3) %>% 
      pull(mech_code)
    
    df_tx %>%
      filter(mech_code %in% v_tx_lrg_partner,
             trendscoarse == "<15") %>% 
      ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
      geom_col() +
      geom_text(data = . %>% filter(period == max(period)), 
                aes(label = label_number_si()(results_latest)), 
                vjust = -.7, color = matterhorn,
                family = "Source Sans Pro") +
      facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_y_continuous(label = label_number_si()) +
      scale_x_discrete(labels = pd_brks) +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL, 
           title = "DECLINES IN PEDIATRIC TX_CURR VARY BY TREATMENT PARTNER",
           subtitle = glue("TX_CURR<15 trends in largest {length(v_tx_lrg)} treatment partners | FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
           caption = glue("Source: {msd_source}")) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown())
    
    si_save(glue("Images/{curr_pd}_ZAM_partner_txcurr_peds.png"))

  #  

# IIT ---------------------------------------------------------------------

    genie_path_site <- list.files(si_path(), "SiteByIMs-MultipleOUs-Daily-2022-08-15", full.names = T)
    df_site <- read_msd(genie_path_site)
    
    # DATIM data as of: 08/12/2022 22:02:39 UTC
    # Genie report updated: 08/15/2022 04:55:52 UTC
    # Current period(s): 2021 Target,  2021 Q1,  2021 Q2,  2021 Q3,  2021 Q4,  2022 Target,  2022 Q1,  
    # 2022 Q2,  2022 Q3,  2023 Target
    
    # What is the number of sites with TX_ML data compared to sites with TX_CURR?
    df_iit_comp <- df_site %>% 
      filter(funding_agency == "USAID", 
             indicator %in% c("TX_ML", "TX_CURR"), 
             standardizeddisaggregate %in% c("Total Numerator")) %>% 
      group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      filter(facility != "Data reported above Facility level") %>% 
      mutate(has_iit = !is.na(tx_ml),
             has_txcurr = !is.na(tx_curr))
    
    
    # IIT report by FY / SNU1
    df_iit_comp %>% 
      group_by(period, snu1) %>% 
      summarise(iit_comp = sum(has_iit)/sum(has_txcurr)) %>% 
      pivot_wider(names_from = period, values_from = iit_comp)
    
    df_iit <- df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_CURR_Lag1", "TX_RTT"), 
             #count(indicator, standardizeddisaggregate) %>% view()
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>% 
      group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator, mech_code) %>% 
      #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
      ungroup()   
    
    # FLAG OVC SERV SITES
    df_ovc_site <- 
      df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("OVC_SERV"), 
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, snu1, facility, facilityuid, indicator, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      group_by(facility) %>% 
      mutate(ovc_facility = ifelse(ovc_serv>0, 1, NA_integer_),
             fy = substr(period, 3, 4)) %>% 
      group_by(facility, fy) %>% 
      fill(ovc_facility, .direction = "updown") %>% 
      ungroup() %>% 
      filter(facility != "Data reported above Facility level") %>% 
      select(period, facilityuid, ovc_facility, ovc_serv) %>% 
      filter(period %in% c("FY21Q4", "FY22Q1", "FY22Q2", pd))
    
    # FOLDING IN OTHER INDICATORS THAT WERE REQUESTED
    # HTS_TST_POS and VLC
    df_site_hts <- 
      df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_PVLS", "TX_CURR_Lag2"),
             standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus", "Age/Sex/HIVStatus", "Modality/Age/Sex/Result")) %>% 
      clean_indicator() %>% 
      filter(indicator != "TX_PVLS") %>%
      group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator, mech_code) %>% 
      #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      filter(period %in% c("FY21Q4", "FY22Q1", "FY22Q2", pd)) %>% 
      mutate(positivity = hts_tst_pos / hts_tst, .after = hts_tst_pos) %>% 
      mutate(vlc = tx_pvls_d / tx_curr_lag2, .after = tx_pvls_d)
             
    
    # Flag sites that have had consistent IIT from FY21Q4 - FY22Q2 -->
    # What does the distribution of tx_ml look like across sites that are
    # losing patients?
    df_iit %>% 
      filter(period %in% c("FY21Q4", "FY22Q1", "FY22Q2", pd),
             str_detect(snu1, "Central|Copperbelt")) %>% 
      group_by(facility, snu1, mech_code) %>% 
      mutate(iit_flag = ifelse(iit > .05, 1, 0),
             tot_iit = sum(iit_flag, na.rm = T)) %>% 
      ungroup() %>% 
      filter(tot_iit >= 2) %>% 
      left_join(., df_site_hts) %>% 
      left_join(., df_ovc_site) %>% 
      write_csv(glue::glue("Dataout/{pd}_ZMB_high_IIT_sites_Central_Copperbelt.csv"), na = "")
    
    
    df_iit %>% 
      filter(tx_curr_lag1 != 0,
             trendscoarse == "15+"
             ) %>%
      mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
      ggplot(aes(period, iit, size = tx_curr_lag1)) +
      geom_point(position = position_jitter(width = .2, seed = 42),
                 na.rm = TRUE, color = scooter,
                 alpha = .2) +
      geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
                  method = "loess",
                  formula = "y ~ x", se = FALSE, na.rm = TRUE,
                  size = 1.5, color = golden_sand) +
      facet_wrap(~snu1, drop = T, ) +
      scale_size(label = comma) +
      scale_x_discrete(labels = pd_brks) +
      scale_y_continuous(limits = c(0,.25),
                         label = percent_format(1),
                         oob = oob_squish) +
      labs(x = NULL, y = NULL,
           size = "Site TX_CURR (1 period prior)",
           title = glue("Sizable Adult IIT continues into {curr_pd}") %>% toupper,
           subtitle = glue("IIT calculated in the largest {length(v_tx_lrg)} TX_CURR regions"),
           caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source}")) +
      si_style() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown())
    
    si_save(glue("Images/{curr_pd}_ZMB_region_iit_adult.png"))
    
    
 # COPPERBELT IIT -------------------
    
    df_iit_copperbelt <- 
      df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_CURR_Lag1", "TX_RTT"),
             # count(indicator, standardizeddisaggregate) %>% view()
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"),
             snu1 == "Copperbelt Province") %>% 
      swap_targets() %>% 
      group_by(fiscal_year, snu1, trendscoarse, sex, facility, facilityuid, mech_code, mech_name, indicator) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE),
             mech_name = ifelse(mech_code == 17399, "DISCOVER-H", mech_name),
             disp_name = glue("{mech_code} - {mech_name}")) %>% 
      ungroup()   

    
    adult_large_iit <- df_iit_copperbelt %>% 
      filter(mech_code != 82086,
             tx_curr_lag1 != 0,
             trendscoarse == "15+",
             period == max(period),
             iit > 0.25) %>% distinct(facilityuid) %>% 
      pull()
    
    df_iit_copperbelt %>% 
      filter(mech_code != 82086,
             tx_curr_lag1 != 0,
             trendscoarse == "15+") %>%
      mutate(line_color = ifelse(sex == "Female", moody_blue, genoa)) %>% 
      #mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
      ggplot(aes(period, iit, size = tx_curr_lag1)) +
      geom_point(position = position_jitter(width = .2, seed = 42),
                 na.rm = TRUE, color = scooter,
                 alpha = .2) +
      geom_smooth(aes(weight = tx_curr_lag1, group = sex, color = line_color),
                  method = "loess",
                  formula = "y ~ x", se = FALSE, na.rm = TRUE,
                  size = 1.5) +
      facet_wrap(~disp_name) +
      scale_color_identity() +
      scale_size(label = comma) +
      scale_x_discrete(labels = pd_brks) +
      scale_y_continuous(limits = c(0,.25),
                         label = percent_format(1),
                         oob = oob_squish) +
      labs(x = NULL, y = NULL,
           size = "Site TX_CURR (1 period prior)",
           title = glue("Significant variability in Adult IIT by partner in the Copperbelt Province, with {length(adult_large_iit)} SAFE 
                        facilities over 25% IIT in {curr_pd}") %>% toupper,
           subtitle = glue("IIT calculated in the 2 largest treatment partners in the Copperbelt Province"),
           caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source}")) +
      si_style() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown())
    
    si_save(glue("Images/{curr_pd}_ZMB_adult_iit_copperbelt.png"))
    
    
    
    peds_large_iit <- df_iit_copperbelt %>% 
      filter(mech_code != 82086,
             tx_curr_lag1 != 0,
             trendscoarse == "<15",
             period == max(period),
             iit > 0.25) %>% distinct(facilityuid) %>% 
      pull()
    
    df_iit_copperbelt %>% 
      filter(mech_code != 82086,
             tx_curr_lag1 != 0,
             trendscoarse == "<15") %>%
      mutate(line_color = ifelse(sex == "Female", moody_blue, genoa)) %>% 
      #mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
      ggplot(aes(period, iit, size = tx_curr_lag1)) +
      geom_point(position = position_jitter(width = .2, seed = 42),
                 na.rm = TRUE, color = scooter,
                 alpha = .2) +
      geom_smooth(aes(weight = tx_curr_lag1, group = sex, color = line_color),
                  method = "loess",
                  formula = "y ~ x", se = FALSE, na.rm = TRUE,
                  size = 1.5) +
      scale_color_identity()+
      facet_wrap(~disp_name) +
      scale_size(label = comma) +
      scale_x_discrete(labels = pd_brks) +
      scale_y_continuous(limits = c(0,.25),
                         label = percent_format(1),
                         oob = oob_squish) +
      labs(x = NULL, y = NULL,
           size = "Site TX_CURR (1 period prior)",
           title = glue("Significant variability in Pediatric IIT by partner in the Copperbelt Province, with {length(peds_large_iit)} 
           SAFE facilities over 25% IIT in {curr_pd}") %>% toupper,
           subtitle = glue("IIT calculated in the 2 largest treatment partners in the Copperbelt Province"),
           caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source}")) +
      si_style() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown())
    
    si_save(glue("Images/{curr_pd}_ZMB_peds_iit_copperbelt.png"))
    
#NET_NEW -----------------------------------------------------------------

df_nn <- df %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         snu1 %in% v_tx_lrg) %>% 
  group_by(snu1, indicator, trendscoarse, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  pivot_longer(c(tx_net_new, tx_new), 
               names_to = "indicator") %>% 
  mutate(fill_color = ifelse(indicator == "tx_net_new", old_rose, denim),
         indicator = glue("{toupper(indicator)}"),
         # indicator = glue("{toupper(indicator)} share of TX_CURR"),
         share = value / tx_curr)
    

df_nn %>%
  filter(period != min(period),
         trendscoarse == "<15") %>%
  ggplot(aes(period, value, fill = fct_rev(indicator))) +
  geom_col(alpha = .75,
           position = position_dodge(width = .4)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~fct_reorder2(snu1, period, tx_curr),scales = "free_y") +
  scale_y_continuous(label = comma) +
  scale_x_discrete(label = pd_brks[2:length(pd_brks)]) +
  scale_fill_manual(values = c("TX_NEW" = scooter,
                               "TX_NET_NEW" = scooter_light)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Copperbelt is in its fifth straight quarter of negative TX_NET_NEW" %>% toupper,
       #title = glue("All provinces had NET_NEW gains for adults in {pd}") %>% toupper,
       caption = glue("Source: {msd_source}")) +
  si_style_ygrid() +
  theme(panel.spacing = unit(.5, "line"),
        strip.text = element_markdown())

si_save(glue("Images/{curr_pd}_ZMB_region_peds_tx-new-nn.png"))

# VLC/S -------------------------------------------------------------------

df_vl <- df_site %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
         ageasentered != "Unknown Age") %>% 
  mutate(ageasentered = case_when(trendscoarse == "<15" ~ trendscoarse,
                                  ageasentered %in% c("50-54","55-59", "60-64", "65+") ~ "50+",
                                  TRUE ~ ageasentered)) %>% 
  clean_indicator() %>% 
  group_by(snu1, indicator, ageasentered, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}")

df_vl <- df_vl %>% 
  arrange(snu1, ageasentered, period) %>% 
  group_by(snu1, ageasentered) %>% 
  mutate(vlc = tx_pvls_d/tx_curr_lag2,
         vls = tx_pvls / tx_pvls_d,
         vls_adj = tx_pvls /tx_curr_lag2) %>% 
  ungroup() %>% 
  filter(period == max(period))

df_avg <- df_vl %>% 
  group_by(snu1) %>% 
  summarise(avg_vlc = mean(vlc),
            avg_vls = mean(vls),
            avg_vls_adj = mean(vls_adj)) %>% 
  ungroup()


df_usaid_vl <- df_vl %>% 
  summarise(vlc = sum(tx_pvls_d, na.rm = TRUE)/sum(tx_curr_lag2, na.rm = TRUE),
            vls = sum(tx_pvls, na.rm = TRUE) / sum(tx_pvls_d, na.rm = TRUE),
            vls_adj = sum(tx_pvls, na.rm = TRUE) /sum(tx_curr_lag2, na.rm = TRUE))


df_vl %>% 
  filter(snu1 %in% v_tx_lrg) %>% 
  mutate(tx_curr_pct = 1) %>% 
  ggplot(aes(tx_curr_pct, ageasentered)) +
  geom_vline(data = df_avg, aes(xintercept = avg_vlc, color = grey80k)) +
  geom_col(fill = grey10k, alpha = 0.6) +
  geom_col(aes(vls), fill = scooter_light, alpha = 0.75, width = 0.75) +
  geom_col(aes(vlc), fill = scooter, alpha = 1, width = 0.75) +
  geom_vline(xintercept = 1, size = 0.5, color = grey30k) +
  geom_vline(xintercept = 0, size = 0.5, color = grey30k) +
  # geom_richtext(aes(label = glue("<span style='color:#2166ac'>{percent(vls_adj, 1)}</span> | <span style='color:#67a9cf'>{percent(vlc, 1)}</span>")), 
  #               label.color = NA,
  #           nudge_x = .2,
  #           family = "Source Sans Pro") +
  geom_richtext(aes(vls, label = glue("<span style='color:#505050'>{percent(vls, 1)}</span>")), 
                label.color = NA, fill = NA,
                nudge_x = -0.04, size = 3,
                family = "Source Sans Pro") +
  geom_richtext(aes(vlc, label = glue("<span style='color:white'>{percent(vlc, 1)}</span>")), 
                label.color = NA, fill = NA,
                nudge_x = -0.04, size = 3,
                family = "Source Sans Pro") +
  geom_text(data = df_avg, aes(x = avg_vlc, y = avg_vlc, label = percent(avg_vlc, 1)),
            hjust = .3, vjust = 1.6,
            family = "Source Sans Pro",
            color = grey90k, size = 10/.pt) +
  facet_wrap(~fct_reorder(snu1, tx_curr_lag2, sum, na.rm = TRUE,.desc = TRUE)) +
  facet_wrap(~snu1) +
  coord_cartesian(expand = T, clip = "off") +
  scale_color_identity() +
  scale_x_continuous(label = percent) +
  labs(x = NULL, y = NULL, 
       title = glue("In {curr_pd}, USAID VLC and VLS are at {percent(df_usaid_vl$vlc, 1)} and {percent(df_usaid_vl$vls, 1)} respectively") %>% toupper,
       subtitle = glue("<span style='color:{scooter}'>**VLC**</span> and <span style='color:{scooter_light}'>**VLS**</span> rates | Largest {length(v_tx_lrg)} TX_CURR regions"),
       caption = glue("Source: {msd_source}")) +
  si_style_nolines() +
  theme(panel.spacing = unit(.5, "line"),
        plot.subtitle = element_markdown(),
        strip.text = element_markdown(),
        axis.text.x = element_blank()) 

si_save(glue("Graphics/{curr_pd}_ZMB_region_vl.svg"), scale = 1.25)
si_save(glue("Images/{curr_pd}_ZMB_region_vl.png"), scale = 1.25)
