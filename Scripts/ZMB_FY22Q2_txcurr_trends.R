# PROJECT:  nshima-time
# AUTHOR:   K. Srikanth, T. Essam | USAID
# PURPOSE:  develop visuals for FY22Q2 review
# LICENSE:  MIT
# DATE:     2022-05-17
# UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gt)
library(selfdestructin5)
library(fontawesome)
library(lubridate)


# GLOBAL VARIABLES --------------------------------------------------------

#genie_path <- file.path(si_path(), "Genie-PSNUByIMs-Zambia-Daily-2022-05-16_ALL.zip")
genie_path  <- file.path(si_path(), "MER_Structured_Datasets_Site_IM_FY20-22_20220513_v1_2_Zambia.zip")

msd_source <- source_info(genie_path)
curr_pd <- source_info(genie_path, return = "period")
curr_fy <- source_info(genie_path, return = "fiscal_year")
curr_qtr <- source_info(genie_path, return = "quarter")

#indicators used
# c("HTS_TST_POS", "OVC_SERV", "PMTCT_EID", "TB_PREV", "TB_STAT", "TB_STAT_POS", "TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW", "TX_NEW", "TX_PVLS", "TX_PVLS", "TX_PVLS_D")

# IMPORT ------------------------------------------------------------------

# df <- si_path() %>% 
#   return_latest("PSNU_IM") %>% 
#   read_msd() %>% 
#   filter(operatingunit == "Tanzania")

df <- read_msd(genie_path)

# PERIODS -----------------------------------------------------------------

full_pds <- (min(df$fiscal_year) - 1) %>% 
  paste0("-10-01") %>% 
  as_date() %>% 
  seq.Date(convert_qtr_to_date(curr_pd), by = "3 months") %>% 
  convert_date_to_qtr()

pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")

# TX_CURR TRENDS ----------------------------------------------------------

#Peds - OU Level -----

df_tx <- df %>% 
  filter(funding_agency == "USAID",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
  group_by(indicator, fiscal_year, trendscoarse) %>% 
  summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd("quarters") %>% 
  select(-results_cumulative)

df_tx <- df_tx %>% 
  #group_by(snu1) %>%
  mutate(decline = results < lag(results, 1),
         decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
         fill_color = case_when(fiscal_year < curr_fy ~ trolley_grey,
                                decline == TRUE ~ golden_sand,
                                TRUE ~ scooter),
         fill_alpha = ifelse(fiscal_year < curr_fy, .6, .9),
         results_latest = case_when(period == max(period) ~ results),
         decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
  fill(results_latest,decline_latest, .direction = "up") %>% 
  #mutate(disp_name = glue("{snu1} {decline_latest}")) %>% 
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
    #snu1 %in% v_tx_lrg,
    trendscoarse == "<15") %>% 
  ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
  geom_col() +
  geom_text(data = . %>% filter(period == max(period)), 
            aes(label = label_number_si()(results_latest)), 
            vjust = -.7, color = matterhorn,
            family = "Source Sans Pro") +
  #facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_y_continuous(label = label_number_si()) +
  scale_x_discrete(labels = pd_brks) +
  coord_cartesian(expand = T, clip = "off") +
  labs(x = NULL, y = NULL, 
       title = "OVERALL PEDIATRIC TX_CURR DECLINING IN FY22Q2",
       subtitle = glue("FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
       caption = glue("Source: {msd_source}")) +
  si_style_ygrid() +
  theme(panel.spacing = unit(.5, "line"),
        plot.subtitle = element_markdown(),
        strip.text = element_markdown())

si_save(glue("Images/{curr_pd}_ZAM_ou_txcurr_peds.png"))

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
         fill_color = case_when(fiscal_year < curr_fy ~ trolley_grey,
                                decline == TRUE ~ golden_sand,
                                TRUE ~ scooter),
         fill_alpha = ifelse(fiscal_year < curr_fy, .6, .9),
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
         fill_color = case_when(fiscal_year < curr_fy ~ trolley_grey,
                                decline == TRUE ~ golden_sand,
                                TRUE ~ scooter),
         fill_alpha = ifelse(fiscal_year < curr_fy, .6, .9),
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

# IIT ---------------------------------------------------------------------

genie_path_site <- file.path(si_path(), "MER_Structured_Datasets_Site_IM_FY20-22_20220513_v1_2_Zambia.zip")
df_site <- read_msd(genie_path_site)

df_iit <- df_site %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_CURR_Lag1", "TX_RTT"),
 # count(indicator, standardizeddisaggregate) %>% view()
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>% 
  #group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
  group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "indicator",
              names_glue = "{tolower(indicator)}") %>% 
  rowwise() %>% 
  mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  ungroup()

# Flag sites that have had consistent IIT from FY21Q4 - FY22Q2 -->
# What does the distribution of tx_ml look like across sites that are
# losing patients?
  df_iit %>% 
    filter(period %in% c("FY21Q4", "FY22Q1", "FY22Q2"),
           str_detect(snu1, "Central|Copperbelt")) %>% 
    group_by(facility) %>% 
    mutate(iit_flag = ifelse(iit > .05, 1, 0),
           tot_iit = sum(iit_flag, na.rm = T)) %>% 
    filter(tot_iit == 2) %>% 
    write_csv("Dataout/ZMB_high_IIT_sites_Central_Copperbelt.csv")


df_iit %>% 
  filter(snu1 %in% c,
         tx_curr_lag1 != 0,
         trendscoarse == "<15") %>% 
  mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
  ggplot(aes(period, iit, size = tx_curr_lag1)) +
  geom_point(position = position_jitter(width = .2, seed = 42),
             na.rm = TRUE, color = scooter,
             alpha = .2) +
  geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
              method = "loess",
              formula = "y ~ x", se = FALSE, na.rm = TRUE,
              size = 1.5, color = golden_sand) +
  facet_wrap(~snu1) +
  scale_size(label = comma) +
  scale_x_discrete(labels = pd_brks) +
  scale_y_continuous(limits = c(0,.25),
                     label = percent_format(1),
                     oob = oob_squish) +
  labs(x = NULL, y = NULL,
       size = "Site TX_CURR (1 period prior)",
       title = glue("Sizable Pediatric IIT continue into {curr_pd}") %>% toupper,
       subtitle = glue("IIT calculated in the largest {length(v_tx_lrg)} TX_CURR regions"),
       caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source}")) +
  si_style() +
  theme(panel.spacing = unit(.5, "line"),
        plot.subtitle = element_markdown())

si_save(glue("Images/{curr_pd}_ZMB_region_iit_peds.png"))


# COPPERBELT -------------------

df_iit_copperbelt <- df_site %>% 
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
       title = glue("Significant variability in Adult IIT by partner in the Copperbelt Province, with {length(adult_large_iit)} SAFE facilities
                    over 25% IIT in {curr_pd}") %>% toupper,
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
         title = glue("Significant variability in Pediatric IIT by partner in the Copperbelt Province, with {length(peds_large_iit)} SAFE facilities
                    over 25% IIT in {curr_pd}") %>% toupper,
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
         title = "Lower TX_NEW in FY22 not making up for NET_NEW losses" %>% toupper,
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          strip.text = element_markdown())
  
  si_save(glue("Images/{curr_pd}_ZMB_region_tx-new-nn.png"))
  
  # VLC/S -------------------------------------------------------------------
  
  df_vl <- df %>% 
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
  
  
