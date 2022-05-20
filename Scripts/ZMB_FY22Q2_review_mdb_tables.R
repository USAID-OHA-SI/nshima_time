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



# additional indicators ---------------------------------------------------

drms_ban_ou <- 
  psnu_im %>% 
  filter(indicator == "AGYW_PREV", 
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
         fiscal_year == fy) %>% 
  group_by(fiscal_year, standardizeddisaggregate, snu1) %>% 
  summarise(across(matches("cum"), sum, na.rm = T)) %>% 
  spread(standardizeddisaggregate, cumulative) %>% 
  mutate(pct_complete = `Total Numerator`/`Total Denominator`,
         ou_order = fct_reorder(snu1, `Total Denominator`)) %>% 
  arrange(desc(pct_complete))

drms_ban_ou %>% 
  ggplot(aes(y = ou_order)) +
  geom_col(aes(x = `Total Denominator`), fill = trolley_grey_light) +
  geom_col(aes(x = `Total Numerator`), fill = scooter, alpha = 0.75) +
  geom_vline(xintercept = seq(1e4, 4e4, by = 1e4), color = "white") +
  geom_text(aes(x = `Total Numerator`, label = percent(pct_complete, 1)), size = 9/.pt, family = "Source Sans Pro", color = color_plot_text) +
  si_style_nolines() +
  scale_x_continuous(labels = unit_format(1, unit = "K", scale = 1e-3),
                     position = "top", 
                     breaks = seq(1e4, 4e4, by = 1e4)) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL, title = "Percentage of adolescent girls and young women (AGYW) that completed \nat least the DREAMS primary package of evidence-based services/interventions.",
       caption = glue("Source: {msd_source}"))

si_save("Images/ZMB_dreams_completion_FY22Q2")


# CXCA --------------------------------------------------------------------

df_cxca <- psnu_im %>% 
  filter(fundingagency == "USAID",
         str_detect(indicator, "CXCA"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year <= fy) %>% 
  group_by(fiscal_year, fundingagency, indicator, snu1) %>% 
  summarise(across(matches("2|4"), sum, na.rm = TRUE)) %>% 
  pivot_longer(cols = matches("qtr"),
               names_to = "period") %>% 
  spread(indicator, value) %>% 
  mutate(tx_rate = CXCA_TX / CXCA_SCRN_POS,
         period = paste0("FY", str_sub(fiscal_year, 3, 4), "Q", str_sub(period, 4))) %>% 
  group_by(snu1) %>% 
  mutate(order_var = sum(CXCA_SCRN, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ou_order = fct_reorder(snu1, order_var, .desc = T))
       

cust_breaks <- c(paste0("FY", rep(seq(18, 21, 1), each = 2), "Q", seq(2, 4, 2)))

ou_count <- df_cxca %>% 
  filter(period == "FY21Q4", operatingunit != "USAID") %>% 
  distinct(operatingunit) %>% 
  tally() %>% pull()

si_blue <- "#4974a5"
nudge_space  <-  0.15

a <-  df_cxca %>% 
  filter(period != "FY18Q2") %>% 
  mutate(positivity = CXCA_SCRN_POS/CXCA_SCRN) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = CXCA_SCRN), fill = golden_sand_light,
           width = 0.6) +
  geom_col(aes(y = CXCA_SCRN_POS), fill = golden_sand, width = 0.5, 
           position = position_nudge(x = nudge_space)) + 
  geom_col(aes(y = CXCA_TX), fill = si_blue, width = 0.5, position = position_nudge(x = -nudge_space)) +
  geom_text(aes(y = CXCA_SCRN_POS, label = percent(positivity, 1)),
            size = 12/.pt, family = "Source Sans Pro SemiBold", vjust = -0.5, hjust = -0.5) +
  facet_wrap(~snu1, scales = "free_y") +
  si_style_xline() +
  #geom_hline(yintercept = seq(1e5, 3e5, 1e5), color = "white", size = 0.5) +
  scale_y_continuous(position = "right", labels = label_number_si()) +
  coord_cartesian(expand = F) 

si_save("Images/ZMB_cxca_scrn.png")


b <- df_cxca %>% 
  filter(operatingunit == "USAID", period != "FY18Q2") %>% 
  ggplot(aes(x = period, y = tx_rate, group = operatingunit)) +
  geom_area(aes(y = 1), fill = "#bdcee2", alpha = 0.5)+
  geom_area(fill = si_blue, alpha = 0.5)+
  geom_line(color = si_blue, size = 2) +
  # geom_textpath(aes(label = "Treatment rate"), hjust = 0.95, vjust = -1, include_line = F)+
  geom_text(aes(label = "Treatment rate", y = 1, x = "FY21Q2"), vjust = -1,
            family = "Source Sans Pro", size = 12/.pt) +
  geom_hline(yintercept = 1, size = 0.25, linetype = "dotted") +
  geom_label(aes(label = percent(tx_rate, 1)), size = 12/.pt, family = "Source Sans Pro SemiBold") +
  si_style_xline() +
  # coord_cartesian(expand = F) +
  scale_x_discrete(expand = expansion(add = 0.25))+
  scale_y_continuous(expand = expansion(mult = 0), lim = c(0, 1.5)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank()) +
  labs(caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

a / b + plot_layout(heights = c(6, 2))