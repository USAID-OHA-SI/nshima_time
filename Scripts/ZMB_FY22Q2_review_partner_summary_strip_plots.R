# PROJECT:  FY22Q1 Partner Review
# AUTHOR:   T. Essam adopted from A. Chafetz
# PURPOSE:  provide PSNU level analysis for Zambia FY22Q1
# LICENSE:  MIT
# DATE:     2021-09-03
# UPDATE:   2022-01-25


# SOURCE META DATA --------------------------------------------------------

# FY22Q2 Genie Pull USAID ONLY
# DATIM data as of: 01/25/2022 00:11:08 UTC
# Genie report updated: 01/25/2022 03:42:05 UTC
# Current period(s): 2021 Target,  2021 Q1,  2021 Q2,  2021 Q3,  2021 Q4,  2022 Target,  2022 Q1,  2023 Target
# stub = PSNUByIMs-MultipleOUs-Daily-2022-01-25
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
library(gisr)
library(sf)
library(selfdestructin5)
library(gt)

# Load getrdone functions while it's a WIP
devtools::load_all(path = "../getrdone/")


# GLOBALS -----------------------------------------------------------------

load_secrets()

merdata <- file.path(glamr::si_path("path_msd"), "Genie")

file_genie <- return_latest(folderpath = merdata,
                            pattern = "Genie-PSNUByIMs-Zambia-Daily-")

# Grab metadata
pd <- source_info(file_genie, return = "period")
fy <- source_info(file_genie, return = "fiscal_year")
qtr <- source_info(file_genie, return = "quarter")  

#caption info for plotting
source <- source_info(file_genie)

source("Scripts/fy22_partner_reivew_functions.R")

# Fetch template info and create mech crosswalk
df_ptnr <- fetch_clinical() %>% 
  create_clin_cw() %>% 
  distinct(IM) %>% 
  filter(str_detect(IM, ("ALL USAID|EQUIP"), negate = T)) %>% 
  mutate(partner = gsub("\\(|[0-9]|\\)", "", IM) %>% trimws,
         mech_code = gsub("[A-Za-z]|-|\\)|\\(", "", IM) %>% trimws) %>% 
  select(-IM) %>% 
  as_tibble()

# Grab indicators
ind_sel <- indic_ptner(qtr)

# Read in genie
psnu_im <- read_msd(file_genie, convert_to_old_names = T)

# MUNGE -------------------------------------------------------------------

#subset to key indicators
df_sub <- psnu_im %>% 
  filter(fundingagency == "USAID",
         fiscal_year == fy,
         indicator %in% ind_sel) %>% 
  clean_indicator()

#limit to select partners with preferred names
df_sub <-  inner_join(df_sub, df_ptnr)

# MUNGE - NAT/SNU ACHIEVEMENT ---------------------------------------------

#aggregate to regional level
df_achv <- df_sub %>% 
  bind_rows(df_sub %>% 
              mutate(psnu = "NATIONAL",
                     psnuuid = "NATIONAL")) %>% 
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  group_by(fiscal_year, partner, psnu, psnuuid, indicator) %>% 
  summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
            .groups = "drop")

#calculate achievement
df_achv <- df_achv %>% 
  adorn_achievement(qtr)

#viz adjustments
df_achv_viz <- df_achv %>% 
  complete(indicator, nesting(partner), fill = list(fiscal_year = fy, psnu = "NATIONAL")) %>% 
  mutate(natl_achv = case_when(psnu == "NATIONAL" ~ achievement),
         achievement = ifelse(psnu == "NATIONAL", NA, achievement),
         indicator = factor(indicator, ind_sel),
         baseline_pt_1 = 0,
         baseline_pt_2 = .25,
         baseline_pt_3 = .5,
         baseline_pt_4 = .75,
         baseline_pt_5 = 1,
  )

#adjust facet label to include indicator and national values
df_achv_viz <- df_achv_viz %>% 
  mutate(ind_w_natl_vals = case_when(psnu == "NATIONAL" & is.na(targets) ~ 
                                       glue("**{indicator}**<br><span style = 'font-size:9pt;'>No MER reporting</span>"),
                                     psnu == "NATIONAL" ~ 
                                       glue("**{indicator}**<br><span style = 'font-size:9pt;'>{comma(cumulative, 1)} / {comma(targets, 1)}</span>"))) %>% 
  group_by(partner, indicator) %>% 
  fill(ind_w_natl_vals, .direction = "updown") %>% 
  ungroup() %>% 
  arrange(partner, indicator) %>% 
  mutate(ind_w_natl_vals = fct_inorder(ind_w_natl_vals)) %>% 
  clean_psnu()  


# VIZ function ------------------------------------------------------------

plot_achv <- function(ptnr, export = TRUE){
  df_achv_viz %>% 
    filter(partner == {ptnr}) %>% 
    mutate(ind_w_natl_vals = fct_drop(ind_w_natl_vals),
           ind_w_natl_vals = fct_inorder(ind_w_natl_vals)) %>% 
    ggplot(aes(achievement, indicator, color = achv_color)) +
    geom_blank() +
    geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
    geom_point(aes(baseline_pt_1), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_2), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_3), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_4), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_5), shape = 3, color = "#D3D3D3") +
    geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .4, size = 3) +
    geom_point(aes(natl_achv), size = 8, alpha = .8, na.rm = TRUE) +
    geom_text(aes(natl_achv, label = percent(natl_achv, 1)), na.rm = TRUE,
              color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limit=c(0,1.1), oob=scales::squish) +
    scale_color_identity() +
    facet_wrap(~ind_w_natl_vals, scales = "free_y", drop = TRUE) +
    labs(x = NULL, y = NULL,
         title = glue("{pd} Zambia | {ptnr}") %>% toupper,
         subtitle = glue("Partner achievement nationally (large, labeled points) with psnu reference points<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for 100% at Q{qtr} (snapshot indicators pegged to year end target 100%)</span>"),
         caption = glue("Target achievement capped at 110%
                        Source: {source}
                        US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"),
          plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  if(export == TRUE)
    si_save(glue("Images/{pd}_ZMB_Partner-Achievement_{ptnr}.png"))
}



# VIZ output --------------------------------------------------------------

partner_list <- df_achv_viz %>% 
  #filter(str_detect(partner, "Action|DISCOVER|SAFE")) %>% 
  distinct(partner) %>% pull()

plot_achv("SAFE", export = T)

walk(partner_list, plot_achv)

# REPEAT FOR PEDS ---------------------------------------------------------

#  AD HOC REQUEST FOR PEDS ANALYSIS ---------------------------------------

df_peds_disaggs <- 
  df_disaggs %>% 
  filter(str_detect(indicator, "PrEP|VMMC|HTS_SELF", negate = T)) %>% 
  add_row(indicator = "PMTCT_EID", standardizeddisaggregate = "Total Numerator") %>% 
  add_row(indicator = "PMTCT_EID_D", standardizeddisaggregate = "Total Denominator") %>% 
  arrange(indicator)

peds_indic <- df_peds_disaggs %>% distinct(indicator) %>% pull()

#subset to key indicators
df_sub_peds <- psnu_im %>% 
  filter(fundingagency == "USAID",
         fiscal_year == fy,
         indicator %in% peds_indic) %>% 
  clean_indicator()

#limit to select partners with preferred names
df_sub_peds <-  inner_join(df_sub_peds, df_ptnr %>% filter(str_detect(partner, "DISC|SAFE|Action")))


#limit to correct age/sex disaggs
df_peds <- df_sub_peds %>% 
  inner_join(df_sub_peds %>% filter(indicator != "OVC_SERV")) %>% 
  filter(trendscoarse == "<15") %>% 
  mutate(drop_var = case_when(
    indicator == "PMTCT_EID" & standardizeddisaggregate == "Age" ~ 1, 
    TRUE ~ 0)
  ) %>% 
  filter(drop_var == 0)


#aggregate to regional level
df_achv <- df_peds %>% 
  bind_rows(df_peds %>% 
              mutate(psnu = "NATIONAL",
                     psnuuid = "NATIONAL")) %>% 
  filter(standardizeddisaggregate != "Age/Sex/ARVDispense/HIVStatus") %>% 
  group_by(fiscal_year, partner, psnu, psnuuid, indicator, numeratordenom) %>% 
  summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
            .groups = "drop")

#calculate achievement
df_achv <- df_achv %>% 
  adorn_achievement(qtr)

#viz adjustments
df_achv_viz <- df_achv %>% 
  complete(indicator, nesting(partner), fill = list(fiscal_year = fy, psnu = "NATIONAL")) %>% 
  mutate(natl_achv = case_when(psnu == "NATIONAL" ~ achievement),
         achievement = ifelse(psnu == "NATIONAL", NA, achievement),
         indicator = factor(indicator, c(peds_indic)),
         baseline_pt_1 = 0,
         baseline_pt_2 = .25,
         baseline_pt_3 = .5,
         baseline_pt_4 = .75,
         baseline_pt_5 = 1,
  )

#adjust facet label to include indicator and national values
df_achv_viz <- df_achv_viz %>% 
  mutate(ind_w_natl_vals = 
           case_when(psnu == "NATIONAL" & is.na(targets) ~ 
                       glue("**{indicator}**<br><span style = 'font-size:9pt;'>No MER reporting</span>"),
                     psnu == "NATIONAL" ~ 
                       glue("**{indicator}**<br><span style = 'font-size:9pt;'>{comma(cumulative, 1)} / {comma(targets, 1)}</span>"))) %>% 
  group_by(partner, indicator) %>% 
  fill(ind_w_natl_vals, .direction = "updown") %>% 
  ungroup() %>% 
  arrange(partner, indicator) %>% 
  mutate(ind_w_natl_vals = fct_inorder(ind_w_natl_vals)) %>% 
  clean_psnu()  %>% 
  filter(str_detect(indicator, "PMTCT", negate = T))


plot_achv <- function(ptnr, export = TRUE){
  df_achv_viz %>% 
    filter(partner == {ptnr}) %>% 
    mutate(ind_w_natl_vals = fct_drop(ind_w_natl_vals),
           ind_w_natl_vals = fct_inorder(ind_w_natl_vals)) %>% 
    ggplot(aes(achievement, indicator, color = achv_color)) +
    geom_blank() +
    geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
    geom_point(aes(baseline_pt_1), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_2), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_3), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_4), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_5), shape = 3, color = "#D3D3D3") +
    geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .4, size = 3) +
    geom_point(aes(natl_achv), size = 8, alpha = .8, na.rm = TRUE) +
    geom_text(aes(natl_achv, label = percent(natl_achv, 1)), na.rm = TRUE,
              color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limit=c(0,1.1), oob=scales::squish) +
    scale_color_identity() +
    facet_wrap(~ind_w_natl_vals, scales = "free_y", drop = TRUE) +
    labs(x = NULL, y = NULL,
         title = glue("{pd} Zambia PEDS (<15) | {ptnr}") %>% toupper,
         subtitle = glue("Partner achievement nationally (large, labeled points) with psnu reference points<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal is 25% at Q{qtr} (snapshot indicators pegged to year end target 100%)</span>"),
         caption = glue("Target achievement capped at 110%
                        Source: {source}
                        US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"),
          plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  if(export == TRUE)
    si_save(glue("Images/{pd}_ZMB_Partner-Achievement_{ptnr}_PEDS.png"))
}



plot_achv("SAFE", export = T)
walk(partner_list, plot_achv)



