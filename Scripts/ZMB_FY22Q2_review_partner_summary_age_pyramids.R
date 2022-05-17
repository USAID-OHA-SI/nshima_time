# PROJECT:  FY22Q2 Zambia Mission Review - Age / IP breakdown
# AUTHOR:   T. Essam | K. Srinkanth adopted from A. Chafetz
# PURPOSE:  provide Age-band level analysis for Zambia FY22Q2
# LICENSE:  MIT
# DATE:     2022-05-17
# UPDATE:   2022-05-17


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


# AGE BREAKDOWN -----------------------------------------------------------

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

# AGE SEX MUNGE -------------------------------------------------------------

#limit to correct age/sex disaggs
df_agesex <- df_sub %>% 
  inner_join(df_disaggs %>% filter(indicator != "OVC_SERV")) %>% 
  filter(ageasentered %ni% c("Unknown Age", "<10"))

#aggregate
df_agesex <- df_agesex %>% 
  group_by(fiscal_year, partner, indicator, ageasentered, sex) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
  complete(indicator, nesting(partner), fill = list(fiscal_year = fy, ageasentered = "<01"))

df_agesex <- df_agesex %>% 
  adorn_achievement(qtr)

df_agesex_viz <- df_agesex %>% 
  mutate(plot_targets = ifelse(sex == "Female", -targets, targets),
         plot_cumulative = ifelse(sex == "Female", -cumulative, cumulative)) %>% 
  group_by(partner, indicator) %>% 
  mutate(balance = max(targets, cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(curr_target = ifelse(indicator %in% snapshot_ind, 1 * targets, (qtr/4)*targets),
         curr_target = ifelse(sex == "Female", -curr_target, curr_target),
         indicator = factor(indicator, ind_sel)) 
  # filter(ageasentered %ni% c("50-54", "55-59", "60-64", "65+"))


# VIZ - AGE/SEX -----------------------------------------------------------

plot_agesex <- function(df, ptnr, age = "", export = TRUE){
  v <- 
    df %>% 
    filter(partner == {ptnr}) %>% 
    ggplot(aes(y = ageasentered)) +
    geom_blank(aes(balance)) + 
    geom_blank(aes(-balance)) + 
    geom_col(aes(x = plot_targets), fill = trolley_grey_light) +
    geom_col(aes(x = plot_cumulative, fill = sex), alpha = 0.75) +
    geom_errorbar(aes(xmin = curr_target, xmax = curr_target), size = .4, color = grey90k) +
    geom_vline(aes(xintercept = 0), size = 2, color = "white") +
    facet_wrap(~indicator, scales = "free_x") +
    scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa)) +
    scale_x_continuous(labels = ~ comma(abs(.))) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("{pd} Zambia | {ptnr}") %>% toupper,
         subtitle = glue("Partner cumulative **<span style = 'color:{moody_blue};'> Female</span>/<span style = 'color:{genoa};'>Male</span>** results by age against **<span style = 'color:{trolley_grey};'>targets</span>**<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal (vertical bar) for 100% at Q{qtr} (snapshot indicators pegged to year end target 100%)</span>"),
         caption = glue("No targets for HTS_SELF
                        Source: {source}
                        US Agency for International Development")) +
    theme(#panel.spacing.x = unit(1, "lines"),
      # panel.spacing.y = unit(0, "lines"),
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(size = 7/.pt),
      axis.text.y = element_text(size = 7/.pt),
      # plot.subtitle = element_markdown(),
      strip.background = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.ticks = element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    si_style_xgrid(facet_space = 0.5) +
    theme(plot.subtitle = element_markdown())
  
  
  if(export == TRUE){
    si_save(glue("Images/{pd}_ZMB_Partner-AgeSex_{ptnr}{age}.png"), v, scale = 1.75)
  } else {
    return(v)
  }
}


partner_list <- 
  df_agesex_viz %>% 
  filter(str_detect(partner, "Action|DISCOVER|SAFE")) %>% 
  distinct(partner) %>% 
  pull()

plot_agesex(df_agesex_viz, "SAFE", export = FALSE)

plot_agesex(df_agesex_viz %>% 
              filter(ageasentered %in% ), 
            "SAFE", FALSE)

walk(partner_list, ~plot_agesex(df_agesex_viz, .x, export = TRUE))

# Under 24 filter
under_24s <- c("<01", "1-4", "5-9", "10-14", "15-19", "20-24")

walk(partner_list, ~plot_agesex(df_agesex_viz %>% filter(ageasentered %in% under_24s),
                                .x, age = "under_24s", export = T))

# Check the numbers


            