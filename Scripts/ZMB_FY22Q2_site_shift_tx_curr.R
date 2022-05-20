# PROJECT:  FY22Q2 PREP_NEW and PREP_CT by AGE
# AUTHOR:   T. Essam | K. Srinkanth
# PURPOSE:  provide {PREP visuals 
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
                            pattern = "Genie-SiteByIMs-Zambia-Daily-2022-05-16.zip")


# Grab metadata
pd <- source_info(file_genie, return = "period")
fy <- source_info(file_genie, return = "fiscal_year")
qtr <- source_info(file_genie, return = "quarter")  

#caption info for plotting
source <- source_info(file_genie)

# Transition list
sites <- tibble::tribble(
       ~facilityname,      ~snu1,      ~psnu,
          "Chinsaka",  "Luapula",   "Samfya",
         "Chitulika", "Muchinga",    "Mpika",
            "Fibale",  "Luapula",    "Mansa",
         "Fiyongoli",  "Luapula",    "Mansa",
         "Kaizya HP", "Northern", "Mpulungu",
           "Kalwala", "Muchinga", "Chinsali",
      "Kamuzwazi HP", "Northern",    "Mbala",
          "Kamwanya", "Muchinga",    "Mpika",
           "Kaombwe", "Muchinga",  "Nakonde",
       "Kapililonga", "Muchinga",    "Isoka",
        "Kasakalawe", "Northern", "Mpulungu",
     "Kasama Police", "Northern",   "Kasama",
       "Kaseshya HP", "Northern",    "Mbala",
  "Kasoma Bangweulu",  "Luapula",   "Samfya",
            "Katozi", "Muchinga",  "Nakonde",
      "Mbala Prison", "Northern",    "Mbala",
  "Milima Prison HP", "Northern",   "Kasama",
     "Mulenga Hills", "Northern",   "Kasama",
          "Musumali",  "Luapula",    "Mansa",
         "Nambuluma", "Muchinga", "Chinsali",
         "Namulundu", "Northern",   "Kasama",
         "Namwandwe",  "Luapula",    "Mansa",
     "Samfya Prison",  "Luapula",   "Samfya",
       "Sansamwenje", "Muchinga",    "Isoka",
             "Sumbu",  "Luapula",    "Mansa"
  )



# MUNGE TESTING SITES DISCOVER/ACTION HIV ---------------------------------

  df <- 
  read_msd(file_genie, convert_to_old_names = T) %>% 
  filter(mech_code %in% c("17399", "82075")) %>% 
  filter(indicator == "TX_CURR", standardizeddisaggregate == "Age/Sex/HIVStatus") 

df_sites <- 
  read_msd(file_genie, convert_to_old_names = T) %>% 
    filter(mech_code %in% c("17399", "82075")) %>% 
    filter(indicator == "TX_CURR", standardizeddisaggregate == "Total Numerator") %>% 
    select(orgunituid, sitename, mech_name, mech_code, indicator, standardizeddisaggregate, 
           fiscal_year, cumulative, qtr1, qtr2, qtr3, qtr4)


# Issue: In FY22Q1, a handful of sites transitioned from DISCOVER TO SAFE. 
# The question is, what would have ACTION HIVs performance looked like had
# they not absorbed these sites. What % of ACTIONs performance was due to these 
# DISCOVER sites and vice version.

# Let's first see if we can tag these sites in the MSD by looking at where
# sites flipped implenting partners

# Overall TX_CURR and Peds TX_CURR

df_site_switch <- 
  df_sites %>% 
  filter(fiscal_year >= 2021) %>% 
    mutate(mech_name = case_when(
      str_detect(mech_name, "Maintained") ~ "Action HIV",
      TRUE ~ "DISCOVER-H"
    )) %>% 
    group_by(sitename, orgunituid) %>% 
    mutate(site_count = n()) %>% 
  filter(site_count > 1) %>% 
  mutate(site_switch = ifelse(mech_name == lag(mech_name), 0, 1)) %>% 
  fill(site_switch, .direction = "up") %>% 
  filter(site_switch == 1, site_count < 3) %>% 
  ungroup()

site_list_switch <- df_site_switch %>% distinct(orgunituid) %>% pull()

# Calculate TX_CURR by partner across time.
# Remap the DISCOVER H sites back into it's portfolio

 df_tx <- 
   df %>% 
    filter(fiscal_year >= 2021) %>% 
    mutate(mech_name = case_when(
      str_detect(mech_name, "Maintained") ~ "Action HIV",
      TRUE ~ "DISCOVER-H")
      ) %>% 
    mutate(tag_sites = ifelse(orgunituid %in% site_list_switch, 1, 0),
           mech_name_old = case_when(
             tag_sites == 1 ~ "Swapped Sites",
             TRUE ~ mech_name
           )
    )

  df_tx_agg <- df_tx %>% 
    reshape_msd("semi-wide") %>% 
    group_by(trendscoarse, mech_name_old, period) %>% 
    summarise(tx_curr = sum(results, na.rm = T)) %>% 
    filter(str_detect(period, "Q")) %>% 
    mutate(time_tag = case_when(
      str_detect(period, "22") & mech_name_old == "Swapped Sites" ~ "Action HIV new sites", 
      str_detect(period, "21") & mech_name_old == "Swapped Sites" ~ "DISCOVER-H transfers",
      TRUE ~ mech_name_old
    ),
    parent_tag = ifelse(str_detect(time_tag, "Action"), "Action HIV", "DISCOVER-H")) %>% 
    group_by(period, parent_tag, trendscoarse) %>% 
    mutate(tx_curr_tot = sum(tx_curr)) %>% 
    ungroup() %>% 
    mutate(tx_share = tx_curr / tx_curr_tot,
           fill_colors = case_when(
             time_tag == "Action HIV new sites" ~ scooter_med,
             time_tag == "Action HIV" ~ scooter,
             time_tag == "DISCOVER-H transfers" ~ "#c33c4c",
             time_tag == "DISCOVER-H" ~ "#7f001c"
           )
          ) 
  
  # Now

  # Under 15s 5K max, 15+ 121330
  
 a <-  df_tx_agg %>% 
    mutate(ymax = ifelse(trendscoarse == "<15", 5e3, 1.32e5),
           ymin = 0,
           mech_order = fct_relevel(parent_tag, c("DISCOVER-H", "Action HIV")),
           time_tag = fct_relevel(time_tag, c("Action HIV new sites", "Action HIV",
                                                "DISCOVER-H transfers", "DISCOVER-H"))) %>%
    ggplot(aes(x = period, y = tx_curr, fill = time_tag)) +
    geom_blank(aes(y = ymin)) +
    geom_blank(aes(y = ymax)) +
    geom_col(alpha = 0.85)+
    facet_wrap(trendscoarse ~ mech_order, scales = "free_y") +
    scale_y_continuous(labels = comma) +
    geom_text(data = . %>% filter(mech_name_old == "Swapped Sites"),
              aes(x = period, y = tx_curr, label = percent(tx_share, 1)),
              size = 9/.pt, family = "Source Sans Pro",
              position = position_stack(vjust = 7)) +
    geom_text(data = . %>% filter(mech_name_old == "Swapped Sites"),
              aes(x = period, y = tx_curr, label = comma(tx_curr)),
              size = 9/.pt, family = "Source Sans Pro",
              position = position_stack(vjust = 8)) +
    geom_text(data = . %>% filter(mech_name_old != "Swapped Sites"),
              aes(x = period, y = tx_curr, label = comma(tx_curr)),
              size = 9/.pt, family = "Source Sans Pro", color = "white",
              position = position_stack(vjust = 0.85)) +
    scale_fill_manual(values = c(
      "Action HIV new sites" = "#5DB6d6",
      "Action HIV" = "#005c78",
      "DISCOVER-H transfers" = "#EF646F",
      "DISCOVER-H" = "#960B2C")) +
    si_style() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(),
          legend.position = "none")
  
  si_save("Graphics/ZMB_discover_actionhiv_swapped_sites_analysis.svg", scale = 1.25)
  
  
b <- 
  df_tx_agg %>% 
    filter(mech_name_old == "Swapped Sites") %>% 
    ggplot(aes(x = period, y = tx_curr, fill = time_tag)) +
    geom_col() +
    facet_wrap(~trendscoarse, scales = "free_y", nrow = 2) +
    scale_fill_manual(values = c(
      "Action HIV new sites" = "#5DB6d6",
      "DISCOVER-H transfers" = "#EF646F")) +
   geom_text(aes(label = comma(tx_curr)),
             size = 9/.pt, 
             family = "Source Sans Pro", 
             color = grey90k) +
    si_style() +
    theme(axis.text.y = element_blank(),
          legend.position = "none") +
    labs(x = NULL, y = NULL)

a + b +
  plot_layout(widths = c(2, 1))

