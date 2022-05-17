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

genie_path <- file.path(si_path(), "Genie-PSNUByIMs-Zambia-Daily-2022-05-16_ALL.zip")

msd_source <- source_info(genie_path)
curr_pd <- source_info(genie_path, return = "period")
curr_fy <- source_info(genie_path, return = "fiscal_year")
curr_qtr <- source_info(genie_path, return = "quarter")

#indicators used
# c("HTS_TST_POS", "OVC_SERV", "PMTCT_EID", "TB_PREV", "TB_STAT", "TB_STAT_POS", "TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW", "TX_NEW", "TX_PVLS", "TX_PVLS", "TX_PVLS_D")

# IMPORT ------------------------------------------------------------------

df <- read_msd(genie_path)

# PERIODS -----------------------------------------------------------------

full_pds <- (min(df$fiscal_year) - 1) %>% 
  paste0("-10-01") %>% 
  as_date() %>% 
  seq.Date(convert_qtr_to_date(curr_pd), by = "3 months") %>% 
  convert_date_to_qtr()

pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")

# FUNCTON ---------------------------------------------------------------------

indic_collapse <- function(df, ...) {
  df %>%
    bind_rows(df %>% mutate(snu1 = "PEPFAR")) %>%
    group_by(fiscal_year, indicator, ...) %>%
    summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>%
    reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T)
}


# Fills targets based on an extracted fiscal year
# and custom groupings
fill_targets <- function(df, ...) {
  df %>%
    mutate(fy = substr(period, 3, 4)) %>%
    group_by(fy, ...) %>%
    fill(targets, .direction = "down") %>%
    filter(period %ni% c("FY20", "FY21", "FY22"))
}


create_cmlt <- function(df, ...) {
  df %>%
    group_by(fy, snu1, ...) %>%
    mutate(cmlt_result = cumsum(results)) %>%
    ungroup() %>%
    mutate(
      achv = (cmlt_result / targets),
      snu1 = str_remove_all(snu1, " Province"),
      snu1_order = fct_reorder(snu1, targets, .desc = T),
    )
}

return_age_disag <- function(df, disag, indic) {
  df %>%
    filter(
      indicator %in% {{ indic }},
      standardizeddisaggregate == {{ disag }},
      fiscal_year == curr_fy
    ) %>%
    group_by(fiscal_year, indicator, ageasentered, sex, snu1) %>%
    summarise(across(matches("targ|cum"), sum, na.rm = TRUE)) %>%
    filter(ageasentered %ni% c("<10", "<01")) %>%
    mutate(
      achv = cumulative / targets,
      new_targ = ifelse(sex == "Female", targets, -targets),
      new_cmlt = ifelse(sex == "Female", cumulative, -cumulative),
      label_achv = ifelse(achv < 1, achv, NA_real_),
      fill_val = ifelse(sex == "Female", moody_blue, genoa),
      snu1 = str_remove_all(snu1, " Province")
    )
}

return_bar_sort <- function(df) {
  df %>%
    mutate(
      order_var = str_c(ageasentered, sex),
      achv_chunk = ifelse(achv >= 1, 1, 0)
    ) %>%
    arrange(sex, ageasentered, desc(achv_chunk), cumulative) %>%
    group_by(sex, ageasentered) %>%
    mutate(
      sort_order = row_number(),
      hline_var = case_when(
        lag(achv_chunk, n = 1) != achv_chunk ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    mutate(
      snu1_order = reorder_within(snu1, sort_order, order_var),
      newvar = case_when(
        hline_var == 1 ~ factor(snu1_order),
        TRUE ~ snu1_order
      )
    )
}

data_source <- glue("{msd_source}\n Created by: {authors} | SI Team")

authors <- c("Tim Essam")

cust_seq <- seq(0, 1, .25)[2:5]
cust_label <- cust_seq %>% percent()

# CASCADE ----------------------------------------------------------------------

cascade <- function(df, pop) {
  
  
  
}

df_cascade <- df %>%
  filter(
    indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_NET_NEW", "TX_CURR", "TX_PVLS"),
    standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus",
                                    "Age/Sex/Indication/HIVStatus"),
    fiscal_year == curr_fy,
    funding_agency == "USAID"
  ) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
  group_by(funding_agency, indicator, trendscoarse, fiscal_year) %>%
  summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>%
  reshape_msd(direction = "semi-wide", qtrs_keep_cumulative = T) %>%
  fill_targets() %>%
  filter(trendscoarse == "<15") %>% 
  mutate(
    achv = ifelse(targets > 0, results / targets, NA_real_),
    indic_colors = case_when(
      indicator == "HTS_TST" ~ "#877ec9",
      indicator == "HTS_TST_POS" ~ "#b5aaf9",
      indicator == "TX_NEW" ~ golden_sand_light,
      indicator == "TX_NET_NEW" ~ golden_sand_light,
      indicator == "TX_CURR" ~ golden_sand,
      indicator == "TX_PVLS_D" ~ scooter_med,
      indicator == "TX_PVLS" ~ scooter
    ),
    cascade = case_when(
      str_detect(indicator, "HTS") ~ "1st 90",
      str_detect(indicator, "TX_NE") ~ "2nd 90",
      TRUE ~ "3rd 90"
    )
  )

df_cascade <-
  df_cascade %>%
  bind_rows(df_cascade %>% filter(indicator == "HTS_TST_POS") %>% mutate(cascade = "2nd 90")) %>%
  mutate(indicator = fct_relevel(indicator, c(
    "HTS_TST", "HTS_TST_POS", "TX_NEW",
    "TX_NET_NEW", "TX_CURR", "TX_PVLS_D",
    "TX_PVLS"
  )))

df_cascade %>%
  filter(period == max(period)) %>% 
  ggplot(aes(x = indicator, fill = indic_colors)) +
  geom_col(aes(y = targets), fill = trolley_grey_light, width = 0.5) +
  geom_col(aes(y = results), width = 0.5, position = position_nudge(x = 0.1)) +
  geom_text(aes(y = results, label = comma(results)),
            size = 12 / .pt, vjust = -0.45,
            family = "Source Sans Pro",
            position = position_nudge(x = 0.1)
  ) +
  geom_label(aes(y = results, label = percent(achv, 1)),
             size = 9 / .pt, vjust = 1.2,
             family = "Source Sans Pro",
             position = position_nudge(x = 0.1), fill = "white"
  ) +
  geom_text(
    data = . %>% filter(indicator == "HTS_TST"), aes(y = targets, label = "FY22 Targets"),
    size = 12 / .pt, family = "Source Sans Pro", hjust = 0.4
  ) +
  scale_y_continuous(labels = comma, expand = c(0.02, 1)) +
  scale_fill_identity() +
  facet_wrap(~cascade, scales = "free") +
  si_style_ygrid(text_scale = 1.25) +
  labs(
    x = NULL, y = NULL, title = glue("ZAMBIA PEDIATRIC CASCADE - {curr_pd} RESULTS TO TARGETS (GRAY BARS)"),
    subtitle = glue("{curr_pd} results numbers listed above colored bar, achievement in box below"),
    caption = data_source
  )

si_save("Images/ZMB_FY22Q2_CASCADE.png")
si_save("Graphics/ZMB_FY22Q2_CASCADE.svg")

