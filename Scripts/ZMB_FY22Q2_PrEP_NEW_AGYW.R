# PROJECT:  nshima-time - PrEP_NEW for AGYW
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
library(glitrpatch)



# GLOBAL VARIABLES --------------------------------------------------------

#genie_path <- file.path(si_path(), "Genie-PSNUByIMs-Zambia-Daily-2022-05-16_ALL.zip")
  genie_path  <- return_latest(si_path(), "PSNU_IM_FY20-22_.*_v1_1_Zambia")
  
  msd_source <- source_info(genie_path)
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  curr_qtr <- source_info(genie_path, return = "quarter")
  data_source <-
    glamr::source_info(genie_path) %>% glue::glue(., '
  Created by: USAID OHA SI Team')
  
  df <- read_msd(genie_path) %>% 
    filter(indicator %in% c("PrEP_NEW")) 

# MUNGE -------------------------------------------------------------------

  #Comparison: 15-24 year females (AGYW) compared to 25+ females
  df_prep <- 
    df %>% 
    filter(sex == "Female") %>% 
    mutate(age = case_when(
      ageasentered %in% c("15-19", "20-24") ~ "AGYW (15-24)",
      TRUE ~ "Females 25+"
      )
    ) %>% 
    group_by(age, indicator, fiscal_year) %>% 
    summarise(across(c(targets, starts_with("cum")), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(achv = cumulative / targets,
           fill_color = ifelse(age == "AGYW (15-24)", golden_sand, "#4076f2"))

  
  width <- .6  
  offset <- 0.15

  df_prep %>% 
    ggplot(aes(x = as.factor(fiscal_year), group = fill_color)) +
    geom_col(aes(y = targets), fill = grey10k, width = width ) +
    geom_col(aes(y = cumulative, fill = fill_color), width = width,
             position = position_nudge(x = offset)) +
    geom_text(aes(y = cumulative, label = percent(achv, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro",
              position = position_nudge(x = offset), 
              vjust = -0.2) +
    facet_wrap(~age) +
    si_style_ygrid(facet_space = 0.75) +
    scale_y_continuous(labels = comma, sec.axis = dup_axis()) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL, 
         caption = glue("Source: {msd_source}"),
         title = "FY20 - FY22 PrEP INITATIONS AMONG AGWY AND FEMALES 25+")
  
  si_save(glue("Graphics/{curr_pd}_ZAM_PrEP_NEW_AGWY.svg"))  
  
  
  