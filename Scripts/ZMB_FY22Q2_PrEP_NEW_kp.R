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



# GLOBAL VARIABLES --------------------------------------------------------

#genie_path <- file.path(si_path(), "Genie-PSNUByIMs-Zambia-Daily-2022-05-16_ALL.zip")
  genie_path  <- return_latest(si_path(), "PSNU_IM_FY20-22_.*_v1_1_Zambia")
  
  msd_source <- source_info(genie_path)
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  curr_qtr <- source_info(genie_path, return = "quarter")

  df <- read_msd(genie_path) %>% filter(indicator %in% c("PrEP_NEW"))  
  
  df %>% 
    count(standardizeddisaggregate, otherdisaggregate, categoryoptioncomboname, 
          fiscal_year, indicator) %>% prinf()
  

# MUNGE -------------------------------------------------------------------

  df_kp <- 
    df %>% 
    filter(standardizeddisaggregate == "KeyPopAbr",
           funding_agency == "USAID") %>%
    group_by(otherdisaggregate, fiscal_year, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd("quarters") %>% prinf()
    select(-results_cumulative)


# Plot -------------------------------------------------------------------

  width <- .6  
  offset <- 0.15
  ymax = 3.6e3
  ymax2 = 950
  
  df_kp %>%
    mutate(ymax = ifelse(otherdisaggregate %in% c("FSW", "MSM"), ymax, ymax2),
           achv = case_when(
             period %in% c("FY21Q4", curr_pd) ~ results/targets, 
             TRUE ~ NA_real_
           )
          )%>% 
    group_by(otherdisaggregate) %>% 
    mutate(qtrs_run = row_number()) %>% 
    ungroup() %>% 
    filter(qtrs_run > 4) %>% 
    ggplot(aes(x = period)) +
    geom_blank(aes(ymax = ymax)) +
    geom_col(aes(y = targets), fill = grey10k, width = width ) +
    geom_col(aes(y = results), fill = scooter_med, width = width,
             position = position_nudge(x = offset)) +
    geom_text(aes(y = results, label = percent(achv, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro",
              position = position_nudge(x = offset), 
              vjust = 0.1)+
    facet_wrap(~otherdisaggregate, scales = "free_y") +
    si_style_ygrid(facet_space = 0.75) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         caption = glue("Source: {msd_source}"))

  si_save(glue("Graphics/{curr_pd}_ZAM_PrEP_NEW_KP.svg"))  
  

  