# PROJECT:  nshima-time
# AUTHOR:   K. Srikanth, T. Essam | USAID
# PURPOSE:  develop index testing cascade
# LICENSE:  MIT
# DATE:     2022-05-19
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

#MUNGE --------------------------------------------------------------------

#look at USAID testing
df_mods <- df %>% 
  filter(funding_agency == "USAID",
         indicator == "HTS_INDEX",
         standardizeddisaggregate %in% c("1:Age/Sex/IndexCasesOffered", 
                                         "2:Age/Sex/IndexCasesAccepted",
                                         "3:Age Aggregated/Sex/Contacts",
                                         "4:Age/Sex/Result")) %>% 
  count(standardizeddisaggregate, otherdisaggregate)

df_cascade <- df_mods %>% 
  filter(fiscal_year == curr_fy) %>%
  mutate(indicator = case_when(indicator == "HTS_INDEX" & modality == "Index" ~ "HTS_INDEX_FAC",
                               indicator == "HTS_INDEX" & modality == "IndexMod" ~ "HTS_INDEX_COM",
                               TRUE ~ indicator)) %>% 
  group_by(funding_agency, indicator, standardizeddisaggregate, otherdisaggregate, statushiv, fiscal_year) %>%
  summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
  ungroup()
  reshape_msd(direction = "semi-wide", qtrs_keep_cumulative = T)
  
df_cascade <- df_cascade %>% 
 # str_remove(standardizeddisaggregate, "")
  mutate(standardizeddisaggregate = recode(standardizeddisaggregate,
                                           "1:Age/Sex/IndexCasesOffered" = "Offered",
                                           "2:Age/Sex/IndexCasesAccepted" = "Accepted",
                                           "3:Age Aggregated/Sex/Contacts" = "Contacts",
                                           "4:Age/Sex/Result" = "Results"),
         statushiv = case_when(is.na(otherdisaggregate) & statushiv == "Negative" ~ "Documented Negative",
                               TRUE ~ statushiv),
         otherdisaggregate = ifelse(statushiv == "Documented Negative", "", otherdisaggregate),
         statushiv = str_c(otherdisaggregate, statushiv, sep = " ")) %>% 
  reshape_msd(direction = "semi-wide", qtrs_keep_cumulative = T) %>%
  fill_targets() 
  


df_cascade <- df_cascade %>% 
  mutate(
    achv = ifelse(targets > 0, results / targets, NA_real_),
    indic_colors = case_when(
      standardizeddisaggregate == "Offered" ~ "#877ec9",
      standardizeddisaggregate == "Accepted" ~ "#b5aaf9",
      standardizeddisaggregate == "Contacts" ~ "#005e7a",
      statushiv == "Known at Entry Positive" ~ scooter_med,
      statushiv == "Newly Identified Negative" ~ denim_light,
      statushiv == "Newly Identified Positive" ~ scooter,
      statushiv == " Documented Negative" ~ usaid_medblue
    ),
    cascade = case_when(
      str_detect(indicator, "COM") ~ "Community",
      str_detect(indicator, "FAC") ~ "Facility",
      TRUE ~ "3rd 90"
    )
  )

df_cascade %>%
  mutate(standardizeddisaggregate = fct_relevel(standardizeddisaggregate, c(
    "Offered", "Accepted", "Contacts", "Results"
  )),
  statushiv = fct_relevel(statushiv, c(
    "Newly Identified Positive", "Newly Identified Negative", "Known at Entry Positive", "Documented Negative"
  ))) %>% 
  filter(period == "FY22Q2") %>% 
  ggplot(aes(x = standardizeddisaggregate, fill = indic_colors)) +
  # geom_col(aes(data = . %>% filter(statushiv == "Newly Identified Negative"),
  #              y = targets), fill = trolley_grey_light, width = 0.5) +
  geom_col(aes(y = results, fill = "#005e7a"), width = 0.7, alpha = 0.3
  ) +
  geom_col(aes(y = results), width = 0.7
           , position = "dodge"
           ) +
  geom_text(aes(y = results, label = comma(results, 1)),
            size = 12 / .pt, vjust = -0,
            family = "Source Sans Pro"
  ) +
  facet_wrap(~cascade, scales = "free_y") +
  scale_fill_identity() +
  #si_style_ygrid() +
  scale_y_continuous(labels = label_number_si()) +
 # scale_fill_identity() +
 # facet_wrap(~cascade, scales = "free") +
  si_style_ygrid(text_scale = 1.25) +
labs(
  x = NULL, y = NULL, title = glue("INDEX TESTING CASCADE - {curr_pd}") %>% toupper(),
  subtitle = glue("{curr_pd} results numbers listed above colored bar"),
  caption = data_source
) 


si_save("Graphics/index-cascade.svg")
