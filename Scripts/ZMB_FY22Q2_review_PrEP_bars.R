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
                            pattern = "Genie-PSNUByIMs-Zambia-Daily-")

# Grab metadata
pd <- source_info(file_genie, return = "period")
fy <- source_info(file_genie, return = "fiscal_year")
qtr <- source_info(file_genie, return = "quarter")  

#caption info for plotting
source <- source_info(file_genie)


# MUNGE PREP --------------------------------------------------------------

 df_prep <- read_msd(file_genie, convert_to_old_names = T) %>% 
  filter(indicator %in% c("PrEP_NEW", "PrEP_CT"))


 df_prep_long <- 
   df_prep %>%
   mutate(mech_code = ifelse(mech_code == "18304", "82075", mech_code), 
          mech_name = ifelse(mech_name == "EQUIP", "Action HIV", mech_name), 
          mech_name = ifelse(mech_name == "Maintained Epidemic Control of HIV", 
                             "Action HIV", mech_name),
          mech_name = ifelse(str_detect(mech_name, "DISCOVER"), "DISCOVER", mech_name)
          ) %>% 
   filter(standardizeddisaggregate == "Age/Sex",
          fundingagency == "USAID") %>%
   reshape_msd(direction = "semi-wide")
 
 df_prep_new <- 
   df_prep_long %>% 
   filter(indicator == "PrEP_NEW") %>% 
   bind_rows(df_prep_long %>% 
               filter(indicator == "PrEP_NEW") %>% 
               mutate(mech_name = "USAID",
                      primepartner = "USAID", 
                      mech_code = "USAID")) %>% 
 select(-cumulative) %>% 
   group_by(primepartner, mech_name, period, mech_code, indicator, 
            ageasentered, sex) %>% 
   summarise(across(c(results, targets), sum, na.rm = T)) %>% 
   mutate(pd_col = substr(period, 1, 4)) %>% 
   mutate(targets = ifelse(targets == 0, NA_real_, targets)) %>% 
   group_by(mech_code, pd_col, ageasentered, sex) %>% 
   fill(targets, .direction = "down") %>% 
   ungroup() %>% 
   mutate(achv = results/targets)
          
 facet_mech_order <- c("USAID", "Action HIV", "DISCOVER", "SAFE")
 
 df_prep_new %>% 
   filter(period == {pd}, 
          ageasentered != "Unknown Age", 
          indicator == "PrEP_NEW", 
          mech_code != "82086") %>% 
   mutate(facet_order = fct_relevel(mech_name, facet_mech_order)) %>% 
   mutate(sex_color = case_when(
     sex == "Male" & achv >= 1 ~ genoa,
     sex == "Male" & achv < 1 ~ "#72c3b4",
     sex == "Female" & achv >= 1 ~ "#171d5a",
     sex == "Female" & achv < 1 ~ moody_blue)
     ) %>% 
   mutate(xmax = ifelse(sex == "Male", 2e3, 12e3),
          xmin = 0) %>%
   ggplot() +
   geom_blank(aes(x = xmin)) +
   geom_blank(aes(x = xmax)) +
   geom_col(aes(y = ageasentered, x = targets), fill = grey10k) +
   geom_col(aes(y = ageasentered, x = results, fill = sex_color)) +
   geom_text(aes(y = ageasentered, x = results, label = percent(achv, 1)),
             size = 9/.pt, family = "Source Sans Pro", hjust = -0.1) +
   facet_wrap(facet_order ~ sex, nrow = 4, scales = "free", 
              labeller = label_wrap_gen(width = 20, multi_line = FALSE)) +
   scale_fill_identity()+
   si_style_xgrid(facet_space = 0.75) +
   labs(x = NULL, y = NULL,
      title = glue("PrEP_CURR: ACTION HIV HAS ACHIEVED THEIR FY22 TARGETS FOR MALES AS OF {pd}"),
        caption = glue("Source: {source}")) +
   scale_x_continuous(labels = comma) +
   coord_cartesian(clip = "off", expand = FALSE) 
 
 si_save(glue("Images/{pd}_PrEP_CURR.png"), scale = 1.25)
     

# PrEP_CT by Age ----------------------------------------------------------

df_prep_ct <- 
   df_prep_long %>% 
   filter(indicator == "PrEP_CT", 
          mech_code != "82086") %>% 
   bind_rows(df_prep_long %>% 
               filter(indicator == "PrEP_CT", 
                      mech_code != "82086") %>% 
               mutate(mech_name = "USAID", 
                      mech_code = "USAID", 
                      primepartner = "USAID")) %>% 
   group_by(primepartner, mech_name, period, mech_code, indicator, 
            ageasentered, sex) %>% 
   select(-targets) %>% 
   summarise(across(c(results), sum, na.rm = T)) %>% 
   ungroup()

 df_prep_ct %>% 
   filter(period == {pd}, 
          ageasentered != "Unknown Age") %>% 
   mutate(facet_order = fct_relevel(mech_name, 
                                    c("USAID", "Action HIV", "DISCOVER", "SAFE")),
          sex_color = ifelse(sex == "Male", "#72c3b4", moody_blue)) %>% 
   mutate(xmax = ifelse(sex == "Male", 1.3e3, 8e3),
          xmin = 0) %>%
   ggplot() +
   geom_blank(aes(x = xmin)) +
   geom_blank(aes(x = xmax)) +
   geom_col(aes(y = ageasentered, x = results, fill = sex_color)) +
   facet_wrap(facet_order ~ sex, nrow = 4, scales = "free",
              labeller = label_wrap_gen(width = 20, multi_line = FALSE)) +
   scale_fill_identity()+
   si_style_xgrid(facet_space = 0.5) +
   labs(x = NULL, y = NULL,
        title = glue("PrEP_CT RESULTS BY AGENCY AND PARTNER AS OF {pd}"),
        caption = glue("Source: {source}")) +
   scale_x_continuous(labels = comma) +
   coord_cartesian(clip = "off", expand = FALSE)   
 
 si_save(glue("Images/{pd}_PrEP_Ct.png"), scale = 1.25)

   
 