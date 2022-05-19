## PROJECT: nshima-time
## AUTHOR:  K. Srikanth | T. Essam
## PURPOSE: Cascade Partner Review - PMTCT
## LICENSE: MIT
## DATE:    2022-05-19
## Notes: adapted from groundhog_day/Scripts/FY21Q1_TZA_PMTCT.R 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(googledrive)
library(glamr)
library(glitr)
library(scales)
library(extrafont)
library(gophr)
library(glue)
library(tidytext)
library(patchwork)
library(ggtext)


# GLOBALS -----------------------------------------------------------------

#bar colors
filler <- c("Total" = trolley_grey,
            "Known at Entry" = moody_blue,
            "Newly Identified" = genoa,
            "Recent" = denim,
            "Already" = denim_light,
            "New" = burnt_sienna,
            "<=02 Months" = scooter,
            "02 - 12 Months" = old_rose)

genie_path <- file.path(si_path(), "Genie-PSNUByIMs-Zambia-Daily-2022-05-16_ALL.zip")

msd_source <- source_info(genie_path)
curr_pd <- source_info(genie_path, return = "period")
curr_fy <- source_info(genie_path, return = "fiscal_year")
curr_qtr <- source_info(genie_path, return = "quarter")

# IMPORT ------------------------------------------------------------------

df <- read_msd(genie_path)


# MUNGE -------------------------------------------------------------------

#denom
df_pmtct <- df %>% 
  filter(funding_agency == "USAID") %>% 
  clean_indicator()

#coverage
df_cov <- df_pmtct %>% 
  filter(indicator %in% c("PMTCT_STAT", "PMTCT_STAT_D"),
         standardizeddisaggregate %in% c("Total Denominator", "Age/Sex/KnownNewResult")) %>% 
  mutate(group_cas = "Coverage",
         indicator_cas = ifelse(indicator == "PMTCT_STAT_D", "# of ANC1 Clients", "Clients with known HIV status at ANC1"),
         disagg_cas = otherdisaggregate)

#lnk
df_lnk <- df_pmtct %>% 
  filter(indicator %in% c("PMTCT_STAT_POS","PMTCT_ART", "PMTCT_EID_D", "PMTCT_EID", "PMTCT_EID_Less_Equal_Two_Months"),
         standardizeddisaggregate %in% c("Age/Sex/KnownNewResult", 
                                         "Age/NewExistingArt/Sex/HIVStatus",
                                         "Total Denominator",
                                         "Total Numerator"),
         (!(indicator %in% c("PMTCT_ART", "PMTCT_STAT_POS") & standardizeddisaggregate == "Total Numerator"))) %>% 
  mutate(group_cas = "Linkage",
         indicator_cas = case_when(indicator == "PMTCT_STAT_POS" ~ "# of HIV+ pregnant women",
                                   indicator == "PMTCT_ART" ~ "HIV+ pregnant women on ART",
                                   indicator == "PMTCT_EID_D" ~ "# of infants that should be tested [ANC1 + Post-ANC1]",
                                   indicator == "PMTCT_EID_Less_Equal_Two_Months" ~ "HEI with virologic test by <2 months",
                                   indicator == "PMTCT_EID" ~ "HEI with virologic test by 12 months"),
         disagg_cas = otherdisaggregate)

#hei
df_hei <- df_pmtct %>% 
  filter(indicator %in% c("PMTCT_HEI_POS", "PMTCT_HEI_POS_ART"),
         standardizeddisaggregate %in% c("Total Numerator", "Age/HIVStatus"),
         !(indicator == "PMTCT_HEI_POS" & standardizeddisaggregate == "Total Numerator")) 

df_hei <- df_hei %>% 
  filter(indicator == "PMTCT_HEI_POS", 
         ageasentered == "<=02 Months") %>% 
  mutate(indicator = "PMTCT_HEI_POS_2MO") %>% 
  bind_rows(df_hei, .) %>% 
  mutate(group_cas = "HEI",
         indicator_cas = case_when(indicator == "PMTCT_HEI_POS_2MO" ~ "HIV+ infants identified by <2 months",
                                   indicator == "PMTCT_HEI_POS" ~ "HIV+ infants identified by 12 months",
                                   indicator == "PMTCT_HEI_POS_ART" ~ "HIV+ infants linked to ART"),
         disagg_cas = ageasentered)

#combine
df_combo <- bind_rows(df_cov, df_lnk, df_hei) %>% 
  mutate(group_cas = factor(group_cas, c("Coverage", "Linkage", "HEI")),
         indicator_cas = factor(indicator_cas, c("# of ANC1 Clients", "Clients with known HIV status at ANC1", 
                                                 "# of HIV+ pregnant women", "HIV+ pregnant women on ART", 
                                                 "# of infants that should be tested [ANC1 + Post-ANC1]",
                                                 "HEI with virologic test by <2 months",
                                                 "HEI with virologic test by 12 months",
                                                 "HIV+ infants identified by <2 months",
                                                 "HIV+ infants identified by 12 months",
                                                 "HIV+ infants linked to ART")),
         disagg_cas = str_remove(disagg_cas, "Life-long ART, "),
         disagg_cas = ifelse(is.na(disagg_cas), "Total", disagg_cas),
         disagg_cas = factor(disagg_cas, c("Newly Identified", "Known at Entry",  "Recent",
                                           "Already", "New", "02 - 12 Months", "<=02 Months",  "Total")))

# PLOT --------------------------------------------------------------------


df_viz <- df_combo %>% 
  group_by(fiscal_year, group_cas, indicator_cas, disagg_cas) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  filter(period == curr_pd) 


v_c <- df_viz %>% 
  filter(group_cas == "Coverage") %>% 
  ggplot(aes(indicator_cas, value, fill = disagg_cas)) +
  geom_col(alpha = .9) +
  facet_grid(~ group_cas, scales = "free", space = "free") +
  geom_text(aes(y = value, label = comma(value, 1)),
            size = 12 / .pt, vjust = -1,
            family = "Source Sans Pro"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = filler, na.value = trolley_grey) +
  labs(x = NULL, y = NULL, fill = NULL) +
  si_style_ygrid() +
  theme(axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.position = "none")

v_l <- df_viz %>% 
  filter(group_cas == "Linkage") %>% 
  ggplot(aes(indicator_cas, value, fill = disagg_cas)) +
  geom_col(alpha = .9) +
  geom_text(aes(y = value, label = comma(value, 1)),
            size = 12 / .pt, vjust = -1,
            family = "Source Sans Pro"
  ) +
  facet_grid(~ group_cas, scales = "free", space = "free") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = filler, na.value = trolley_grey) +
  labs(x = NULL, y = NULL, fill = NULL) +
  si_style_ygrid() +
  theme(axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.position = "none")

v_h <- df_viz %>% 
  filter(group_cas == "HEI") %>% 
  ggplot(aes(indicator_cas, value, fill = disagg_cas)) +
  geom_col(alpha = .9) +
  geom_text(aes(y = value, label = comma(value, 1)),
            size = 12 / .pt, vjust = -1,
            family = "Source Sans Pro"
  ) +
  facet_grid(~ group_cas, scales = "free", space = "free") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = filler, na.value = trolley_grey) +
  labs(x = NULL, y = NULL, fill = NULL) +
  si_style_ygrid() +
  theme(axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.position = "none")


v_c + v_l + v_h +
  plot_layout(widths = c(2, 5, 3)) +
  plot_annotation(
    title = 'FY22Q2 PMTCT CASCADE Zambia',
    subtitle = "ANC entry to HEI ART Linkage<br><br>
        <span style = 'color:#808080;'>Total</span> |
        <span style = 'color:#8980cb;'>Known at Entry</span> |
        <span style = 'color:#287c6f;'>Newly Identified</span> |
        <span style = 'color:#2057a7;'>Recent</span> |
        <span style = 'color:#bfddff;'>Already</span> |
        <span style = 'color:#e07653;'>New</span> |
        <span style = 'color:#1e87a5;'><02 mo</span> | 
        <span style = 'color:#c43d4d;'>02 - 12 mo</span>",
    caption = 'Mirrors the USAID PMB Quarterly Review Dashboard
        PMTCT_STAT_POS is used as the denominator for EID Testing Coverage. Panorama uses [PMTCT_STAT_POS + Post_ANC1_POS] as the denominator for EID Testing Coverage
        Source: FY22Q2 PSNUxIM Genie [2022-05-16]') & si_style_ygrid() & 
  theme(plot.subtitle = element_markdown(),
        strip.text = element_blank(),
        legend.position = "none")

si_save("Graphics/FY22Q2_ZMB_PMTCT-Cascade.svg")    


