# PROJECT:  nshima-time
# AUTHOR:   K. Srikanth, T. Essam | USAID
# PURPOSE:  develop OVC visuals for FY22Q2 review
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

full_pds <- (min(df$fiscal_year) - 1) %>% 
  paste0("-10-01") %>% 
  as_date() %>% 
  seq.Date(convert_qtr_to_date(curr_pd), by = "3 months") %>% 
  convert_date_to_qtr()

pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")

swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
  # Using EQUIP as default as this has to be done each time in FY21
  .data %>%
    mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
           mech_name = ifelse(mech_code == {{mech2}}, "Action HIV", mech_name))
}

# OVC SERV BY PARTNER ------------------------------

df_ovc <- df %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/DREAMS", "Age/Sex/Preventive"),
         trendscoarse == "<18") %>% 
  swap_targets() %>% 
  group_by(mech_code, mech_name, indicator, fiscal_year, trendscoarse) %>% 
  summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd("quarters") %>% 
  select(-results_cumulative) %>% 
  mutate(mech_name = ifelse(mech_code == 17399, "DISCOVER-H", mech_name),
         achv = results/targets)

df_ovc <- df_ovc %>% 
  group_by(mech_code) %>%
  mutate(decline = results < lag(results, 1),
         decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
         fill_color = case_when(fiscal_year < curr_fy ~ trolley_grey,
                                decline == TRUE ~ golden_sand,
                                TRUE ~ scooter),
         fill_alpha = ifelse(fiscal_year < curr_fy, .6, .9),
         results_latest = case_when(period == max(period) ~ results),
         targets_latest = case_when(period == max(period) ~ targets),
         decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
  fill(results_latest,decline_latest, .direction = "up") %>% 
  mutate(disp_name = glue("{mech_code} - {mech_name} {decline_latest}")) %>% 
  ungroup() 

v_ovc_lrg <- df_ovc %>% 
  filter(period == max(period),
         trendscoarse == "<18",
         mech_code %in% c(85120, 85121, 85114)) %>% 
  arrange(desc(results)) %>% 
  mutate(cumsum = cumsum(results)/sum(results)) %>% 
  slice_head(n = 3) %>% 
  pull(mech_code)
  
  df_ovc %>% 
    filter(mech_code %in% v_ovc_lrg,
           trendscoarse == "<18"
          # ageasentered != "Unknown Age"
           #period %in% c("FY21Q2", "FY21Q4", "FY22Q2")
    ) %>% 
  mutate(targets = ifelse(period %in% c("FY21Q2", "FY21Q4", "FY22Q2"), targets, NA)) %>% 
  mutate(facet_order = fct_relevel(mech_name, facet_mech_order)) %>% 
  ggplot(aes(period, results, alpha = fill_alpha)) +
  geom_col(aes(y = targets, fill= trolley_grey_light)) +
  geom_col(aes(fill = old_rose), position = position_nudge(x = 0.3)) +
  geom_text(data = . %>% filter(period == max(period)), 
            aes(label = label_number_si()(results_latest)), 
            vjust = -.7, hjust = -0.7, color = matterhorn,
            family = "Source Sans Pro") +
  geom_text(data = . %>% filter(period == max(period)), 
            aes(y = targets, label = label_number_si()(targets_latest)), 
            vjust = -0.7, hjust = 2, color = matterhorn,
            family = "Source Sans Pro") +
    geom_text(data = . %>% filter(period == "FY21Q4"), 
              aes(label = label_number_si()(results)), 
              vjust = -.7, hjust = -0.7, color = matterhorn,
              family = "Source Sans Pro") +
    geom_text(data = . %>% filter(period == "FY21Q4"), 
              aes(y = targets, label = label_number_si()(targets)), 
              vjust = -0.7, hjust = 2, color = matterhorn,
              family = "Source Sans Pro") +  
    geom_text(data = . %>% filter(period == "FY21Q2"), 
              aes(label = label_number_si()(results)), 
              vjust = -.7, hjust = -0.7, color = matterhorn,
              family = "Source Sans Pro") +
    geom_text(data = . %>% filter(period == "FY21Q2"), 
              aes(y = targets, label = label_number_si()(targets)), 
              vjust = -0.7, hjust = 2, color = matterhorn,
              family = "Source Sans Pro") +
    geom_label(data = . %>% filter(period %in% c("FY21Q2", "FY21Q4", "FY22Q2")),
               aes(y = results, label = percent(achv, 1)),
               size = 9 / .pt, vjust = 1.2, hjust = 0,
               family = "Source Sans Pro",
               position = position_nudge(x = 0.1), fill = "white"
    ) +
  facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_y_continuous(label = label_number_si()) +
  scale_x_discrete(labels = pd_brks) +
  coord_cartesian(expand = T, clip = "off") +
  labs(x = NULL, y = NULL, 
       title = glue("Empowered Youth I, II, and III have all reached their OVC Fy22 targets as of {curr_pd}") %>% toupper(),
       subtitle = glue("OVC_SERV<18 trends in largest {length(v_ovc_lrg)} OVC partners in {curr_pd}"),
       caption = glue("Source: {msd_source}")) +
  si_style_ygrid() +
  theme(panel.spacing = unit(.5, "line"),
        plot.subtitle = element_markdown(),
        strip.text = element_markdown())

si_save(glue("Images/{curr_pd}_ZAM_partner_ovcunder18.png"))
si_save(glue("Graphics/{curr_pd}_ZAM_partner_ovcunder18.svg"))


#OVC HIV STAT -----------------------------------------------------------------

df_hiv_ovc <- df %>% 
  filter(indicator %in% c("OVC_HIVSTAT", "OVC_HIVSTAT_POS"), 
 # count(indicator, standardizeddisaggregate)
         standardizeddisaggregate %in% c("Total Numerator"),
         funding_agency == "USAID") %>% 
  swap_targets() %>% 
  group_by(mech_code, mech_name, indicator, fiscal_year) %>% 
  summarise(across(c(targets, starts_with("qtr"), starts_with("cumulative")), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd("quarters") %>% 
  select(-results_cumulative) %>% 
  mutate(mech_name = ifelse(mech_code == 17399, "DISCOVER-H", mech_name),
         achv = results/targets,
         disp_name = glue("{mech_code} - {mech_name}")) %>% 
  ungroup() %>% 
  pivot_wider(names_from = indicator, values_from = results) %>% 
  mutate(pos = OVC_HIVSTAT_POS / OVC_HIVSTAT)


df_hiv_ovc %>% 
  filter(mech_code %in% c(85120, 85121, 85114)) %>% 
  ggplot(aes(period)) + 
  geom_col(
    aes(y = OVC_HIVSTAT, fill = denim_light), na.rm = TRUE) +
  geom_col(
           aes(y = OVC_HIVSTAT_POS, fill = denim), position = position_nudge(x = 0.3), na.rm = TRUE) +
  scale_fill_identity() + 
  facet_wrap(~disp_name, scales = "free") +
  scale_y_continuous(label = scales::comma,
                     expand = c(.005, .005)) +
  geom_text(
    aes(y = OVC_HIVSTAT_POS, label = comma(OVC_HIVSTAT_POS), vjust = -1, na.rm = TRUE,
                family = "Source Sans Pro")) +
  geom_text(
    aes(y = OVC_HIVSTAT, label = comma(OVC_HIVSTAT), vjust = -1, na.rm = TRUE,
        family = "Source Sans Pro")) +
  geom_label(data = . %>% filter(period %in% c("FY21Q2", "FY21Q4", "FY22Q2")),
             aes(y = OVC_HIVSTAT, label = percent(pos, 1)),
             size = 9 / .pt, vjust = 1.2, hjust = 0,
             family = "Source Sans Pro",
             position = position_nudge(x = 0.1), fill = "white"
  ) +
  labs(x = NULL, y = NULL,
       title = glue("OVC_HIVSTAT_POS and OVC_HIVSTAT are increasing across the largest {length(v_ovc_lrg)} OVC partners in {curr_pd}") %>% toupper(),
       subtitle = glue("<span style='color:{denim}'>OVC_HIVSTAT_POS</span> and <span style='color:{denim_light}'>OVC_HIVSTAT</span>"),
       caption = glue("Source: {msd_source}")) +
  si_style_ygrid() +
  scale_x_discrete(labels = pd_brks) +
  theme(legend.position = "none",
        strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        plot.subtitle = element_markdown(),
        strip.text = element_markdown()) + 
  coord_cartesian(expand = F, clip = "off")


si_save(glue("Images/{curr_pd}_ZAM_partner_ovchivstat.png"))
si_save(glue("Graphics/{curr_pd}_ZAM_partner_ovchivstat.svg"))

#ART COVERAGE ----

# df_art <- df %>% 
#   filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/DREAMS", "Age/Sex/Preventive") & trendscoarse == "<18") |
#            (indicator == "OVC_HIVSTAT"),
#          funding_agency == "USAID")
# 
# 
# 
# #create shares 
# 
# df_hiv_ovc <- df_hiv_ovc %>% 
#   pivot_wider(names_from = indicator,values_from = results)
# 
# df_art <- df_art %>%
#   filter(indicator == "OVC_HIVSTAT",
#          otherdisaggregate == "Receiving ART") %>% 
#   group_by(mech_code, mech_name, indicator, fiscal_year) %>% 
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   reshape_msd() %>% 
#   arrange(period) %>% 
#   pivot_wider(names_from = indicator) %>%
#   rename(ovc_art = OVC_HIVSTAT) %>% 
#   left_join(df_hiv_ovc, by = c("mech_code", "mech_name", "period")) %>% 
#   filter(ovc_art > 0 & OVC_HIVSTAT) %>% 
#   mutate(art_share = ovc_art / OVC_HIVSTAT)
# 
# #grab the latest stat from max period
# latest_stat_art <- df_art %>% 
#   filter(period == max(period)) %>% 
#   pull()
# 
# #grab the max period
# latest_pd_art <- df_art %>% 
#   slice_max(order_by = period, n = 1) %>% 
#   pull(period)
# 
# #visual 3: OVC < 18 on ART
# v3 <- df_art %>% 
#   filter(statushiv == "Positive") %>% 
#   ggplot(aes(period, art_share, group = period_type)) +
#   geom_blank(aes(y = 1.1 * art_share)) +
#   geom_line(size = 1.5, color = scooter) +
#   geom_point(shape = 21, color = scooter, fill = scooter, size = 12, stroke = 2) +
#   facet_wrap(~mech_code, scales = "free") +
#   geom_text(aes(label = percent(art_share, 1)),
#             family = "Source Sans Pro", size = 12/.pt, color = "white") +
#   expand_limits(y = .2) +
#   labs(
#     x = NULL, y = NULL) +
#   si_style_nolines() +
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_blank()) + coord_cartesian(expand = F, clip = "off")



