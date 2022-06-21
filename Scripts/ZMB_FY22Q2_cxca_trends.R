# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth | USAID
# PURPOSE:  Global cumulative CX_CA screening and treatment trends
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:  2021-12-03
# NOTE:     Created based on request from FO "2 visuals for AHOP deck" -- 2021-07-16


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
library(googlesheets4)
library(mindthegap)
library(geomtextpath)

# GLOBALS -----------------------------------------------------------------

msd_path <- return_latest(si_path(), "PSNU_IM_FY20-22_.*_v1_1_Zambia")

#source info
curr_pd <- source_info(msd_path, return = "period")
curr_fy <- source_info(msd_path, return = "fiscal_year")
msd_source <- source_info(msd_path)

pull_figures <- function(df, indic, partner, metric) {
  df %>% 
    filter(indicator == indic, 
           partner_type == partner, 
           fiscal_year == curr_fy) %>% 
    select({{metric}}) %>% 
    pull()
}


# IMPORT ------------------------------------------------------------------

#Current MSD
df <- read_msd(msd_path)


# MUNGE -------------------------------------------------------------------

df_cxca <- df %>% 
  filter(funding_agency == "USAID",
         str_detect(indicator, "CXCA"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year <= curr_fy)

df_cxca <- df_cxca %>% 
  group_by(fiscal_year, funding_agency, indicator, operatingunit) %>% 
  summarise(across(matches("qtr2|qtr4"), sum, na.rm = TRUE)) %>% 
  pivot_longer(cols = matches("qtr"),
               names_to = "period") %>% 
  spread(indicator, value) %>% 
  mutate(tx_rate = CXCA_TX / CXCA_SCRN_POS,
         period = paste0("FY", str_sub(fiscal_year, 3, 4), "Q", str_sub(period, 4))) %>% 
  group_by(operatingunit) %>% 
  mutate(order_var = sum(CXCA_SCRN, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ou_order = fct_reorder(operatingunit, order_var, .desc = T)) %>% 
  filter(period != "FY22Q4")


# VIZ ---------------------------------------------------------------------

# all breaks if needed
#cust_breaks <- c(paste0("FY", rep(seq(18, 21, 1), each = 2), "Q", seq(2, 4, 2)))

ou_count <- df_cxca %>% 
  filter(period == "FY21Q4", operatingunit != "USAID") %>% 
  distinct(operatingunit) %>% 
  tally() %>% pull()

si_blue <- "#4974a5"
nudge_space  <-  0.15

a <-  df_cxca %>% 
  mutate(positivity = CXCA_SCRN_POS/CXCA_SCRN) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = CXCA_SCRN), fill = golden_sand_light,
           width = 0.6) +
  geom_col(aes(y = CXCA_SCRN_POS), fill = golden_sand, width = 0.5, 
           position = position_nudge(x = nudge_space)) + 
  geom_col(aes(y = CXCA_TX), fill = si_blue, width = 0.5, position = position_nudge(x = -nudge_space)) +
  geom_text(aes(y = CXCA_SCRN_POS, label = percent(positivity, 1)),
            size = 12/.pt, family = "Source Sans Pro SemiBold", vjust = -0.5, hjust = -0.5) +
  si_style_xline() +
  geom_hline(yintercept = seq(1e4, 5e4, 1e4), color = "white", size = 0.5) +
  scale_y_continuous(position = "right", labels = label_number()) +
  theme(strip.text = element_blank()) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL)



b <- df_cxca %>% 
  ggplot(aes(x = period, y = tx_rate, group = operatingunit)) +
  geom_area(aes(y = 1), fill = "#bdcee2", alpha = 0.5)+
  geom_area(fill = si_blue, alpha = 0.5)+
  geom_line(color = si_blue, size = 2) +
  # geom_textpath(aes(label = "Treatment rate"), hjust = 0.95, vjust = -1, include_line = F)+
  geom_text(aes(label = "Treatment rate", y = 1, x = "FY21Q2"), vjust = -1,
            family = "Source Sans Pro", size = 12/.pt) +
  geom_hline(yintercept = 1, size = 0.25, linetype = "dotted") +
  geom_label(aes(label = percent(tx_rate, 1)), size = 12/.pt, family = "Source Sans Pro SemiBold") +
  si_style_xline() +
  # coord_cartesian(expand = F) +
  scale_x_discrete(expand = expansion(add = 0.25))+
  scale_y_continuous(expand = expansion(mult = 0), lim = c(0, 1.5)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank()) +
  labs(caption = glue("Source: {msd_source}}
                     US Agency for International Development"))

a / b + plot_layout(heights = c(6, 2))

si_save(glue("Graphics/{curr_pd}_CXCA-screening-trends.svg"), scale = 1.2)  



