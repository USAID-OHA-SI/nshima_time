# PROJECT: Visualize monthly BOB data trends
# PURPOSE: Munge and Analysis of BOBs
# AUTHOR: Tim Essam | SI
# REF ID:   dca5128d
# LICENSE: MIT
# DATE: 2022-09-22
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(googlesheets4)
    library(gt)
    library(gtExtras)
    library(selfdestructin5)
    
  # SI specific paths/functions  
    load_secrets()
    file_path <- "1IqIv-0f0H-bOAHcve87GahptDDJ2As6QFpHkrM4kRfY"
  
  # REF ID for plots
    ref_id <- "dca5128d"
    
  # Functions  
  
  #TODO: Clean up function, make mini functions and helpers for longer term use
  #TODO: Check on format of table, how can it be cleaned up a bit?
  #TODO: Optimize layout of table so it fills a slide
  #TODO: figure out syntax for selecting across columns where a value is negative

# LOAD DATA ============================================================================  

  bobs <- read_sheet(ss = file_path, sheet = 4)

# MUNGE ============================================================================
  
  bobs_sub <- 
      bobs %>% 
      filter(str_detect(Indicator, "TX_PVLS|IIT", negate = T)) %>% 
      select(-`Fiscal Year`)
  
  bobs_spark <- bobs %>% 
      filter(str_detect(Indicator, "IIT|TX_PVLS", negate = T)) %>% 
      select(-`Fiscal Year`) %>% 
      pivot_longer(cols = Sep:Aug) %>% 
      group_by(Partner, Age, Indicator) %>% 
      summarise(spark = list(value), .groups = "drop")
  
  
  pct_vars <- c("Proxy Linkage", "VLC", "VLS")
  cont_vars <- c("HTS_TST_POS", "TX_CURR", "TX_NET_NEW", "TX_NEW")
  
  bobs_gt <- bobs_sub %>% 
    left_join(bobs_spark) %>% 
    mutate(change_dir = case_when(
      Aug > Jul ~ "increase",
      Aug < Jul ~ "decrease",
      Aug == Jul ~ "not applicable",
      TRUE ~ NA_character_
    )) %>% 
    mutate(trending = purrr::map(change_dir, make_chg_shape))
  

  
  partner_plot <- function(df, ptnr){
    tbl_title <- glue::glue("{ptnr} Bobs Summary Sep - Aug 2022") %>% str_to_upper()
    
    #Pull max value for TX_NET_NEW from each partner
    month_cols <- bobs_sub %>% 
      select(Sep:Aug) %>% 
      names() 
    
    prtnr_min <- 
      bobs_sub %>% 
      filter(Indicator == "TX_NET_NEW", Partner == ptnr) %>% 
      mutate(tx_nn_min = pmin(!!!rlang::syms(month_cols), na.rm = T)) %>% 
      pull(tx_nn_min) %>% 
      min(.)
    
   df %>% 
      filter(Partner == ptnr) %>% 
    gt(groupname_col = c("Age")) %>%
    fmt_number(rows = Indicator %in% cont_vars,
               columns = Sep:Aug,
               decimals = 0) %>% 
    fmt_percent(rows = Indicator %in% pct_vars,
               columns = Sep:Aug,
               decimals = 0) %>% 
    sub_missing(missing_text = "-") %>% 
    gt_plt_sparkline(spark, same_limit = F, type = "shaded", 
                     fig_dim = c(5, 30),
                     palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                     label = F) %>%
    tab_style(
      style = list(      
        cell_borders(
        sides = c("left", "right"),
        color = "#ffffff",
        weight = px(1)
        )
      ),
      locations = list(
      cells_body(
        columns = Sep:Aug,
        rows = Indicator == "TX_NET_NEW"
        )
      )
    ) %>% 
    data_color(columns = Sep:Aug,
               colors = scales::col_numeric(
                 c(alpha(old_rose_light, alpha = 0.4)),
                 domain = c(prtnr_min, 0),
                 na.color = "#ffffff",
                 alpha = TRUE
               )       
      ) %>%
    tab_style(
      style = list(
        cell_text(weight = 620)
      ),
      locations = cells_body(
        columns = Indicator
      )
    ) %>% 
    tab_options(
      source_notes.font.size = 8,
      table.font.size = 10, 
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1)) %>% 
    cols_hide(columns = c(change_dir, Partner)) %>% 
    cols_label(spark = "", 
               Partner = "",
               Indicator = "") %>% 
    tab_header(title = tbl_title)
  }
      
      

# VIZ ============================================================================

  partner_plot(bobs_gt, "SAFE") %>% 
    tab_options(table.width = 900
    )  %>% 
    gtsave(., "Images/SAFE_bobs_summary.png")
  
  partner_plot(bobs_gt, "ACTION HIV") %>% 
    tab_options(table.width = 900
    )  %>% 
    gtsave(., "Images/ACTION_bobs_summary.png")
  
  partner_plot(bobs_gt, "Discover") %>% 
    tab_options(table.width = 900
    )  %>% 
    gtsave(., "Images/Discover_bobs_summary.png")

# SPINDOWN ============================================================================

