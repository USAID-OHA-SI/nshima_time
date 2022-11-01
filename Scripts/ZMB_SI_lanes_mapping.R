# PROJECT: Visualize Portfolio Coverage by mechanism
# PURPOSE: Munge and Analysis of portfolio data
# AUTHOR: Tim Essam | SI
# REF ID:   e7a70610
# LICENSE: MIT
# DATE: 2022-10-04
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
    library(tidytext)
    
    
  # SI specific paths/functions  
    load_secrets()
      
  # Grab metadata
  # cascade::get_file_metadata()
  
  # REF ID for plots
    ref_id <- "e7a70610"
    
  # Functions  
  link <- "1uPFL57VxyaZaAnsLrVPQmtkc2jAW2Stn3A3ftovj52k"

# LOAD DATA ============================================================================  

  df <- read_sheet(ss = link) %>% 
    mutate(pcode = as.factor(si_poc))
  
  df_tasks <- read_sheet(ss = link, sheet = "si_tasks") 
    # filter(!is.na(main_poc))
  
  df_tasks_wide <- 
    df_tasks %>% 
    select(1:3) %>% 
    mutate(cov = "X") %>% 
    spread(main_poc, cov) %>% 
    write_csv("Dataout/si_tasks.csv", na = "")

# MUNGE ============================================================================
  # Partner order based on TX_CURR targets
  
  df_viz <- 
    df %>% 
    mutate(sort_var = case_when(
      indic_type == "prevention" ~ 1, 
      indic_type == "testing" ~ 2, 
      indic_type == "treatment" ~ 3,
      ),
      indic_order = reorder_within(indicator, sort_var, targets),
      mech_order = fct_reorder(mech_name_short, targets)) %>% 
    mutate(si_poc_color = case_when(
      si_poc == "Mwila" ~ "#5BB5D5",
      si_poc == "Arthur" ~ "#00506c",
      si_poc == "Amanzi" ~ "#984000",
      si_poc == "Amara" ~ "#f49432"
      )
    )
  
  df_viz %>% 
    ggplot(aes(y = indic_order, x = pcode)) +
    geom_tile(aes(fill = si_poc), color = "white") +
    facet_wrap(~mech_name, scales = "free_y") +
    scale_y_reordered() +
    si_style(facet_space = 0.5, text_scale = 0.75) 
  
  # Need to complete the dataframe around mech_code + indicator + si_poc
  # Just need a grid that has mechs X indicator X pcodes
  df_base <- 
    df_viz %>% 
    distinct(indicator, partner_type, indic_type) %>% 
    complete(indicator, partner_type) %>% 
    group_by(indicator) %>% 
    fill(indic_type, .direction = "updown") %>% 
    ungroup()
  
  # Steps needed, need to add the df_base dataset to each mech code
  
  
  df_base %>% View()
    ggplot(aes(y = indicator, x = "a")) +
    geom_tile(color = "white") +
    facet_grid(indic_type ~ partner_type, scales = "free", space = "fixed") +
    si_style_nolines()
  
    ggplot(aes(y = indicator, x = mech_name_short)) +
    geom_tile(fill = grey10k, color = "white") +
    facet_grid(indic_type ~ partner_type, scales = "free", space = "fixed") +
    si_style_nolines()
  
  
df_expanded <- df_viz %>% 
    expand(., nesting(indicator, indic_type), mech_name_short, si_poc) 

ct_mechs <- df_viz %>% 
  filter(partner_type == "C&T") %>% 
  distinct(mech_name_short) %>% 
  pull()

  df_expanded %>% 
    mutate(partner_type = case_when(
      mech_name_short %in% ct_mechs ~ "C&T",
      TRUE ~ "Other"
    )) %>% 
    left_join(df_viz) %>%
    mutate(mech_name_short = fct_relevel(
      mech_name_short, c("SAFE", "ZIHA", "DISCOVER", "ZAM Health",
                         "ACTION HIV", "Eradicate TB", "CHEKUP I",
                         "CHEKUP II", "ECAP I", "ECAP II", "ECAP III",
                         "Stop GBV")
        )
      )  %>%
    ggplot(aes(y = indicator, x = mech_name_short)) +
    geom_tile(fill = grey10k, color = "white") +
    geom_tile(data = df_viz, aes(fill = si_poc_color), color = "white") +
    #geom_point(aes(color = si_poc, size = targets), shape = 15) +
    si_style(facet_space = 0.5, text_scale = 0.75) +
    scale_fill_identity(na.value = grey10k) +
    facet_grid(indic_type ~ partner_type, scales = "free", space = "free") +
    si_style_nolines(facet_space = 0.5, text_scale = 0.75) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "SI ROLES & RESPONSIBILITIES")
  
  si_save("Graphics/SI_lanes.svg")
    
  df_viz %>% 
    count(mech_name_short, `AOR/COR`)


# SI TASKS ----------------------------------------------------------------

  df_tasks %>% 
    count(Focus, main_poc, task) %>% 
    spread(main_poc, n) %>% 
   View()
  
  df_tasks %>% 
    select(Focus, task, main_poc) %>%
    mutate(responsible = ifelse(main_poc != "unknown", "yes", "unknown")) %>% 
    complete(task, main_poc) %>% 
    group_by(task) %>% 
    fill(Focus, .direction = "downup") %>% 
    mutate(main_poc = fct_relevel(main_poc, "unknown", after = 0))%>% 
    ggplot(aes(x = main_poc, y = task)) +
    geom_tile(aes(fill = responsible), color = "white") +
    scale_x_discrete(position = "top") +
    scale_fill_manual(values = c("yes" = scooter_med, "unknown" = old_rose_light),
                      na.value = grey10k) +
    scale_y_discrete(limits = rev) +
    si_style_nolines(facet_space = 0.5) +
    facet_grid(Focus~., scales = "free", space = "free") +
    #facet_wrap(~Focus, scales = "free_y", ncol = 1) +
    labs(x = NULL, y = NULL,
         fill = "coverage",
         title = str_to_upper("Key portfolio topic areas with SI POCs")) +
    guides(fill=guide_legend(ncol=2))
  si_save("Graphics/ZMB_si_portfolio_tasks.svg")
  
  
  # IDEA is this 
  # ALL BOXES ARE GRAYed out
  # REVEAL one-by one where each SI advisor falls in the composition
  # SHOW each staff's footprint one column at a time
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================


