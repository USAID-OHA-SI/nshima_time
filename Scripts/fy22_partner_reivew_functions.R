# PURPOSE: Homeless helper functions to crank out MDB tables
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-05-17
# NOTES: Source on-demand in generating partner visuals

# Homeless helper functions  ============================================================================

# Set the indicators to be used
base_indic <- c( "PrEP_NEW", "VMMC_CIRC", 
                 "HTS_INDEX",  "HTS_INDEX_NEWPOS", "HTS_TST", "HTS_TST_POS",
                 "HTS_SELF", "PMTCT_STAT_D", "PMTCT_STAT", "PMTCT_STAT_POS",
                 "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS")


# Return indicators based on period
indic_ptner <- function(qtr = qtr){
  if({{qtr}} %in% c(1, 3)) {
    ind_sel <- base_indic
  } else {
    ind_sec <- c(base_indic, "OVC_SERV")
  }
}


#table of age/sex disaggs
df_disaggs <- tibble::tribble(
  ~indicator,      ~standardizeddisaggregate,
  "HTS_INDEX",             "4:Age/Sex/Result",
  "HTS_INDEX_NEWPOS",               "Age/Sex/Result",
  "HTS_SELF",          "Age/Sex/HIVSelfTest",
  "HTS_TST",      "Modality/Age/Sex/Result",
  "HTS_TST_POS",      "Modality/Age/Sex/Result",
  "PMTCT_STAT",       "Age/Sex/KnownNewResult",
  "PMTCT_STAT_D",                      "Age/Sex",
  "PMTCT_STAT_POS",       "Age/Sex/KnownNewResult",
  "TX_CURR",            "Age/Sex/HIVStatus",
  "TX_NEW",            "Age/Sex/HIVStatus",
  "TX_PVLS", "Age/Sex/Indication/HIVStatus",
  "TX_PVLS_D", "Age/Sex/Indication/HIVStatus",
  "PrEP_NEW",                      "Age/Sex",
  "OVC_SERV",         "Age/Sex/ProgramStatus"
)


#Bold data in the VLS/VLC table
bold_column <- function(gt_obj, col){
  gt_obj %>% 
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#e6e7e8", alpha = 0.5),
        gt::cell_text(weight = 700)
      ),
      locations = cells_body(
        columns = {{col}},
      )
    )
}

# Bold Agency names - used to increase stroke on row group label
bold_rowgroup <- function(gt_obj){
  gt_obj %>% 
    gt::tab_style(
      style = list(
        gt::cell_text(weight = 700)
      ),
      locations = cells_row_groups(groups = everything())
    )
}


# Embiggen font size
embiggen <- function(gt_obj){
  gt_obj %>% 
    gt::tab_options(
      source_notes.font.size = 10,
      table.font.size = 15,
      footnotes.font.size = 10)
}


# Creates a markdown chunk to be inserted as a subtitle with legend pngs
legend_chunk_q1 <- gt::md(glue::glue("Legend: Cumulative <img src= '{legend_q1}' style='height:15px;'>    &emsp; Snapshot (TX_CURR) <img src= '{legend_snapshot}' style='height:15px;'> "))

legend_chunk_q4 <- gt::md(glue::glue("Legend: <img src= '{legend_snapshot}' style='height:15px;'> "))
