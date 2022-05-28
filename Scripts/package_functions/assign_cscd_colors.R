
# add color to the cascade indicators
#' Title
#'
#' @param .data data frame resulting from `return_cascade`
#'
#' @return `df_cscd_viz` data frame
#' @export
#'
#' @examples
assign_cscd_colors <- function(.data){
  
  df_cscd_viz <- 
    .data %>% 
    dplyr::filter(indicator != "TX_CURR_Lag2") %>% 
    dplyr::mutate(
      achv = ifelse(targets > 0, results_cumulative / targets, NA_real_),
      indic_colors = dplyr::case_when(
        indicator == "HTS_TST" ~ "#877ec9",
        indicator == "HTS_TST_POS" ~ "#b5aaf9",
        indicator == "TX_NEW" ~ glitr::golden_sand_light,
        indicator == "TX_NET_NEW" ~ glitr::golden_sand_light,
        indicator == "TX_CURR" ~ glitr::golden_sand,
        indicator == "TX_PVLS_D" ~ glitr::scooter_med,
        indicator == "TX_PVLS" ~ glitr::scooter
      ),
      cascade = dplyr::case_when(
        stringr::str_detect(indicator, "HTS") ~ "Testing",
        stringr::str_detect(indicator, "TX_NE") ~ "Linkage & Net New",
        TRUE ~ "Treatment & VLS | VLS"
      )
    )
  
  df_cscd_viz <-
    df_cscd_viz %>%
    dplyr::bind_rows(df_cscd_viz %>% 
                       dplyr::filter(indicator == "HTS_TST_POS") %>% 
                       dplyr::mutate(cascade = "Linkage & Net New")) %>%
    dplyr::mutate(indicator = forcats::fct_relevel(indicator, c(
      "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_NET_NEW", 
      "TX_CURR", "TX_PVLS_D", "TX_PVLS"
    )),
    cascade = forcats::fct_relevel(cascade, c(
      "Testing", "Linkage & Net New", "Treatment & VLS | VLS")
    )
    ) 
  
  return(df_cscd_viz)
  
}
