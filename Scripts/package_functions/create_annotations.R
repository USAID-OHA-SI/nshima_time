# Takes the munged cascade dataframe and generates annotation pieces needed
#' Title
#'
#' @param .data 
#' @param cscd_df 
#'
#' @return
#' @export
#'
#' @examples
create_annotations <- function(.data, cscd_df){
  annot_df <- 
    .data  %>% 
    dplyr::select(-results, -funding_agency) %>% 
    dplyr::filter(period == curr_pd) %>% 
    tidyr::pivot_longer(where(is.double),
                        names_to = "period_type") %>% 
    tidyr::pivot_wider(names_from = indicator) %>% 
    dplyr::mutate(Linkage = TX_NEW/HTS_TST_POS,
                  NNT = round(HTS_TST/HTS_TST_POS, 0),
                  VLS = TX_PVLS / TX_PVLS_D,
                  VLC = TX_PVLS_D / TX_CURR_Lag2) %>% 
    tidyr::pivot_longer(cols = where(is.double),
                        names_to = "indicator2") %>% 
    tidyr::pivot_wider(names_from = period_type,
                       values_from = value) %>% 
    dplyr::filter(indicator2 %in% keep_ind) %>% 
    dplyr::mutate(indicator = case_when(
      indicator2 == "Linkage" ~ "TX_NEW",
      indicator2 == "NNT" ~ "HTS_TST",
      indicator2 == "VLC" ~ "TX_PVLS_D", 
      indicator2 == "VLS" ~ "TX_PVLS"
    )) %>% 
    dplyr::mutate(indicator = forcats::fct_relevel(indicator, c(
      "HTS_TST", "TX_NEW", "TX_PVLS_D", "TX_PVLS")
    )
    )
  
  # Need to join in values from the color assigned cascade df
  annot_df <- annot_df %>% 
    inner_join(., cscd_df, by = c("indicator", "period"))
  
  return(annot_df)
}