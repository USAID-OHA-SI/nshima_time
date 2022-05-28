
#' Plot Cascade
#' `plot_cascade` is a ggplot2-based plot that arranges, annotates and 
#' plots data from the 90-90-90 cascade. Four inputs are required to create
#' a valid plot.
#'
#' @param .data dataframe that is the result of `return_cascade` and `assign_cscd_colors`
#' @param df_annot annotation dataframe created from the `create_annotations` function 
#' @param cscd_num cascade number selected by the user
#' @param p_title plot title derived from the `cscd_num`
#'
#' @return
#' @export
#'
#' @examples
plot_cascade <- function(.data, df_annot, cscd_num, p_title){
  
  .data %>% 
    dplyr::filter(period == max(period)) %>% 
    ggplot2::ggplot(aes(x = indicator, 
                        fill = indic_colors)) +
    ggplot2::geom_col(aes(y = targets), 
                      fill = glitr::trolley_grey_light, 
                      width = 0.5) +
    ggplot2::geom_col(aes(y = results_cumulative), 
                      width = 0.5, 
                      position = position_nudge(x = 0.1)) +
    ggplot2::geom_text(aes(y = results_cumulative, label = scales::comma(results_cumulative, 1)),
                       size = 12 / .pt, vjust = -0.45,
                       family = "Source Sans Pro",
                       position = position_nudge(x = 0.1)
    ) +
    ggplot2::geom_label(aes(y = results_cumulative, label = percent(achv, 1)),
                        size = 9 / .pt, vjust = 1.2,
                        family = "Source Sans Pro",
                        position = position_nudge(x = 0.1), fill = "white"
    ) +
    ggplot2::geom_text(data = . %>% filter(indicator == "HTS_TST"), 
                       aes(y = targets, label = "FY22 Targets"),
                       size = 12 / .pt, 
                       family = "Source Sans Pro", 
                       hjust = 0.4
    ) +
    ggplot2::geom_text(data = df_annot %>% filter(indicator == "HTS_TST"),
                       aes(y = results_cumulative.y,
                           label = paste("Tests needed for\none positive:", 
                                         scales::comma(results_cumulative.x),
                                         "\n(Positivity rate:",
                                         scales::percent(1/results_cumulative.x),
                                         ")"),
                           x = indicator, fill = glitr::grey50k),
                       size = 12 / .pt,
                       family = "Source Sans Pro SemiBold",
                       hjust = 0, 
                       vjust = -1) +
    ggplot2::geom_text(data = df_annot %>% filter(indicator == "TX_NEW"),
                       aes(y = results_cumulative.y,
                           label = paste("Linkage:", 
                                         scales::percent(results_cumulative.x)),
                           x = indicator, fill = glitr::grey50k),
                       size = 12 / .pt,
                       family = "Source Sans Pro SemiBold",
                       hjust = 1, 
                       vjust = -10) +
    ggplot2::geom_text(data = df_annot %>% filter(indicator == "TX_PVLS_D"),
                       aes(y = results_cumulative.y,
                           label = paste("VLC:", 
                                         scales::percent(results_cumulative.x)),
                           x = indicator, fill = glitr::grey50k),
                       size = 12 / .pt,
                       family = "Source Sans Pro SemiBold", 
                       vjust = -10) +
    ggplot2::geom_text(data = df_annot %>% filter(indicator == "TX_PVLS"),
                       aes(y = results_cumulative.y,
                           label = paste("VLC:", 
                                         scales::percent(results_cumulative.x)),
                           x = indicator, fill = glitr::grey50k),
                       size = 12 / .pt,
                       family = "Source Sans Pro SemiBold", 
                       vjust = -10) +
    
    ggplot2::scale_y_continuous(labels = comma) +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_wrap(~cascade, 
               scales = "free") +
    glitr::si_style_ygrid(text_scale = 1.25) +
    ggplot2::labs(
      x = NULL, 
      y = NULL, 
      title = glue("{p_title} Cascade- {curr_pd} RESULTS TO TARGETS (GRAY BARS)") %>% 
        toupper(),
      subtitle = glue("{curr_pd} results listed above colored bar, achievement in box below"),
      caption = data_source
      )
  
}
