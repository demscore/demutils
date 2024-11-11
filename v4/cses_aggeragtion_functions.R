
#' Aggregation CSES to Country-Year
#' 
#' Aggregates from cses_respondent level to country_year by counting the 
#' frequency of a response in a given country-year.
#' 
#' @param df a data.frame
#' @return data.frame aggregated to cses_country_year.
#' @examples 
#' aggregate_cses(df)
#'
#' @export
aggregate_cses <- function(df, ...) {
  
  party_id_vars <- names(dplyr::select(df, dplyr::matches("^cses_imd_imd5100_[a-i]"), dplyr::matches("^cses_imd_imd5101_[a-i]"), 
                                       dplyr::matches("^cses_imd_imd5102_[a-i]"), dplyr::matches("^cses_imd_imd5103_[a-i]")))
  
  party_share_vars <- names(dplyr::select(df, dplyr::matches("^cses_imd_imd5001_[a-i]"), dplyr::matches("^cses_imd_imd5002_[a-i]"), 
                                          dplyr::matches("^cses_imd_imd5004_[a-i]"),dplyr::matches("^cses_imd_imd5005_[a-i]")))
  
  group_vars <- c("u_cses_respondent_country", "u_cses_respondent_year", party_id_vars, party_share_vars)
  
  unit_table_start <- read_unit_table("u_cses_respondent")
  unit_table_start %<>% arrange(u_cses_respondent_id) 
  
  df %<>% 
    arrange(cses_imd_imd1005) %>%
    left_join(., unit_table_start, by = c("cses_imd_imd1005" = "u_cses_respondent_id")) %>%
    dplyr::filter(!(cses_imd_imd1004 %in% c("BELW2019", "BELW1999", "GRC22015"))) %>%
    dplyr::select("u_cses_respondent_country", "u_cses_respondent_year", dplyr::matches("cses_imd_imd3007_[a-i]$"), 
                  dplyr::matches("cses_imd_imd3008_[a-i]$"), dplyr::matches("cses_imd_imd3009_[a-i]$"),
                  dplyr::matches("cses_imd_imd3015_[a-d]$"), dplyr::matches("cses_imd_imd5012_[a-i]$"), 
                  dplyr::all_of(party_id_vars), dplyr::all_of(party_share_vars)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::matches("cses_imd_imd3007_[a-i]$|cses_imd_imd3008_[a-i]$|cses_imd_imd3009_[a-i]$|cses_imd_imd5012_[a-i]$"), 
                    list(`0` = ~sum(. == 0), `1` = ~sum(. == 1), `2` = ~sum(. == 2), `3` = ~sum(. == 3), `4` = ~sum(. == 4), 
                         `5` = ~sum(. == 5), `6` = ~sum(. == 6), `7` = ~sum(. == 7), `8` = ~sum(. == 8), `9` = ~sum(. == 9), 
                         `10` = ~sum(. == 10), `95` = ~sum(. == 95), `96` = ~sum(. == 96), `97` = ~sum(. == 97), `98` = ~sum(. == 98), `99` = ~sum(. == 99)), 
                    .names = "{.col}_{.fn}")
    ) %>% dplyr::ungroup()
  
  df <- dplyr::rename_with(df, ~ ifelse(stringr::str_starts(.x, "cses_imd_imd"), paste0(.x, "_cy"), .x))
  
  return(df)
  
}