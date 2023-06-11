#' V-Dem Party-Country-Date to V-Dem Country Year
#' 
#' Aggregates from V-Dem Party-Country-Year to Country Year by only keeping the 
#' observations for the largest party per country-year using the variable v2paseatshare
#' 
#' @param df a data.frame
#' @return data.frame aggregated to country year level
#' @examples 
#' aggregate_vparty_highest_seatshare(df)
#'
#' @export
aggregate_vparty_highest_seatshare <- function(df) {

    df %<>% 
      dplyr::group_by(u_vdem_party_country_year_country_name, 
                      u_vdem_party_country_year_year) %>%
      dplyr::filter(vdem_vparty_v2paseatshare == max(vdem_vparty_v2paseatshare)) %>%
      dplyr::ungroup(.)
    
      return(df)
  
  }