#' Translation to_u_vdem_party_country_year
#' 
#' Translates compatible dataset to primary unit vdem_party_country_year.
#' 
#' @param df a data.frame
#'
#' @return data.frame translated to vdem_party_country_year.
#' @examples 
#' to_u_vdem_party_country_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_vdem_party_country_year <- function(df, ...) UseMethod("to_u_vdem_party_country_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_vdem_party_country_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_vdem_party_country_year!")
}


#' @export
to_u_vdem_party_country_year.u_vdem_country_year <- function(df) {
  
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_country_year")
  unit_table_end <- read_unit_table("u_vdem_party_country_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_vdem_country_year_country = NA_character_,
                  u_vdem_country_year_year = u_vdem_party_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_vdem_country_year_country", "u_vdem_party_country_year_country_name")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_vdem_country_year_country =
        ifelse(u_vdem_party_country_year_country_name %in% common_names,
               u_vdem_party_country_year_country_name, NA))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_vdem_country_year_country", 
                                    "u_vdem_country_year_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_vdem_party_country_year", old_ds_class, "data.frame")
  
  return(out)
}