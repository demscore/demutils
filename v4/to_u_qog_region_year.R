#' Translation to qog_region_year
#'
#' Translates compatible dataset to primary unit qog_region_year
#' 
#' @param df a data.frame.
#' @return data.frame translated to qog_region_year.
#' @examples 
#' to_u_qog_region_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_region_year <- function(df, ...) UseMethod("to_u_qog_region_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_qog_region_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_qog_region_year!")
}

#' @export
to_u_qog_region_year.u_qog_region <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_region")
  df %<>% bind_cols(unit_table_start)
  
  #call end unit table
  unit_table_end <- read_unit_table("u_qog_region_year")
  
  #create unit cols
  df$u_qog_region_year_region_name <- df$u_qog_region_name
  
  # unit col to match to max year only
  
  df$u_qog_region_year_year <- max(unit_table_end$u_qog_region_year_year)
  
  #join end unit table
  out <- dplyr::left_join(unit_table_end, df, 
                          by = c("u_qog_region_year_region_name", "u_qog_region_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_qog_region_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_qog_region_year.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_qog_region_year")
  
  # Extract only regions on the NUTS0, i.e. country level. These regions have a two letter code
  # unit_table_end %<>%  dplyr::filter(stringr::str_length(u_qog_region_year_region) == 2)
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_country_year_country = NA_character_,
                  u_qog_country_year_year = u_qog_region_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_qog_region_year_region_name")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_qog_region_year_region_name %in% common_names,
               u_qog_region_year_region_name, NA))
  
  # Adjust manually where names differ
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country = case_when(
        u_qog_region_year_region_name == "Czech Republic" ~ "Czechia",
        u_qog_region_year_region_name == "Netherlands" ~ "Netherlands (the)",
        u_qog_region_year_region_name == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
        TRUE ~ u_qog_country_year_country
      ))

  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_country_year_country", 
                                    "u_qog_country_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  class(out) <- c("u_qog_region_year", old_ds_class, "data.frame")
  
  return(out)
}