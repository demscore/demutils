#' Translation to u_qog_resp_eqi_1013
#'
#' Translates compatible dataset to primary unit u_qog_resp_eqi_1013
#' 
#' @param df a data.frame.
#' @return data.frame translated to u_qog_resp_eqi_1013r
#' @examples 
#' to_u_qog_resp_eqi_1013(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_resp_eqi_1013 <- function(df, ...) UseMethod("to_u_qog_resp_eqi_1013")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_qog_resp_eqi_1013.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_qog_resp_eqi_1013!")
}

#' @export
to_u_qog_resp_eqi_1013.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_qog_resp_eqi_1013")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_country_year_country = u_qog_resp_eqi_1013_country,
                  u_qog_country_year_year = u_qog_resp_eqi_1013_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_qog_resp_eqi_1013_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_qog_country_year_country %in% common_names,
               u_qog_country_year_country, NA))
  
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country = case_when(
        u_qog_resp_eqi_1013_country == "Netherlands" ~ "Netherlands (the)",
        u_qog_resp_eqi_1013_country == "Czech Republic" ~ "Czechia",
        u_qog_resp_eqi_1013_country == "UK" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
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
  
  class(out) <- c("u_qog_resp_eqi_1013", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_qog_resp_eqi_1013.u_qog_region_year <- function(df) {
  
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_region_year")
  
  unit_table_start %<>%
    mutate(u_qog_region_year_region = case_when(
      u_qog_region_year_region == "IE" ~ "IE0",
      TRUE ~ u_qog_region_year_region
    ))
  
  unit_table_end <- read_unit_table("u_qog_resp_eqi_1013")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_region_year_region_name = u_qog_resp_eqi_1013_country,
                  u_qog_region_year_year = u_qog_resp_eqi_1013_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_region_year_region_name", "u_qog_resp_eqi_1013_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_year_region_name =
        ifelse(u_qog_region_year_region_name %in% common_names,
               u_qog_region_year_region_name, NA))
  
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_year_region_name = case_when(
        u_qog_resp_eqi_1013_country == "Netherlands" ~ "Netherlands (the)",
        u_qog_resp_eqi_1013_country == "Czech Republic" ~ "Czechia",
        u_qog_resp_eqi_1013_country == "UK" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
        TRUE ~ u_qog_region_year_region_name
      )) 
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_region_year_region_name", 
                                    "u_qog_region_year_year")) %>%
    distinct(u_qog_resp_eqi_1013_resp_id, .keep_all = TRUE)
  
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
  
  class(out) <- c("u_qog_resp_eqi_1013", old_ds_class, "data.frame")
  
  return(out)
}