#' Translation to u_qog_agency_year
#' 
#' Translates compatible dataset to primary unit qog_agency_year.
#' 
#' @param df a data.frame
#' @return data.frame translated to qog_agency_year.
#' @examples 
#' to_u_qog_agency_year(df)
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_agency_year <- function(df, ...) UseMethod("to_u_qog_agency_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_qog_agency_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_qog_agency_year!")
}

#' @export
to_u_qog_agency_year.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_qog_agency_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_agency_year_country = "Sweden",
                  u_qog_country_year_year = u_qog_agency_year_agency_fy)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_qog_agency_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_qog_agency_year_country %in% common_names,
               u_qog_agency_year_country, NA))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_agency_year_country" = "u_qog_country_year_country", 
                                    "u_qog_agency_year_agency_fy" = "u_qog_country_year_year"))
  
  out <- select(out, -u_qog_agency_year_country)
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_agency_year", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_qog_agency_year.u_qog_agency_inst <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_agency_inst")
  unit_table_end <- read_unit_table("u_qog_agency_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_agency_inst_agency_id = u_qog_agency_year_agency_id) 
  
  unit_table_end$u_qog_agency_year_agency_fy <- as.character(unit_table_end$u_qog_agency_year_agency_fy)
  
  # Common ids:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_agency_inst_agency_id", "u_qog_agency_year_agency_id")$common
  
  # Prefill where ids are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_agency_inst_agency_id =
        ifelse(u_qog_agency_year_agency_id %in% common_names,
               u_qog_agency_year_agency_id, NA))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  int_df$temp_year <- substr(int_df$u_qog_agency_inst_agency_instruction, 1, 4)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_agency_inst_agency_id", 
                                    "u_qog_agency_year_agency_fy" = "temp_year")) %>%
    distinct(u_qog_agency_inst_agency_id, u_qog_agency_year_agency_fy, .keep_all = TRUE)
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_agency_year", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_qog_agency_year.u_qog_region_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_region_year")
  unit_table_end <- read_unit_table("u_qog_agency_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_agency_year_country = "Sweden",
                  u_qog_region_year_year = u_qog_agency_year_agency_fy)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_region_year_region_name", "u_qog_agency_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_year_region_name =
        ifelse(u_qog_agency_year_country %in% common_names,
               u_qog_agency_year_country, NA))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_agency_year_country" = "u_qog_region_year_region_name", 
                                    "u_qog_agency_year_agency_fy" = "u_qog_region_year_year"))
  
  out <- select(out, -u_qog_agency_year_country)
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_agency_year", old_ds_class, "data.frame")
  
  return(out)
}