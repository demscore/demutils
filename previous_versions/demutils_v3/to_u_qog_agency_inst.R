#' Translation to u_qog_agency_inst
#'
#' Translates compatible dataset to primary unit ucdp_triad_year
#' 
#' @param df a data.frame.
#' @return data.frame translated to cucdp_triad_year
#' @examples 
#' to_u_qog_agency_inst(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_agency_inst <- function(df, ...) UseMethod("to_u_qog_agency_inst")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_qog_agency_inst.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_qog_agency_inst!")
}

#' @export
to_u_qog_agency_inst.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_qog_agency_inst")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_agency_inst_country = "Sweden")
  
  unit_table_end$u_qog_agency_inst_year <- 
    substr(unit_table_end$u_qog_agency_inst_agency_instruction, 1, 4)
  
  unit_table_end %<>%
    dplyr::mutate(u_qog_agency_inst_year = case_when(
      u_qog_agency_inst_agency_instruction == "Instruktion 14 december 1967" ~ "1967" ,
      u_qog_agency_inst_agency_instruction == "Instruktion 15 maj 1959" ~ "1959",
      u_qog_agency_inst_agency_instruction == "Instruktion 19 mars 1971" ~ "1971",
      u_qog_agency_inst_agency_instruction == "Instruktion 1939 21 augusti" ~ "1939" ,
      u_qog_agency_inst_agency_instruction == "Instruktion 1946 28 juni" ~ "1946",
      u_qog_agency_inst_agency_instruction == "Instruktion 1955 15 april" ~ "1955",
      u_qog_agency_inst_agency_instruction == "Instruktion 1962 28 juni" ~ "1962",
      u_qog_agency_inst_agency_instruction == "Instruktion 1963 31 maj" ~ "1963",
      u_qog_agency_inst_agency_instruction == "Instruktion 6 juni 1962" ~ "1963",
      u_qog_agency_inst_agency_instruction == "Instruktionen 1974-04-19" ~ "1974",
      u_qog_agency_inst_agency_instruction == "Kungl. Maj:ts bestämmelser 1/7 1960" ~ "1960",
      TRUE ~ u_qog_agency_inst_year
    )) 
  
  unit_table_end$u_qog_agency_inst_year <- as.integer(unit_table_end$u_qog_agency_inst_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_qog_agency_inst_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_qog_agency_inst_country %in% common_names,
               u_qog_agency_inst_country, NA))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_agency_inst_country" = "u_qog_country_year_country", 
                                    "u_qog_agency_inst_year" = "u_qog_country_year_year"))
  
  out <- select(out, -u_qog_agency_inst_country, -u_qog_agency_inst_year)
  
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
  
  class(out) <- c("u_qog_agency_inst", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_qog_agency_inst.u_qog_agency_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_agency_year")
  unit_table_end <- read_unit_table("u_qog_agency_inst")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_agency_year_agency_id = u_qog_agency_inst_agency_id)
  
  # Common ids:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_agency_year_agency_id", "u_qog_agency_inst_agency_id")$common
  
  # Prefill where ids are common
  unit_table_end %<>%
    dplyr::mutate(
      u_qog_agency_year_agency_id =
        ifelse(u_qog_agency_inst_agency_id %in% common_names,
               u_qog_agency_inst_agency_id, NA))
  
  unit_table_end$temp_year <- as.integer(substr(unit_table_end$u_qog_agency_inst_agency_instruction, 1, 4))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data 
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_agency_year_agency_id", "temp_year" = "u_qog_agency_year_agency_fy")) %>%
    rename(u_qog_agency_year_agency_fy = temp_year) 
  
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
  
  class(out) <- c("u_qog_agency_inst", old_ds_class, "data.frame")
  
  return(out)
}