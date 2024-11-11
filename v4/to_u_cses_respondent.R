#' Translation to u_cses_respondent
#'
#' Translates compatible dataset to primary unit ucdp_triad_year
#' 
#' @param df a data.frame.
#' @return data.frame translated to u_cses_respondent
#' @examples 
#' to_u_cses_respondent(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_cses_respondent <- function(df, ...) UseMethod("to_u_cses_respondent")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_cses_respondent.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_cses_respondent!")
}

#' @export
to_u_cses_respondent.u_qog_country_year <- function(df) {
  
    #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_cses_respondent")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_country_year_ccodealp = u_cses_respondent_country_code,
                  u_qog_country_year_year = u_cses_respondent_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_ccodealp", "u_cses_respondent_country_code")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_ccodealp =
        ifelse(u_qog_country_year_ccodealp %in% common_names,
               u_qog_country_year_ccodealp, NA))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                          by = c("u_qog_country_year_ccodealp", 
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
  
  class(out) <- c("u_cses_respondent", old_ds_class, "data.frame")
  
  return(out)
}


#' @export
to_u_cses_respondent.u_vdem_country_year <- function(df) {

  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_country_year")
  unit_table_end <- read_unit_table("u_cses_respondent")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_vdem_country_year_country_id = u_cses_respondent_vdem_country_code,
                  u_vdem_country_year_year = u_cses_respondent_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_vdem_country_year_country_id", "u_cses_respondent_vdem_country_code")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_vdem_country_year_country_id =
        ifelse(u_vdem_country_year_country_id %in% common_names,
               u_vdem_country_year_country_id, NA))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                          by = c("u_vdem_country_year_country_id", 
                                 "u_vdem_country_year_year"))
  
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
  
  class(out) <- c("u_cses_respondent", old_ds_class, "data.frame")
  
  return(out)
}



#' @export
to_u_cses_respondent.u_complab_country_year <- function(df) {
#save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_complab_country_year")
  unit_table_end <- read_unit_table("u_cses_respondent")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_complab_country_year_country_code = u_cses_respondent_country_code,
                  u_complab_country_year_year = u_cses_respondent_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_complab_country_year_country_code", "u_cses_respondent_country_code")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country_code =
        ifelse(u_complab_country_year_country_code %in% common_names,
               u_complab_country_year_country_code, NA))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                          by = c("u_complab_country_year_country_code", 
                                 "u_complab_country_year_year"))
  
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
  
  class(out) <- c("u_cses_respondent", old_ds_class, "data.frame")
  
  return(out)
}


#' @export
to_u_cses_respondent.u_repdem_cabinet_date <- function(df) {
  
  #aggregate from cab date to country date to country year
  df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year() 
  
  unique(df$u_repdem_cabinet_date_country)
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_end <- read_unit_table("u_cses_respondent")
  
  unit_end_original <- unit_table_end
  
  
  
  unit_table_end %<>% 
    dplyr::mutate(
      u_cses_respondent_country = case_when(
        u_cses_respondent_country == "Great Britain" ~ "United Kingdom",
        u_cses_respondent_country == "Czech Republic/Czechia" ~ "Czechia",
        TRUE ~ u_cses_respondent_country
      ))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, df,
                          by = c("u_cses_respondent_country" = "u_repdem_cabinet_date_country", 
                                 "u_cses_respondent_year" = "u_repdem_cabinet_date_year"))
  
  
  unique(out$u_cses_respondent_country)
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  unique(out$u_cses_respondent_country)
  
  # Drop all variables without a single match in the end output unit
  drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }

  unique(out$u_cses_respondent_country)
    
  class(out) <- c("u_cses_respondent", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_cses_respondent.u_hdata_country_year <- function(df) {
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_hdata_country_year")
  unit_table_end <- read_unit_table("u_cses_respondent")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_country_year_ccodealp = u_cses_respondent_country_code,
                  u_hdata_country_year_year = u_cses_respondent_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_country_year_country", "u_cses_respondent_country_code")$common
  
  unit_table_start %<>% 
    dplyr::mutate(
      u_hdata_country_year_country = case_when(
        u_hdata_country_year_country == "United Kingdom" ~ "Great Britain",
        TRUE ~ u_hdata_country_year_country
      ))
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_ccodealp =
        ifelse(u_hdata_country_year_ccodealp %in% common_names,
               u_hdata_country_year_ccodealp, NA))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                          by = c("u_cses_respondent_country" = "u_hdata_country_year_country", 
                                 "u_cses_respondent_year" = "u_hdata_country_year_year"))
  
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
  
  class(out) <- c("u_cses_respondent", old_ds_class, "data.frame")
  
  return(out)
}