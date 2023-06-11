#' Translation to u_complab_country_year
#'
#' Translates compatible dataset to primary unit complab_country_year.
#' 
#' @param df a data.frame.
#' @return data.frame translated to complab_country_year.
#' @examples 
#' to_u_complab_country_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_complab_country_year <- function(df, ...) UseMethod("to_u_complab_country_year")

#' @export
to_u_complab_country_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_complab_country_year!")
}

#' @export
to_u_complab_country_year.u_vdem_country_year <- function(df, ...) {
  
  #save class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_vdem_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_complab_country_year_country <- df$u_vdem_country_year_country
  df$u_complab_country_year_year <- df$u_vdem_country_year_year
  
  #adjust unit cols
  df %<>% 
    dplyr::mutate(
      u_complab_country_year_country = case_when(
        u_vdem_country_year_country == "United States of America" ~ "United States",
        u_vdem_country_year_country == "Czechia" ~ "Czech Republic",
        TRUE ~ u_complab_country_year_country
      ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_complab_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_complab_country_year_country", "u_complab_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_complab_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}


#' @export
to_u_complab_country_year.u_qog_country_year <- function(df, ...) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_complab_country_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_country_year_country = NA_character_,
                  u_qog_country_year_year = u_complab_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_complab_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_complab_country_year_country %in% common_names,
               u_complab_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(u_qog_country_year_country = case_when(
      u_complab_country_year_country == "South Korea" ~ "Korea (the Republic of)",
      u_complab_country_year_country == "Netherlands" ~ "Netherlands (the)",
      u_complab_country_year_country == "United States of America" ~ "United States of America (the)",
      u_complab_country_year_country == "Czech Republic" ~ "Czechia",
      u_complab_country_year_country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
      # ...
      TRUE ~ u_qog_country_year_country
    ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output dataframe
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_qog_country_year_country", 
                                    "u_qog_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_complab_country_year", old_ds_class, "data.frame")
  
  return(out)
  
}

#' @export
to_u_complab_country_year.u_hdata_country_year <- function(df, ...) {
  
  stopifnot(is.data.frame(df))
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_hdata_country_year")
  unit_table_end <- read_unit_table("u_complab_country_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_country_year_country = NA_character_,
                  u_hdata_country_year_year = u_complab_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_country_year_country", "u_complab_country_year_country")$common
  
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country =
        ifelse(u_complab_country_year_country %in% common_names,
               u_complab_country_year_country, NA))
  
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country = case_when(
        u_complab_country_year_country == "South Korea" ~ "Korea/South Korea",
        # ...
        TRUE ~ u_hdata_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_hdata_country_year_country", 
                                    "u_hdata_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  class(out) <- c("u_complab_country_year", old_ds_class, "data.frame")
  
  return(out)
  
}


#' @export
to_u_complab_country_year.u_repdem_cabinet_date <- function(df) {
 
  #aggregation functions, cab date to country date to country year
   df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year() 
  
   #save class
  old_ds_class <- class(df)[2]
  
  #duplicate unit cols
  df$u_complab_country_year_country <- df$u_repdem_country_year_country
  df$u_complab_country_year_year <- df$u_repdem_country_year_year
  
  #adjust country names
  df %<>% 
    dplyr::mutate(
      u_complab_country_year_country = case_when(
        u_repdem_country_year_country == "Netherlands" ~ "the Netherlands",
        TRUE ~ u_complab_country_year_country
      ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_complab_country_year")

  out <- dplyr::left_join(unit_table_end, 
                          df, 
                          by = c("u_complab_country_year_country", 
                                 "u_complab_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_complab_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}


#' @export
to_u_complab_country_year.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  unit_table_end <- read_unit_table("u_complab_country_year")
  df %<>% bind_cols(unit_table_start)
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_complab_country_year_country" = "u_ucdp_orgv_country_year_country_cy",
                          "u_complab_country_year_year" = "u_ucdp_orgv_country_year_year_cy"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_complab_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_complab_country_year.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.numeric(unit_table_start$u_ucdp_country_year_gwno_a)
  unit_table_end <- read_unit_table("u_complab_country_year")
  df %<>% bind_cols(unit_table_start)
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_complab_country_year_country" = "u_ucdp_country_year_name",
                          "u_complab_country_year_year" = "u_ucdp_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_complab_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}