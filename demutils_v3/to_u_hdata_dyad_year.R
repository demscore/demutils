#' Translation to u_hdata_dyad_year
#' 
#' Translates compatible dataset to primary unit hdata_dyad_year.
#' 
#' @param df a data.frame
#' @return data.frame translated to hdata_dyad_year.
#' @examples 
#' to_u_hdata_dyad_year(df)
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_hdata_dyad_year <- function(df, ...) UseMethod("to_u_hdata_dyad_year")
# This function can find the first unit table?
# This function can find the second unit table?
#' @export
to_u_hdata_dyad_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_hdata_dyad_year!")
}

#' @export
to_u_hdata_dyad_year.u_hdata_country_year <- function(df, ...) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_hdata_country_year")
  unit_table_end <- read_unit_table("u_hdata_dyad_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_country_year_country = NA_character_,
                  u_hdata_country_year_year = u_hdata_dyad_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_country_year_country", "u_hdata_dyad_year_country_one")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country =
        ifelse(u_hdata_dyad_year_country_one %in% common_names,
               u_hdata_dyad_year_country_one, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    mutate(
      u_hdata_country_year_country = case_when(
        u_hdata_dyad_year_country_one == "Uzbekistan" ~ "Bukhara/Uzbekistan" ,
        u_hdata_dyad_year_country_one == "Korea" ~ "Korea/South Korea",
        u_hdata_country_year_country == "Meckelnburg Schwerin" ~ "Mecklenburg Schwerin",
        u_hdata_dyad_year_country_one == "Burma" ~ "Myanmar",
        u_hdata_dyad_year_country_one == "Saudi Arabia" ~ "Nejd/Saudi Arabia",
        u_hdata_dyad_year_country_one == "Serbia" ~ "Serbia/Yugoslavia",
        u_hdata_dyad_year_country_one == "Libya" ~ "Tripolitania/Libya",
        u_hdata_dyad_year_country_one == "United States of America" ~ "United States",
        u_hdata_dyad_year_country_one == "Würtemberg" ~ "Wuerttemburg",
        TRUE ~ u_hdata_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_hdata_country_year_country", 
                                    "u_hdata_country_year_year"))
  
  # Indicate missing values from translation
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
  
  class(out) <- c("u_hdata_dyad_year", old_ds_class, "data.frame")
  
  return(out)
} 


#' @export
to_u_hdata_dyad_year.u_hdata_minister_date <- function(df, ...) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_minister_date")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  country_date <- hdata_minister_date_to_country_date(df)
  df <- hdata_country_date_to_country_year_ministers(country_date)
  
  #drop unnecessary unit cols
  df %<>% select(
    -u_hdata_minister_date_minister,
    -u_hdata_minister_date_date_in,
    -u_hdata_minister_date_date_out,
    -u_hdata_minister_date_year_in,
    -u_hdata_minister_date_year_out,
    -day_count)
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_end <- read_unit_table("u_hdata_dyad_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_country_year_country = NA_character_,
                  u_hdata_country_year_year = u_hdata_dyad_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_country_year_country", "u_hdata_dyad_year_country_one")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country =
        ifelse(u_hdata_dyad_year_country_one %in% common_names,
               u_hdata_dyad_year_country_one, NA))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, df, 
                             by = c("u_hdata_dyad_year_country_one" = "u_hdata_minister_date_country", 
                                    "u_hdata_dyad_year_year" = "u_hdata_minister_date_year"))
  
  # Indicate missing values from translation
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
  
  class(out) <- c("u_hdata_dyad_year", old_ds_class, "data.frame")
  
  return(out)
  
}
    