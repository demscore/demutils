#' Translation to u_qog_region
#'
#' Translates compatible dataset to primary unit qog_region
#' 
#' @param df a data.frame.
#' @return data.frame translated to qog_region.
#' @examples 
#' to_u_qog_region(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_region <- function(df, ...) UseMethod("to_u_qog_region")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_qog_region.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_qog_region!")
}

#' @export
to_u_qog_region.u_qog_region_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_region_year")
  df %<>% bind_cols(unit_table_start)
  
  #call end unit table
  unit_table_end <- read_unit_table("u_qog_region")
  
  #create unit cols
  df$u_qog_region_name <- df$u_qog_region_year_region_name
  
  # unit col to match to max year only
  # Only qog_eqi_long has 2021 observations, the three qog_eureg datasets end in 2019.
  # But as qog_eqi_qgg21 has data for 2021 and is the only dataset with the unit u_qog_region, 
  # it makes more sense to select year == 2021 instead of year == max(year)
  df %<>% filter(u_qog_region_year_year == 2021)
  
  #join end unit table
  out <- dplyr::left_join(unit_table_end, df, 
                          by = c("u_qog_region_name"))
  
  #set class
  class(out) <- c("u_qog_region", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_qog_region.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #call end unit table
  unit_table_end <- read_unit_table("u_qog_region")
  
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_country = case_when(
        u_qog_region_country == "Czech Republic" ~ "Czechia",
        u_qog_region_country == "Netherlands" ~ "Netherlands (the)",
        u_qog_region_country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
        TRUE ~ u_qog_region_country
      ))
  
  #create unit cols
  df$u_qog_region_country <- df$u_qog_country_year_country
  
  # unit col to match to max year only
  df %<>% filter(u_qog_country_year_year == 2021)
  
  #join end unit table
  out <- dplyr::left_join(unit_table_end, df, 
                          by = c("u_qog_region_country"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_qog_region", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_qog_region.u_qog_country <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country")
  df %<>% bind_cols(unit_table_start)
  
  #call end unit table
  unit_table_end <- read_unit_table("u_qog_region")
  
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_country = case_when(
        u_qog_region_country == "Czech Republic" ~ "Czechia",
        u_qog_region_country == "Netherlands" ~ "Netherlands (the)",
        u_qog_region_country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
        TRUE ~ u_qog_region_country
      ))
  
  #create unit cols
  df$u_qog_region_country <- df$u_qog_country_country
  
  #join end unit table
  out <- dplyr::left_join(unit_table_end, df, 
                          by = c("u_qog_region_country"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_qog_region", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}
