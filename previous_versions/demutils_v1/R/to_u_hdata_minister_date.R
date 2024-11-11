#' Translation to hdata_minister_date
#'
#' Translates compatible dataset to primary unit hdata_minister_date
#' 
#' @param df a data.frame.
#' @return data.frame translated to hdata_minister_date.
#' @examples 
#' to_u_hdata_minister_date(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_hdata_minister_date <- function(df, ...) UseMethod("to_u_hdata_minister_date")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_hdata_minister_date.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_hdata_minister_date!")
}


#These translations are by out_year, we can adjust this or add additional options to averages or any other method we may prefer.
#' @export
to_u_hdata_minister_date.u_hdata_country_year <- function(df) {
  
  #save class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_country_year")
  df %<>% dplyr::bind_cols(unit_table_start)
  
  #duplicate country years
  df$u_hdata_minister_date_country <- df$u_hdata_country_year_country
  df$u_hdata_minister_date_year_out <- df$u_hdata_country_year_year
  
  #join end unit table. Matching country year to minister out-year, this can be changed, but seemed to make the most 
  #sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_hdata_minister_date")
  
  out <- dplyr::left_join(unit_table_end, df, 
                   by = c("u_hdata_minister_date_year_out", "u_hdata_minister_date_country")) %>%
    distinct(u_hdata_minister_date_minister, u_hdata_minister_date_date_in,
             u_hdata_minister_date_cowcode, u_hdata_minister_date_year_out, 
             u_hdata_minister_date_country, .keep_all = TRUE)
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_hdata_minister_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
 return (out)
}


#' @export
to_u_hdata_minister_date.u_vdem_country_date <- function(df) {
  
  #save class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_vdem_country_date")
  df %<>% bind_cols(unit_table_start)
  
  #Hdata_minister_date utable for interpolation step
  dfa <- read_unit_table("u_hdata_minister_date")
  
  # unit cols for interpolation, using date_out, this can be adjusted.
  dfa$u_vdem_country_date_date <- dfa$u_hdata_minister_date_date_out
  dfa$u_vdem_country_date_country_name <- dfa$u_hdata_minister_date_country
  
  # Adjust vdem country names to hdata names
  df %<>% 
    dplyr::mutate(u_vdem_country_date_country_name = case_when(
      u_vdem_country_date_country_name == "Turkey" ~ "Ottoman Empire/Turkey",
      u_vdem_country_date_country_name == "Germany" ~ "Prussia/Germany",
      u_vdem_country_date_country_name == "Russia" ~ "Russia/USSR",
      u_vdem_country_date_country_name == "United States of America" ~ "United States",
      TRUE ~ u_vdem_country_date_country_name
    ))
  
  
  # Create data.frame with one row per combined country and date.
  combined_dates <- bind_rows(
    select(df, u_vdem_country_date_country_name, u_vdem_country_date_date),
    select(dfa, u_vdem_country_date_country_name, u_vdem_country_date_date)) %>%
    distinct %>%
    arrange(u_vdem_country_date_country_name, u_vdem_country_date_date)
  
  # Add country-dates to the dataset that is to be translated.
  df %<>%
    full_join(combined_dates, by = c("u_vdem_country_date_country_name", "u_vdem_country_date_date")) %>%
    arrange(u_vdem_country_date_country_name, u_vdem_country_date_date)
  
  # Interpolate all columns within countries from earlier to later dates.
  # This is very dangerous (as some variables should not be interpolated 
  # across non-electoral regimes.). 
  # Interpolate all columns within the same country
  # This can be very slow! ~ 1-2 minutes
  df %<>%
    group_by(u_vdem_country_date_country_name) %>%
    mutate(across(.cols = everything(), 
                  .fns = partial(zoo::na.locf, na.rm = FALSE))) %>%
    ungroup
  
  
  #unit cols for join
  df$u_hdata_minister_date_country <- df$u_vdem_country_date_country_name
  df$u_hdata_minister_date_date_out <- df$u_vdem_country_date_date
  
  
  #join hdata_minister_date utable
  unit_table_end <- read_unit_table("u_hdata_minister_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_hdata_minister_date_country", "u_hdata_minister_date_date_out")) %>% 
    select(-u_vdem_country_date_country_name, -u_vdem_country_date_date)
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_hdata_minister_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_hdata_minister_date.u_repdem_cabinet_date <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #aggregate from cabinet date to country date
  df <- repdem_cabinet_date_to_country_date(df)
  
  #delete newdate column from aggregation and duplicate rows (unneeded in this translation method but could be useful for others).
  df <- dplyr::select(df, -newdate) %>% dplyr::distinct(.)
  
  # Because repdem cabinet date represents an interval during which a cabinet is in power,
  # I am duplicating the data across -all individual dates- within this interval in order to then match
  # on other date units. 
  M <- Map(seq, df$u_repdem_cabinet_date_date_in, df$u_repdem_cabinet_date_date_out, by = "day")
  all_days <- data.frame(
    u_repdem_cabinet_date_country = rep.int(df$u_repdem_cabinet_date_country, vapply(M, length, 1L)), 
    u_repdem_cabinet_date_cab_name = rep.int(df$u_repdem_cabinet_date_cab_name, vapply(M, length, 1L)),
    u_repdem_cabinet_date_date = do.call(c, M)
  )    
  
  df <- left_join(all_days, df, by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_cab_name"))
  
  # Since we are looking at data comparing cabinets and foreign ministers, matching to date_in
  # instead of date_out makes more sense in this case. Foreign ministers are usually cabinet members so matching
  # to the incoming cabinet us more accurate than the outgoing cabinet, unlike other policy our outcome related
  # variables, this can be adjusted.
  
  #duplicate unit cols
  df$u_hdata_minister_date_country <- df$u_repdem_cabinet_date_country
  df$u_hdata_minister_date_date_in <- df$u_repdem_cabinet_date_date
  
  #adjust country names
  df %<>% 
    dplyr::mutate(
      u_hdata_minister_date_country = case_when(
        u_repdem_cabinet_date_country == "the Netherlands" ~ "Netherlands",
        u_repdem_cabinet_date_country == "Germany" ~ "Prussia/Germany",
        TRUE ~ u_hdata_minister_date_country
      ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_hdata_minister_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_hdata_minister_date_country", "u_hdata_minister_date_date_in"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_hdata_minister_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_hdata_minister_date.u_hdata_dyad_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_dyad_year")
  df %<>% bind_cols(unit_table_start)
  
  df %<>% hdata_dyad_year_to_country_year()
  
  #duplicate country years
  df$u_hdata_minister_date_country <- df$u_hdata_dyad_year_country_one
  df$u_hdata_minister_date_year_out <- df$u_hdata_dyad_year_year
  
  #join end unit table. Matching country year to minister out-year, this can be changed, but seemed to make the most 
  #sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_hdata_minister_date")
  
  out <- dplyr::left_join(unit_table_end, df, 
                          by = c("u_hdata_minister_date_year_out", "u_hdata_minister_date_country")) 
  
  # Indicate missing values from translation
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_hdata_minister_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}