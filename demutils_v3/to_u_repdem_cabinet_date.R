#' Translation to u_repdem_cabinet_date
#'
#' Translates compatible dataset to primary unit repdem_cabinet_date
#' 
#' @param df a data.frame.
#' @return data.frame translated to repdem_cabinet_date.
#' @examples 
#' to_u_repdem_cabinet_date(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_repdem_cabinet_date <- function(df, ...) UseMethod("to_u_repdem_cabinet_date")

#' @export
to_u_repdem_cabinet_date.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_repdem_cabinet_date!")
}


#' @export
to_u_repdem_cabinet_date.u_vdem_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_vdem_country_year")
  df %<>% bind_cols(unit_table_start)
  
  
  #duplicate unit columns
  df$u_repdem_cabinet_date_out_year <- df$u_vdem_country_year_year
  df$u_repdem_cabinet_date_country<- df$u_vdem_country_year_country
  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_out_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111

  # Drop all variables without a single match in the end output unit
  #  drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  #  if (length(drop_cols) > 0) {
  #    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  #  } else {
  #    out <- out
  #  }
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_cabinet_date.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_repdem_cabinet_date_out_year <- df$u_qog_country_year_year
  df$u_repdem_cabinet_date_country<- df$u_qog_country_year_country
  
  #adjust country names
  df %<>% 
    dplyr::mutate(
      u_repdem_cabinet_date_country = case_when(
        u_qog_country_year_country == "Netherlands (the)" ~ "Netherlands",
        u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
        u_qog_country_year_country == "United States of America (the)" ~ "United States",
        TRUE ~ u_repdem_cabinet_date_country
      ))
  
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  # Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_out_year"))
  
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
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_cabinet_date.u_hdata_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_repdem_cabinet_date_out_year <- df$u_hdata_country_year_year
  df$u_repdem_cabinet_date_country<- df$u_hdata_country_year_country

  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_out_year"))
  
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
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_cabinet_date.u_complab_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_complab_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  # Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  df$u_repdem_cabinet_date_out_year <- df$u_complab_country_year_year
  df$u_repdem_cabinet_date_country<- df$u_complab_country_year_country

  
  #join end unit table
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_out_year"))
  
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
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_cabinet_date.u_vdem_country_date <- function(df) {

 #save old class
old_ds_class <- class(df)[2]

#bind unit table
unit_table_start <- read_unit_table("u_vdem_country_date")
df %<>% bind_cols(unit_table_start)

#Hdata_minister_date utable for interpolation step
dfa <- read_unit_table("u_repdem_cabinet_date")

# unit  cols for interpolation, using date_out, this can be adjusted.
dfa$u_vdem_country_date_date <- dfa$u_repdem_cabinet_date_date_out
dfa$u_vdem_country_date_country_name <- dfa$u_repdem_cabinet_date_country

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
df$u_repdem_cabinet_date_country <- df$u_vdem_country_date_country_name
df$u_repdem_cabinet_date_date_out <- df$u_vdem_country_date_date


#hdata_minister_date utable for join
unit_table_end <- read_unit_table("u_repdem_cabinet_date")

#join end unit table
out <- left_join(unit_table_end, df, 
                 by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_date_out")) %>% 
  select(-u_vdem_country_date_country_name, -u_vdem_country_date_date)

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

#set class
class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")

stopifnot(nrow(unit_table_end) == nrow(out))

return (out)
}

#' @export
to_u_repdem_cabinet_date.u_hdata_minister_date <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_minister_date")
  df %<>% bind_cols(unit_table_start)
  
  # aggregating to country date by dropping overlaps
  df <- hdata_minister_date_to_country_date(df)
  
  #getting rid of newdate col and duplicated rows, don't need them for this method.
  df <- dplyr::select(df, -u_hdata_minister_date_newdate) %>% dplyr::distinct(.)
  
  
  # Because hdata minister date represents an interval during which a cabinet is in power,
  # I am duplicating the data across -all individual dates- within this interval in order to then match
  # on other date units.
  
  M <- Map(seq, df$u_hdata_minister_date_date_in, df$u_hdata_minister_date_date_out, by = "day")
  all_days <- data.frame(
    u_hdata_minister_date_country = rep.int(df$u_hdata_minister_date_country, vapply(M, length, 1L)), 
    u_hdata_minister_date_minister = rep.int(df$u_hdata_minister_date_minister, vapply(M, length, 1L)),
    u_hdata_minister_date_date_in = rep.int(df$u_hdata_minister_date_date_in, vapply(M, length, 1L)),
    u_hdata_minister_date_date_out = rep.int(df$u_hdata_minister_date_date_out, vapply(M, length, 1L)),
    u_hdata_minister_date_date = do.call(c, M)
  )    
  
  df <- left_join(all_days, df, by = 
                    c("u_hdata_minister_date_country", "u_hdata_minister_date_minister", "u_hdata_minister_date_date_in", "u_hdata_minister_date_date_out"))
  
  # Since we are looking at data comparing cabinets and foreign ministers, matching to date_in
  # instead of date_out makes more sense in this case. Foreign ministers are usually cabinet members so matching
  # to the incoming cabinet us more accurate than the outgoing cabinet, unlike other policy our outcome related
  # variables, this can be adjusted.
  
  #create unit cols
  df$u_repdem_cabinet_date_country <- df$u_hdata_minister_date_country
  df$u_repdem_cabinet_date_date_in <- df$u_hdata_minister_date_date
  
  #adjust country names
  df %<>% 
    mutate(
      u_repdem_cabinet_date_country  = case_when(
        u_hdata_minister_date_country == "Turkey" ~ "Ottoman Empire/Turkey",
        u_hdata_minister_date_country == "Germany" ~ "Prussia/Germany",
        u_hdata_minister_date_country == "Russia" ~ "Russia/USSR",
        u_hdata_minister_date_country == "United States of America" ~ "United States",
        TRUE ~ u_repdem_cabinet_date_country 
      ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_date_in"))
  
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
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_cabinet_date.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  df %<>% bind_cols(unit_table_start)
  
  
  #duplicate unit columns
  df$u_repdem_cabinet_date_out_year <- df$u_ucdp_orgv_country_year_year_cy
  df$u_repdem_cabinet_date_country<- df$u_ucdp_orgv_country_year_country_cy
  
  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_out_year"))
  
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
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_cabinet_date.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.numeric(unit_table_start$u_ucdp_country_year_gwno_a)
  df %<>% bind_cols(unit_table_start)
  
  
  #duplicate unit columns
  df$u_repdem_cabinet_date_out_year <- df$u_ucdp_country_year_year
  df$u_repdem_cabinet_date_country<- df$u_ucdp_country_year_name
  
  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_cabinet_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_cabinet_date_country", "u_repdem_cabinet_date_out_year"))
  
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
  
  #set class
  class(out) <- c("u_repdem_cabinet_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}
