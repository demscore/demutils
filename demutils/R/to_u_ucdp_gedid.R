#' Translation to u_ucdp_gedid
#'
#' Translates compatible dataset to primary unit ucdp_gedid
#' 
#' @param df a data.frame.
#' @return data.frame translated to cucdp_gedid
#' @examples 
#' to_u_ucdp_gedid(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_ucdp_gedid <- function(df, ...) UseMethod("to_u_ucdp_gedid")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_ucdp_gedid.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_ucdp_gedid!")
}

#' @export
to_u_ucdp_gedid.u_ucdp_conflict_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_ucdp_conflict_year")
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_ucdp_conflict_year_conflict_id = u_ucdp_gedid_conflict_new_id,
                  u_ucdp_conflict_year_year = u_ucdp_gedid_year)
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_ucdp_gedid_conflict_new_id" = "u_ucdp_conflict_year_conflict_id", 
                                    "u_ucdp_gedid_year" = "u_ucdp_conflict_year_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_ucdp_gedid.u_ucdp_dyad_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_ucdp_dyad_year")
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_ucdp_dyad_year_dyad_id = u_ucdp_gedid_dyad_new_id,
                  u_ucdp_dyad_year_year = u_ucdp_gedid_year)
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df) 
  int_df$u_ucdp_dyad_year_dyad_id <- as.integer(int_df$u_ucdp_dyad_year_dyad_id)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_ucdp_dyad_year_dyad_id", 
                                    "u_ucdp_dyad_year_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_ucdp_gedid.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_ucdp_country_year_gwno_a = u_ucdp_gedid_gwno_a,
                  u_ucdp_country_year_year = u_ucdp_gedid_year)
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df) 
  int_df$u_ucdp_country_year_gwno_a <- as.character(int_df$u_ucdp_country_year_gwno_a)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_ucdp_country_year_gwno_a", 
                                    "u_ucdp_country_year_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_ucdp_gedid.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_ucdp_gedid_country <- df$u_qog_country_year_country
  df$u_ucdp_gedid_year <- df$u_qog_country_year_year
  
  #adjust country names
  df %<>% 
    dplyr::mutate(u_ucdp_gedid_country = case_when(
      u_qog_country_year_country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      u_qog_country_year_country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina",
      u_qog_country_year_country == "Cambodia" ~ "Cambodia (Kampuchea)",
      u_qog_country_year_country == "Congo (the Democratic Republic of the)" ~ "DR Congo (Zaire)",
      u_qog_country_year_country == "Congo (the)" ~ "Congo",
      u_qog_country_year_country == "Iran (Islamic Republic of)" ~ "Iran",
      u_qog_country_year_country == "Lao People's Democratic Republic (the)" ~ "Laos",
      u_qog_country_year_country == "Eswatini" ~ "Kingdom of eSwatini (Swaziland)",
      u_qog_country_year_country == "North Macedonia" ~ "Macedonia, FYR",
      u_qog_country_year_country == "Madagascar" ~ "Madagascar (Malagasy)",
      u_qog_country_year_country == "Moldova (the Republic of)" ~ "Moldova",
      u_qog_country_year_country == "Netherlands (the)" ~ "Netherlands",
      u_qog_country_year_country == "Niger (the)" ~ "Niger",
      u_qog_country_year_country == "Philippines (the)" ~ "Philippines",
      u_qog_country_year_country == "Sudan (the)" ~ "Sudan",
      u_qog_country_year_country == "Syrian Arab Republic (the)" ~ "Syria",
      u_qog_country_year_country == "Tanzania, the United Republic of" ~ "Tanzania",
      u_qog_country_year_country == "United Arab Emirates (the)" ~ "United Arab Emirates",
      u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
      u_qog_country_year_country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      u_qog_country_year_country == "United States of America (the)" ~ "United States of America",
      u_qog_country_year_country == "Myanmar" ~ "Myanmar (Burma)",
      u_qog_country_year_country == "Côte d'Ivoire" ~ "Ivory Coast",
      # QoG CY Unit table has data for USSR until 2020!
      u_qog_country_year_country == "Russian Federation (the)" & 
        u_qog_country_year_year >= 1992 ~ "Russia (Soviet Union)",
      u_qog_country_year_country == "USSR" & 
        u_qog_country_year_year <= 1991 ~ "Russia (Soviet Union)",
      u_qog_country_year_country == "Serbia and Montenegro" &
        u_qog_country_year_year <= 2005 ~ "Serbia (Yugoslavia)",
      u_qog_country_year_country == "Serbia" &
        u_qog_country_year_year >= 2006  ~ "Serbia (Yugoslavia)",
      u_qog_country_year_country == "Yemen" ~ "Yemen (North Yemen)",
      u_qog_country_year_country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
      TRUE ~ u_ucdp_gedid_country
    ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_gedid_country", "u_ucdp_gedid_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}

#' @export
to_u_ucdp_gedid.u_vdem_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_vdem_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_ucdp_gedid_country <- df$u_vdem_country_year_country
  df$u_ucdp_gedid_year <- df$u_vdem_country_year_year
  
  #adjust country names
  df %<>% 
    dplyr::mutate(u_ucdp_gedid_country = case_when(
      u_vdem_country_year_country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina" ,
      u_vdem_country_year_country == "Cambodia" ~ "Cambodia (Kampuchea)",
      u_vdem_country_year_country == "Democratic Republic of the Congo" ~ "DR Congo (Zaire)",
      u_vdem_country_year_country == "Republic of the Congo" ~ "Congo",
      u_vdem_country_year_country == "The Gambia" ~ "Gambia",
      u_vdem_country_year_country == "Burma/Myanmar" ~ "Myanmar (Burma)",
      u_vdem_country_year_country == "Russia" ~ "Russia (Soviet Union)",
      u_vdem_country_year_country == "Serbia" ~ "Serbia (Yugoslavia)",
      u_vdem_country_year_country == "Yemen" ~ "Yemen (North Yemen)",
      u_vdem_country_year_country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
      u_vdem_country_year_country == "Eswatini (former Swaziland)" ~ "Kingdom of eSwatini (Swaziland)",
      u_vdem_country_year_country == "North Macedonia" ~ "Macedonia, FYR",
      TRUE ~ u_ucdp_gedid_country
    ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_gedid_country", "u_ucdp_gedid_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_ucdp_gedid.u_complab_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_complab_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_ucdp_gedid_country <- df$u_complab_country_year_country
  df$u_ucdp_gedid_year <- df$u_complab_country_year_year
  
  #adjust country names
  df %<>% 
    dplyr::mutate(u_ucdp_gedid_country = case_when(
      u_complab_country_year_country == "United States" ~ "United States of America",
      TRUE ~ u_ucdp_gedid_country
    ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_gedid_country", "u_ucdp_gedid_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_ucdp_gedid.u_hdata_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_ucdp_gedid_country <- df$u_hdata_country_year_country
  df$u_ucdp_gedid_year <- df$u_hdata_country_year_year
  
  #adjust country names
  df %<>% 
    dplyr::mutate(u_ucdp_gedid_country = case_when(
      u_hdata_country_year_country == "Myanmar" ~ "Myanmar (Burma)",
      u_hdata_country_year_country == "Russia" ~ "Russia (Soviet Union)",
      u_hdata_country_year_country == "Serbia/Yugoslavia" ~ "Serbia (Yugoslavia)",
      u_hdata_country_year_country == "Tripolitania/Libya" ~ "Libya",
      u_hdata_country_year_country == "Nejd/Saudi Arabia" ~ "Saudi Arabia",
      u_hdata_country_year_country == "United States" ~ "United States of America",
      u_hdata_country_year_country == "Yemen" ~ "Yemen (North Yemen)",
      u_hdata_country_year_country == "Bukhara/Uzbekistan" ~ "Uzbekistan",
      u_hdata_country_year_country == "Madagascar" ~ "Madagascar (Malagasy)",
      TRUE ~ u_ucdp_gedid_country
    ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_gedid_country", "u_ucdp_gedid_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_ucdp_gedid.u_repdem_cabinet_date <- function(df) {
  
  #aggregate from cab date to country date to country year
  df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year()
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #duplicate unit cols
  df$u_ucdp_gedid_country <- df$u_repdem_cabinet_date_country
  df$u_ucdp_gedid_year <- df$u_repdem_cabinet_date_year
  
  #adjust country names
  df %<>% 
    dplyr::mutate(
      u_ucdp_gedid_country = case_when(
        u_repdem_cabinet_date_country == "the Netherlands" ~ "Netherlands",
        TRUE ~ u_ucdp_gedid_country
      ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_ucdp_gedid")
  
  out <- dplyr::left_join(unit_table_end, 
                          df, 
                          by = c("u_ucdp_gedid_country", 
                                 "u_ucdp_gedid_year"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_ucdp_gedid", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}