#' Translation to u_vdem_country_year
#' 
#' Translates compatible dataset to primary unit vdem_country_year.
#' 
#' @param df a data.frame
#' @return data.frame translated to vdem_country_year.
#' @examples 
#' to_u_vdem_country_year(df)
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_vdem_country_year <- function(df, ...) UseMethod("to_u_vdem_country_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_vdem_country_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_vdem_country_year!")
}


#' @export
to_u_vdem_country_year.u_qog_country_year <- function(df, ...) {
  
  stopifnot(is.data.frame(df))
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_vdem_country_year")
  
  stopifnot(nrow(df) == nrow(unit_table_start))
  
  
  #Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_country_year_country = NA_character_,
                  u_qog_country_year_year = u_vdem_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_vdem_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_vdem_country_year_country %in% common_names,
               u_vdem_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country = case_when(
        u_vdem_country_year_country == "Democratic Republic of the Congo" ~ "Congo (the Democratic Republic of the)",
        u_vdem_country_year_country == "The Gambia" ~ "Gambia (the)",
        u_vdem_country_year_country == "Ivory Coast" ~ "Cote d'Ivoire",
        u_vdem_country_year_country == "North Korea" ~ "Korea (the Democratic People's Republic of)",
        u_vdem_country_year_country == "Russia" & 
          u_vdem_country_year_year <= 1991 ~ "USSR",
        u_vdem_country_year_country == "Russia" & 
          u_vdem_country_year_year >= 1992 ~ "Russian Federation (the)",
        u_vdem_country_year_country == "South Korea" ~ "Korea (the Republic of)",
        u_vdem_country_year_country == "United States of America" ~ "United States of America (the)",
        u_vdem_country_year_country == "Vietnam" & 
          u_vdem_country_year_year >= 1977 ~ "Viet Nam",
        u_vdem_country_year_country == "Vietnam" & 
          u_vdem_country_year_year <= 1977 & 
          u_vdem_country_year_year >= 1945 ~ "Vietnam, North",
        u_vdem_country_year_country == "Republic of Vietnam" ~ "Vietnam, South",
        u_vdem_country_year_country == "Yemen" & 
          u_vdem_country_year_year <= 1989 ~ "North Yemen",
        u_vdem_country_year_country == "Yemen" & 
          u_vdem_country_year_year >= 1990 ~ "Yemen",
        u_vdem_country_year_country == "South Yemen" ~ "Yemen, South",
        u_vdem_country_year_country == "Bolivia" ~ "Bolivia (Plurinational State of)",
        u_vdem_country_year_country == "Burma/Myanmar" ~ "Myanmar",
        u_vdem_country_year_country == "Cape Verde" ~ "Cabo Verde",
        u_vdem_country_year_country == "Central African Republic" ~ "Central African Republic (the)",
        u_vdem_country_year_country == "Comoros" ~ "Comoros (the)",
        u_vdem_country_year_country == "Dominican Republic" ~ "Dominican Republic (the)",
        u_vdem_country_year_country == "Iran" ~ "Iran (Islamic Republic of)",
        u_vdem_country_year_country == "Laos" ~ "Lao People's Democratic Republic (the)",
        u_vdem_country_year_country == "Moldova" ~ "Moldova (the Republic of)",
        u_vdem_country_year_country == "Netherlands" ~ "Netherlands (the)",
        u_vdem_country_year_country == "Niger" ~ "Niger (the)",
        u_vdem_country_year_country == "Philippines" ~ "Philippines (the)",
        u_vdem_country_year_country == "Republic of the Congo" ~ "Congo (the)",
        u_vdem_country_year_country == "Sudan" ~ "Sudan (the)",
        u_vdem_country_year_country == "Tanzania" ~ "Tanzania, the United Republic of",
        u_vdem_country_year_country == "United Arab Emirates" ~ "United Arab Emirates (the)",
        u_vdem_country_year_country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
        u_vdem_country_year_country == "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
        TRUE ~ u_qog_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_qog_country_year_country", 
                                    "u_qog_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
  
  return(out)
}
  

#' @export
to_u_vdem_country_year.u_complab_country_year <- function(df, ...) {
  
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_complab_country_year")
  unit_table_end <- read_unit_table("u_vdem_country_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_complab_country_year_country = NA_character_,
           u_complab_country_year_year = u_vdem_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_complab_country_year_country", "u_vdem_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country =
        ifelse(u_vdem_country_year_country %in% common_names,
               u_vdem_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country = case_when(
        u_vdem_country_year_country == "United States of America" ~ "United States",
        u_vdem_country_year_country == "Czechia" ~ "Czech Republic",
        TRUE ~ u_complab_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_complab_country_year_country", 
                                    "u_complab_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
  
  return(out)
} 

#' @export
to_u_vdem_country_year.u_hdata_country_year <- function(df, ...) {
  
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_hdata_country_year")
  unit_table_end <- read_unit_table("u_vdem_country_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_country_year_country = NA_character_,
                  u_hdata_country_year_year = u_vdem_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_country_year_country", "u_vdem_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country =
        ifelse(u_vdem_country_year_country %in% common_names,
               u_vdem_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    mutate(
      u_hdata_country_year_country = case_when(
        u_vdem_country_year_country == "Uzbekistan" ~ "Bukhara/Uzbekistan" ,
        u_vdem_country_year_country == "Hesse-Darmstadt" ~ "Hesse Grand Ducal",
        u_vdem_country_year_country == "Hesse-Kassel" ~ "Hesse Electoral",
        u_vdem_country_year_country == "South Korea" ~ "Korea/South Korea",
        u_vdem_country_year_country == "Mecklenburg Schwerin" ~ "Meckelnburg Schwerin",
        u_vdem_country_year_country == "Burma/Myanmar" ~ "Myanmar",
        u_vdem_country_year_country == "Saudi Arabia" ~ "Nejd/Saudi Arabia",
        u_vdem_country_year_country == "Serbia" ~ "Serbia/Yugoslavia",
        u_vdem_country_year_country == "Libya" ~ "Tripolitania/Libya",
        u_vdem_country_year_country == "United States of America" ~ "United States",
        u_vdem_country_year_country == "Würtemberg" ~ "Wuerttemburg",
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
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
  
  return(out)
} 

#' @export
to_u_vdem_country_year.u_repdem_cabinet_date <- function(df) {
  
  #aggregate from cab date to country date to country year
  df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year() 
      
  #save old class
      old_ds_class <- class(df)[2]
      
      #duplicate unit cols
      df$u_vdem_country_year_country <- df$u_repdem_country_year_country
      df$u_vdem_country_year_year <- df$u_repdem_country_year_year
      
      #adjust country names
      df %<>% 
        dplyr::mutate(
          u_vdem_country_year_country = case_when(
            u_repdem_country_year_country == "Netherlands" ~ "the Netherlands",
            u_repdem_country_year_country == "Czechia" ~ "Czech Republic",
            TRUE ~ u_vdem_country_year_country
          ))
      
      #join end unit table
      unit_table_end <- read_unit_table("u_vdem_country_year")
     
      out <- dplyr::left_join(unit_table_end, 
                              df, 
                              by = c("u_vdem_country_year_country", 
                                     "u_vdem_country_year_year"))
      
      # Indicate merge missingness by replacing NAs in all rows without match with -11111
      # e contains all empty rows
      # Indicate missing values from translation
      out <- out %>%
        dplyr::mutate_if(lubridate::is.Date, as.character)
      
      e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
      # Assign -11111 to selected cells in a single operation
      out[e, !grepl("^u_", names(out))] <- -11111
      
      
      #set class
      class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
      
      stopifnot(nrow(unit_table_end) == nrow(out))
      
      return(out)
}


#' @export
to_u_vdem_country_year.u_vdem_party_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_party_country_year")
  df %<>% bind_cols(unit_table_start)
  
  df %<>% aggregate_vparty_highest_seatshare()
  
  unit_table_end <- read_unit_table("u_vdem_country_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_vdem_party_country_year_country_name = NA_character_,
                  u_vdem_party_country_year_year = u_vdem_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_vdem_party_country_year_country_name",
                                        "u_vdem_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_vdem_party_country_year_country_name =
        ifelse(u_vdem_country_year_country %in% common_names,
               u_vdem_country_year_country, NA))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end,df, 
                             by = c("u_vdem_party_country_year_country_name", 
                                    "u_vdem_party_country_year_year")) %>%
  distinct(u_vdem_country_year_country, u_vdem_country_year_year, .keep_all = TRUE)
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_vdem_country_year.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  unit_table_end <- read_unit_table("u_vdem_country_year")
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for V-Dem CoW to UCDP GW 
  df %<>% 
    mutate(
      u_ucdp_orgv_country_year_country_id_cy = case_when(
        u_ucdp_orgv_country_year_country_cy == "Germany" & u_ucdp_orgv_country_year_year_cy >= 1991 ~ 255,
        u_ucdp_orgv_country_year_country_cy == "Yemen (North Yemen)" ~ 679,
        TRUE ~ as.numeric(u_ucdp_orgv_country_year_country_id_cy)
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_vdem_country_year_cowcode" = "u_ucdp_orgv_country_year_country_id_cy",
                          "u_vdem_country_year_year" = "u_ucdp_orgv_country_year_year_cy"))
  
  out <- out %>% 
    dplyr::mutate(across(dplyr::starts_with("ucdp_orgv_"), ~ifelse(u_vdem_country_year_country_text_id == "PSG" & 
                                                      u_vdem_country_year_year >= 1989, -22222, .))) %>%
    dplyr::mutate(across(dplyr::starts_with("ucdp_orgv_"), ~ifelse(u_vdem_country_year_country_text_id == "PSE" & 
                                                      u_vdem_country_year_year >= 1989, -22222, .))) %>%
    dplyr::mutate(across(dplyr::starts_with("ucdp_orgv_"), ~ifelse(u_vdem_country_year_country_text_id == "XKX" & 
                                                      u_vdem_country_year_year >= 1999 & 
                                                        u_vdem_country_year_year <= 2007, -22222, .))) %>%
    dplyr::mutate(across(dplyr::starts_with("ucdp_orgv_"), ~ifelse(u_vdem_country_year_country_text_id == "ERI" & 
                                                        u_vdem_country_year_year >= 1989 & 
                                                        u_vdem_country_year_year <= 1993, -22222, .)))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_vdem_country_year.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.numeric(unit_table_start$u_ucdp_country_year_gwno_a)
  
  unit_table_end <- read_unit_table("u_vdem_country_year")
  
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for V-Dem CoW to UCDP GW 
  df %<>% 
    mutate(
      u_ucdp_country_year_gwno_a = case_when(
        u_ucdp_country_year_name == "German Federal Republic" & u_ucdp_country_year_year >= 1991 ~ 255,
        u_ucdp_country_year_name == "Yemen (North Yemen)" ~ 679,
        TRUE ~ as.numeric(u_ucdp_country_year_gwno_a)
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_vdem_country_year_cowcode" = "u_ucdp_country_year_gwno_a",
                          "u_vdem_country_year_year" = "u_ucdp_country_year_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_vdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}
