#' Translation to_u_qog_country_year
#' 
#' Translates compatible dataset to primary unit qog_country_year.
#' 
#' @param df a data.frame
#'
#' @return data.frame translated to qog_country_year.
#' @examples 
#' to_u_qog_country_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_country_year <- function(df, ...) UseMethod("to_u_qog_country_year")
	# This function can find the first unit table?
	# This function can find the second unit table?

#' @export
to_u_qog_country_year.default <- function(df, ...) {
	stop("We do not have a method for data from this input unit to u_qog_country_year!")
}

#' @export
to_u_qog_country_year.u_qog_country <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country")
  df %<>% bind_cols(unit_table_start)
  
  #call end unit table
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  #create unit cols
  df$u_qog_country_year_country <- df$u_qog_country_country
  
  # unit col to match to max year only
  df$u_qog_country_year_year <- max(unit_table_end$u_qog_country_year_year)
  
  #join end unit table
  out <- dplyr::left_join(unit_table_end, df, 
                          by = c("u_qog_country_year_country", "u_qog_country_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_qog_country_year.u_vdem_country_year <- function(df, ...) {

  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_country_year")
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_vdem_country_year_country = NA_character_,
                  u_vdem_country_year_year = u_qog_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_vdem_country_year_country", "u_qog_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_vdem_country_year_country =
        ifelse(u_qog_country_year_country %in% common_names,
               u_qog_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_vdem_country_year_country = case_when(
        u_qog_country_year_country == "Bolivia (Plurinational State of)" ~ "Bolivia",
        u_qog_country_year_country == "Cabo Verde" ~ "Cap Verde",
        u_qog_country_year_country == "Central African Republic (the)" ~ "Central African Republic",
        u_qog_country_year_country == "Comoros (the)" ~ "Comoros",
        u_qog_country_year_country == "Dominican Republic (the)" ~ "Dominican Republic",
        u_qog_country_year_country == "Iran (Islamic Republic of)" ~ "Iran",
        u_qog_country_year_country == "Lao People's Democratic Republic (the)" ~ "Laos",
        u_qog_country_year_country == "Moldova (the Republic of)" ~ "Moldova",
        u_qog_country_year_country == "Niger (the)" ~ "Niger",
        u_qog_country_year_country == "Philippines (the)" ~ "Philippines",
        u_qog_country_year_country == "Myanmar" ~ "Burma/Myanmar",
        u_qog_country_year_country == "Gambia (the)" ~ "The Gambia",
        u_qog_country_year_country == "Sudan (the)" ~ "Sudan",
        u_qog_country_year_country == "Syrian Arab Republic (the)" ~ "Syria",
        u_qog_country_year_country == "Taiwan (Province of China)" ~ "Taiwan",
        u_qog_country_year_country == "Tanzania, the United Republic of " ~ "Tanzania",
        u_qog_country_year_country == "United Arab Emirates (the)" ~ "United Arab Emirates",
        u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
        u_qog_country_year_country == "Venezuela (the Bolivarian Republic of)" ~ "Venezuela",
        u_qog_country_year_country == "Yemen Democratic" ~ "South Yemen",
        u_qog_country_year_country == "USSR" ~ "Russia",
        u_qog_country_year_country == "Russian Federation (the)" ~ "Russia",
        u_qog_country_year_country == "Viet Nam" ~ "Vietnam",
        u_qog_country_year_country == "Vietnam, North" ~ "Vietnam",
        u_qog_country_year_country == "Vietnam, South" ~ "Republic of Vietnam",
        u_qog_country_year_country == "United States of America (the)" ~ "United States of America",
        u_qog_country_year_country == "Congo (the Democratic Republic of the)" ~ "Democratic Republic of the Congo",
        u_qog_country_year_country == "Korea (the Democratic People's Republic of)" ~ "North Korea",
        u_qog_country_year_country == "Korea (the Republic of)" ~ "South Korea",
        u_qog_country_year_country == "Cote d'Ivoire" ~ "Ivory Coast",
        u_qog_country_year_country == "Netherlands (the)" ~ "Netherlands",
        u_qog_country_year_country == "Congo (the)" ~ "Republic of the Congo",
        u_qog_country_year_country == "Czechoslovakia" ~ "Czechia",
        TRUE ~ u_vdem_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_vdem_country_year_country", 
                                    "u_vdem_country_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  return(out)
}



#' @export
to_u_qog_country_year.u_complab_country_year <- function(df, ...) {

  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_complab_country_year")
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_complab_country_year_country = NA_character_,
                  u_complab_country_year_year = u_qog_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_complab_country_year_country", "u_qog_country_year_country")$common
  
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country =
        ifelse(u_qog_country_year_country %in% common_names,
               u_qog_country_year_country, NA))
  
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country = case_when(
        u_qog_country_year_country == "Korea (the Republic of)" ~ "South Korea",
        u_qog_country_year_country == "Netherlands (the)" ~ "Netherlands",
        u_qog_country_year_country == "United States of America (the)" ~ "United States of America",
        u_qog_country_year_country == "Czechia" ~ "Czech Republic",
        u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
        # ...
        TRUE ~ u_complab_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_complab_country_year_country", 
                                    "u_complab_country_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  return(out)
  
 }

#' @export
to_u_qog_country_year.u_hdata_country_year <- function(df, ...) {
  
  stopifnot(is.data.frame(df))
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_hdata_country_year")
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_country_year_country = NA_character_,
                  u_hdata_country_year_year = u_qog_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_country_year_country", "u_qog_country_year_country")$common
  
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country =
        ifelse(u_qog_country_year_country %in% common_names,
               u_qog_country_year_country, NA))
  
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_country_year_country = case_when(	
        u_qog_country_year_country == "Korea (the Republic of)" ~ "Korea/South Korea",
        u_qog_country_year_country == "Saudi Arabia" ~ "Nejd/Saudi Arabia",
        u_qog_country_year_country == "Libya" ~ "Tripolitania/Libya",	
        u_qog_country_year_country == "Uzbekistan" ~ "Bukhara/Uzbekistan",
        u_qog_country_year_country == "Yugoslavia" ~ "Serbia/Yugoslavia",
        u_qog_country_year_country == "Serbia and Montenegro" ~ "Serbia/Yugoslavia",	
        u_qog_country_year_country == "Serbia" ~ "Serbia/Yugoslavia",
        u_qog_country_year_country == "Netherlands (the)" ~ "Netherlands",
        u_qog_country_year_country == "Viet Nam" ~ "Vietnam",
        u_qog_country_year_country == "Vietnam, North" ~ "Vietnam",
        u_qog_country_year_country == "Vietnam, South" ~ "Vietnam",
        u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
        u_qog_country_year_country == "United States of America (the)" ~ "United States",
        u_qog_country_year_country == "Russian Federation (the)" ~ "Russia",
        u_qog_country_year_country == "Iran (Islamic Republic of)" ~ "Iran",
        u_qog_country_year_country == "Dominican Republic (the)" ~ "Dominican Republic",
        u_qog_country_year_country == "Bolivia (Plurinational State of)" ~ "Bolivia",
        u_qog_country_year_country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
        # ...
        TRUE ~ u_hdata_country_year_country
      ))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_hdata_country_year_country", 
                                    "u_hdata_country_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  return(out)
  
}


#' @export
to_u_qog_country_year.u_repdem_cabinet_date <- function(df) {
 
    #aggregate from cabinet date to country date and to country year
   df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year()
    
   #save old class
    old_ds_class <- class(df)[2]
    
    #duplicate unit cols
    df$u_qog_country_year_country <- df$u_repdem_country_year_country
    df$u_qog_country_year_year <- df$u_repdem_country_year_year
    
    #adjust country names
    df %<>% 
      dplyr::mutate(
        u_qog_country_year_country = case_when(
          u_qog_country_year_country == "Netherlands (the)" ~ "the Netherlands",
          u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
          u_qog_country_year_country == "United States of America (the)" ~ "United States",
          TRUE ~ u_qog_country_year_country
        ))
    
    #join end unit table
    unit_table_end <- read_unit_table("u_qog_country_year")

    out <- dplyr::left_join(unit_table_end, 
                            df, 
                            by = c("u_qog_country_year_country", 
                                   "u_qog_country_year_year"))
    
    # Indicate missing values resulting from translation
    out <- out %>%
      dplyr::mutate_if(lubridate::is.Date, as.character)
    
    e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
    # Assign -11111 to selected cells in a single operation
    out[e, !grepl("^u_", names(out))] <- -11111
    
    #set class
    class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
    
    stopifnot(nrow(unit_table_end) == nrow(out))
    
    return(out)
}

#' @export
to_u_qog_country_year.u_qog_region_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_region_year")
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_region_year_region_name = NA_character_,
                  u_qog_region_year_year = u_qog_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_region_year_region_name", "u_qog_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_year_region_name =
        ifelse(u_qog_country_year_country %in% common_names,
               u_qog_country_year_country, NA))
  
  # Adjust manually where names differ
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_region_year_region_name = case_when(
        u_qog_country_year_country == "Czechia" ~ "Czech Republic",
        u_qog_country_year_country == "Netherlands (the)" ~ "Netherlands",
        TRUE ~ u_qog_region_year_region_name
      ))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_qog_region_year_region_name", 
                                    "u_qog_region_year_year")) %>%
    distinct(u_qog_country_year_country, u_qog_country_year_year, .keep_all = TRUE)
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#'@export
to_u_qog_country_year.u_qog_agency_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_agency_year")
  df %<>% bind_cols(unit_table_start)
  
  # aggregate u_qog_agency_year to year level
  
  df %<>% agency_bud_to_year_sum() %>%
    mutate(u_country = "Sweden")
  
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  # Output data
  out <- dplyr::left_join(unit_table_end, df,
                             by = c("u_qog_country_year_year" = "u_qog_agency_year_agency_fy",
                                    "u_qog_country_year_country" = "u_country"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#'@export
to_u_qog_country_year.u_qog_agency_inst <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_agency_inst")
  df %<>% bind_cols(unit_table_start)
  
  # aggregate agency_inst to year level
  
  df %<>% agency_inst_to_year()%>%
    mutate(u_country = "Sweden")
  
  unit_table_end <- read_unit_table("u_qog_country_year")
  
  # Output data
  out <- dplyr::left_join(unit_table_end, df,
                             by = c("u_qog_country_year_year", 
                                    "u_qog_country_year_country" = "u_country"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#'@export
to_u_qog_country_year.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  unit_table_end <- read_unit_table("u_qog_country_year")
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for V-Dem CoW to UCDP GW 
  df %<>% 
    mutate(
      u_ucdp_orgv_country_year_country_id_cy = case_when(
        u_ucdp_orgv_country_year_country_cy == "Germany" & u_ucdp_orgv_country_year_year_cy >= 1991 ~ 255L,
        u_ucdp_orgv_country_year_country_cy == "Yemen (North Yemen)" ~ 679L,
        TRUE ~ u_ucdp_orgv_country_year_country_id_cy
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_qog_country_year_ccodecow" = "u_ucdp_orgv_country_year_country_id_cy",
                          "u_qog_country_year_year" = "u_ucdp_orgv_country_year_year_cy"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#'@export
to_u_qog_country_year.u_ucdp_country_year<- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.numeric(unit_table_start$u_ucdp_country_year_gwno_a)
  unit_table_end <- read_unit_table("u_qog_country_year")
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for V-Dem CoW to UCDP GW 
  df %<>% 
    mutate(
      u_ucdp_country_year_gwno_a = case_when(
        u_ucdp_country_year_name == "Germany" & u_ucdp_country_year_year >= 1991 ~ 255,
        u_ucdp_country_year_name == "Yemen (North Yemen)" ~ 679,
        TRUE ~ as.numeric(u_ucdp_country_year_gwno_a)
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_qog_country_year_ccodecow" = "u_ucdp_country_year_gwno_a",
                          "u_qog_country_year_year" = "u_ucdp_country_year_year"))
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  # Assign -11111 to selected cells in a single operation
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_qog_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}