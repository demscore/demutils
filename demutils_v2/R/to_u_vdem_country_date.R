#' Translation to vdem_country_date
#'
#' Translates compatible dataset to primary unit vdem_country_date
#' 
#' @param df a data.frame.
#' @return data.frame translated to vdem_country_date.
#' @examples 
#' to_u_vdem_country_date(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_vdem_country_date <- function(df, ...) UseMethod("to_u_vdem_country_date")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_vdem_country_date.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_vdem_country_date!")
}


#' @export
to_u_vdem_country_date.u_repdem_cabinet_date <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #aggregate from cab date to country date
  df <- repdem_cabinet_date_to_country_date(df)
  
  #drop newdate col and duplicate rows (unnecessary for this method)
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
  
  #duplicate unit cols
  df$u_vdem_country_date_country_name <- df$u_repdem_cabinet_date_country
  df$u_vdem_country_date_date <- df$u_repdem_cabinet_date_date
  
  #adjust country names
  df %<>% 
    dplyr::mutate(
      u_vdem_country_date_country_name = case_when(
        u_repdem_cabinet_date_country == "the Netherlands" ~ "Netherlands",
        u_repdem_cabinet_date_country == "Czech Republic" ~ "Czechia",
        TRUE ~ u_vdem_country_date_country_name
      ))

  #join unit table
  unit_table_end <- read_unit_table("u_vdem_country_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_vdem_country_date_country_name", "u_vdem_country_date_date"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_vdem_country_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_vdem_country_date.u_hdata_minister_date <- function(df) {
  
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
  # I am duplicating the data across all individual dates within this interval in order to then match
  # on other date units.
  M <- Map(seq, df$u_hdata_minister_date_date_in, df$u_hdata_minister_date_date_out, by = "day")
  all_days <- data.frame(
    u_hdata_minister_date_country = rep.int(df$u_hdata_minister_date_country, vapply(M, length, 1L)),
    u_hdata_minister_date_cowcode = rep.int(df$u_hdata_minister_date_cowcode, vapply(M, length, 1L)), 
    u_hdata_minister_date_minister = rep.int(df$u_hdata_minister_date_minister, vapply(M, length, 1L)),
    u_hdata_minister_date_date_in = rep.int(df$u_hdata_minister_date_date_in, vapply(M, length, 1L)),
    u_hdata_minister_date_date_out = rep.int(df$u_hdata_minister_date_date_out, vapply(M, length, 1L)),
    u_hdata_minister_date_date = do.call(c, M)
  )    
  
  df <- left_join(all_days, df, by = 
                    c("u_hdata_minister_date_country", "u_hdata_minister_date_minister", "u_hdata_minister_date_cowcode", "u_hdata_minister_date_date_in", "u_hdata_minister_date_date_out"))
  
  #duplicate unit cols
  df$u_vdem_country_date_country_name <- df$u_hdata_minister_date_country
  df$u_vdem_country_date_date <- df$u_hdata_minister_date_date
  
  #adjust country names
  df %<>% 
    mutate(
      u_vdem_country_date_country_name = case_when(
        u_hdata_minister_date_country == "Ottoman Empire/Turkey" ~ "Turkey",
        u_hdata_minister_date_country == "Prussia/Germany" ~ "Germany",
        u_hdata_minister_date_country == "Russia/USSR" ~ "Russia",
        u_hdata_minister_date_country == "United States" ~ "United States of America",
        TRUE ~ u_vdem_country_date_country_name
      ))
  
  #join end unit table
  unit_table_end <- read_unit_table("u_vdem_country_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_vdem_country_date_country_name", "u_vdem_country_date_date")) %>%
    # If we don't do distinct, we get one duplicate row for Austria AUT 305 1867-12-30
    distinct(u_vdem_country_date_country_name, u_vdem_country_date_date, .keep_all = TRUE)
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_vdem_country_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_vdem_country_date.u_vdem_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_country_year")
  unit_table_end <- read_unit_table("u_vdem_country_date")
  
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  
  unit_table_end$u_vdem_country_date_year <- 
    as.integer(format(unit_table_end$u_vdem_country_date_date, "%Y"))
  
  unit_table_end %<>%
    dplyr::mutate(u_vdem_country_year_country = NA_character_,
                  u_vdem_country_year_year = u_vdem_country_date_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_vdem_country_year_country", "u_vdem_country_date_country_name")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_vdem_country_year_country =
        ifelse(u_vdem_country_date_country_name %in% common_names,
               u_vdem_country_date_country_name, NA))
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df)
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_vdem_country_year_country", 
                                    "u_vdem_country_year_year"))%>%
    dplyr::select(-u_vdem_country_date_year)
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  class(out) <- c("u_vdem_country_date", old_ds_class, "data.frame")
  
  return(out)
}


#' @export
to_u_vdem_country_date.u_hdata_cabinet_date <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #aggregate from cab date to country date
  df <- hdata_cabinet_date_to_country_date(df)
  
  #drop newdate col and duplicate rows (unnecessary for this method)
  df <- dplyr::select(df, -newdate) %>% dplyr::distinct(.)
  
  # Because repdem cabinet date represents an interval during which a cabinet is in power,
  # I am duplicating the data across -all individual dates- within this interval in order to then match
  # on other date units.
  M <- Map(seq, df$u_hdata_cabinet_date_date_in, df$u_hdata_cabinet_date_date_out, by = "day")
  all_days <- data.frame(
    u_hdata_cabinet_date_country = rep.int(df$u_hdata_cabinet_date_country, vapply(M, length, 1L)), 
    u_hdata_cabinet_date_cab_name = rep.int(df$u_hdata_cabinet_date_cab_name, vapply(M, length, 1L)),
    u_hdata_cabinet_date_date = do.call(c, M)
  )    
  
  df <- left_join(all_days, df, by = c("u_hdata_cabinet_date_country", "u_hdata_cabinet_date_cab_name"))
  
  #duplicate unit cols
  df$u_vdem_country_date_country_name <- df$u_hdata_cabinet_date_country
  df$u_vdem_country_date_date <- df$u_hdata_cabinet_date_date
  
  #join unit table
  unit_table_end <- read_unit_table("u_vdem_country_date")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_vdem_country_date_country_name", "u_vdem_country_date_date"))
  
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #set class
  class(out) <- c("u_vdem_country_date", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}
