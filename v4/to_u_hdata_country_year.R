#' Translation to u_hdata_country_year
#' 
#' Translates compatible dataset to primary unit hdata_country_year.
#' 
#' @param df a data.frame
#' @return data.frame translated to hdata_country_year.
#' @examples 
#' to_u_hdata_country_year(df)
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_hdata_country_year <- function(df, ...) UseMethod("to_u_hdata_country_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_hdata_country_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_hdata_country_year!")
}

#' @export
to_u_hdata_country_year.u_qog_country_year <- function(df, ...) {
  
  stopifnot(is.data.frame(df))
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_hdata_country_year")
  
  stopifnot(nrow(df) == nrow(unit_table_start))
  
  
  #Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_qog_country_year_country = NA_character_,
                  u_qog_country_year_year = u_hdata_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_qog_country_year_country", "u_hdata_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country =
        ifelse(u_hdata_country_year_country %in% common_names,
               u_hdata_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_qog_country_year_country = case_when(
        u_hdata_country_year_country == "Korea/South Korea" ~ "Korea (the Republic of)",
        u_hdata_country_year_country == "Nejd/Saudi Arabia" ~ "Saudi Arabia",
        u_hdata_country_year_country == "Tripolitania/Libya" ~ "Libya",
        u_hdata_country_year_country == "Bukhara/Uzbekistan" ~ "Uzbekistan",
        u_hdata_country_year_country == "Netherlands" ~ "Netherlands (the)",
        u_hdata_country_year_country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
        u_hdata_country_year_country == "United States" ~ "United States of America (the)",
        u_hdata_country_year_country == "Russia" &  
          u_hdata_country_year_year >= 1992 ~ "Russian Federation (the)",
        u_hdata_country_year_country == "Russia" &  
          u_hdata_country_year_year <= 1992 ~ "USSR",
        u_hdata_country_year_country == "Iran" ~ "Iran (Islamic Republic of)",
        u_hdata_country_year_country == "Dominican Republic" ~ "Dominican Republic (the)",
        u_hdata_country_year_country == "Bolivia" ~ "Bolivia (Plurinational State of)",
        u_hdata_country_year_country == "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
        u_hdata_country_year_country == "Serbia/Yugoslavia" & 
          u_hdata_country_year_year <= 1991 ~ "Yugoslavia",
        u_hdata_country_year_country == "Serbia/Yugoslavia" & 
          u_hdata_country_year_year >= 2006 ~ "Serbia",
        u_hdata_country_year_country == "Serbia/Yugoslavia" & 
          u_hdata_country_year_year >= 1992 & 
          u_hdata_country_year_year <= 2005 ~ "Serbia and Montenegro",
        TRUE ~ u_qog_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_qog_country_year_country", 
                                    "u_qog_country_year_year"))
  
  # Indicate missing values arising from translation. 
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_hdata_country_year.u_complab_country_year <- function(df, ...) {
  
  stopifnot(is.data.frame(df))
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_complab_country_year")
  unit_table_end <- read_unit_table("u_hdata_country_year")
  
  stopifnot(nrow(df) == nrow(unit_table_start))
  
  
  #Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_complab_country_year_country = NA_character_,
                  u_complab_country_year_year = u_hdata_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_complab_country_year_country", "u_hdata_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country =
        ifelse(u_hdata_country_year_country %in% common_names,
               u_hdata_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    dplyr::mutate(
      u_complab_country_year_country = case_when(
        u_hdata_country_year_country == "Korea/South Korea" ~ "South Korea",
        TRUE ~ u_complab_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_complab_country_year_country", 
                                    "u_complab_country_year_year"))
  
  # Indicate missing values arising from translation. 
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_hdata_country_year.u_vdem_country_year <- function(df, ...) {
  
  stopifnot(is.data.frame(df))
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_country_year")
  unit_table_end <- read_unit_table("u_hdata_country_year")
  
  stopifnot(nrow(df) == nrow(unit_table_start))
  
  
  #Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    mutate(u_vdem_country_year_country = NA_character_,
           u_vdem_country_year_year = u_hdata_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_vdem_country_year_country", "u_hdata_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    mutate(
      u_vdem_country_year_country =
        ifelse(u_hdata_country_year_country %in% common_names,
               u_hdata_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    mutate(
      u_vdem_country_year_country = case_when(
        u_hdata_country_year_country == "Bukhara/Uzbekistan" ~ "Uzbekistan",
        u_hdata_country_year_country == "Hesse Grand Ducal" ~ "Hesse-Darmstadt",
        u_hdata_country_year_country == "Hesse Electoral" ~ "Hesse-Kassel",
        u_hdata_country_year_country == "Korea/South Korea" ~ "South Korea",
        u_hdata_country_year_country == "Meckelnburg Schwerin" ~ "Mecklenburg Schwerin",
        u_hdata_country_year_country == "Myanmar" ~ "Burma/Myanmar",
        u_hdata_country_year_country == "Nejd/Saudi Arabia" ~ "Saudi Arabia",
        u_hdata_country_year_country == "Serbia/Yugoslavia" ~ "Serbia",
        u_hdata_country_year_country == "Tripolitania/Libya" ~ "Libya" ,
        u_hdata_country_year_country == "United States" ~ "United States of America",
        u_hdata_country_year_country == "Wuerttemburg" ~ "Würtemberg",
        u_hdata_country_year_country == "Turkey" ~ "Türkiye",
        # ...
        TRUE ~ u_vdem_country_year_country
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_vdem_country_year_country", 
                                    "u_vdem_country_year_year"))
  
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  return(out)
}

#' @export
to_u_hdata_country_year.u_hdata_minister_date <- function(df, ...) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_minister_date")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  country_date <- hdata_minister_date_to_country_date(df)
  country_year <- hdata_country_date_to_country_year_ministers(country_date)
  
  #drop unnecessary unit cols
  country_year %<>% select(
    -u_hdata_minister_date_minister,
    -u_hdata_minister_date_date_in,
    -u_hdata_minister_date_date_out,
    -u_hdata_minister_date_year_in,
    -u_hdata_minister_date_year_out,
    -day_count)
  

  #join end unit table
  unit_table_end <- read_unit_table("u_hdata_country_year")
  
  out <- left_join(unit_table_end, country_year, 
                   by = c("u_hdata_country_year_country", "u_hdata_country_year_year"))
  
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

  # Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}


#' @export
to_u_hdata_country_year.u_repdem_cabinet_date <- function(df) {
  
  #aggregate from cab date to country date to country year
  df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year() 
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #duplicate unit cols
  df$u_hdata_country_year_country <- df$u_repdem_country_year_country
  df$u_hdata_country_year_year <- df$u_repdem_country_year_year
  
  
  #join end unit table
  unit_table_end <- read_unit_table("u_hdata_country_year")
  out <- dplyr::left_join(unit_table_end, 
                          df, 
                          by = c("u_hdata_country_year_country", 
                                 "u_hdata_country_year_year"))
  
  # Indicate missing values arising from translation. This requires changing class of date columns
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}


#' @export
to_u_hdata_country_year.u_hdata_dyad_year <- function(df, ...) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_hdata_dyad_year")
  df %<>% bind_cols(unit_table_start)
  
  df %<>% hdata_dyad_year_to_country_year()
  
  unit_table_end <- read_unit_table("u_hdata_country_year")
  
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_hdata_dyad_year_country_one = NA_character_,
                  u_hdata_dyad_year_year = u_hdata_country_year_year)
  
  # Common names:
  common_names <- compare_country_names(unit_table_start, unit_table_end, 
                                        "u_hdata_dyad_year_country_one", "u_hdata_country_year_country")$common
  
  # Prefill where names are common
  unit_table_end %<>% 
    dplyr::mutate(
      u_hdata_dyad_year_country_one =
        ifelse(u_hdata_country_year_country %in% common_names,
               u_hdata_country_year_country, NA))
  
  # Manually adjust the rest:
  unit_table_end %<>% 
    mutate(
      u_hdata_dyad_year_country_one = case_when(
        u_hdata_dyad_year_country_one == "Uzbekistan" ~ "Bukhara/Uzbekistan" ,
        u_hdata_dyad_year_country_one == "Korea" ~ "Korea/South Korea",
        u_hdata_country_year_country == "Meckelnburg Schwerin" ~ "Mecklenburg Schwerin",
        u_hdata_dyad_year_country_one == "Burma" ~ "Myanmar",
        u_hdata_dyad_year_country_one == "Saudi Arabia" ~ "Nejd/Saudi Arabia",
        u_hdata_dyad_year_country_one == "Serbia" ~ "Serbia/Yugoslavia",
        u_hdata_dyad_year_country_one == "Libya" ~ "Tripolitania/Libya",
        u_hdata_dyad_year_country_one == "United States of America" ~ "United States",
        u_hdata_dyad_year_country_one == "Würtemberg" ~ "Wuerttemburg",
        TRUE ~ u_hdata_dyad_year_country_one
      ))
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  #Output data
  out <- dplyr::left_join(unit_table_end, df, 
                             by = c("u_hdata_dyad_year_country_one", 
                                    "u_hdata_dyad_year_year"))
  
  # Indicate missing values arising from translation.
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
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
to_u_hdata_country_year.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  unit_table_end <- read_unit_table("u_hdata_country_year")
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for H-DATA CoW to UCDP GW 
  df %<>% 
    mutate(
      u_ucdp_orgv_country_year_country_id_cy = case_when(
        u_ucdp_orgv_country_year_country_cy == "Germany" & u_ucdp_orgv_country_year_year_cy >= 1991 ~ 255,
        u_ucdp_orgv_country_year_country_cy == "Yemen (North Yemen)" ~ 679,
        TRUE ~ as.numeric(u_ucdp_orgv_country_year_country_id_cy)
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_hdata_country_year_cowcode" = "u_ucdp_orgv_country_year_country_id_cy",
                          "u_hdata_country_year_year" = "u_ucdp_orgv_country_year_year_cy"))
  
  # Indicate missing values arising from translation. 
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_hdata_country_year.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.numeric(unit_table_start$u_ucdp_country_year_gwno_a)
  unit_table_end <- read_unit_table("u_hdata_country_year")
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for H-DATA CoW to UCDP GW 
  df %<>% 
    mutate(
      u_ucdp_country_year_gwno_a = case_when(
        u_ucdp_country_year_name == "Germany" & u_ucdp_country_year_year >= 1991 ~ 255,
        u_ucdp_country_year_name == "Yemen (North Yemen)" ~ 679,
        TRUE ~ as.numeric(u_ucdp_country_year_gwno_a)
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_hdata_country_year_cowcode" = "u_ucdp_country_year_gwno_a",
                          "u_hdata_country_year_year" = "u_ucdp_country_year_year"))
  
  # Indicate missing values arising from translation.
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_hdata_country_year.u_hdata_cabinet_date <- function(df) {
  
  #aggregate from cab date to country date to country year
  df <- df %>% 
    hdata_cabinet_date_to_country_date() %>%
    hdata_country_date_to_country_year_cabinets() 
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #duplicate unit cols
  df$u_hdata_country_year_country <- df$u_hdata_country_year_country
  df$u_hdata_country_year_year <- as.integer(df$u_hdata_country_year_year)
  
  #join end unit table
  unit_table_end <- read_unit_table("u_hdata_country_year")
  out <- dplyr::left_join(unit_table_end, 
                          df, 
                          by = c("u_hdata_country_year_country", 
                                 "u_hdata_country_year_year"))
  
  # Indicate missing values arising from translation. This requires changing class of date columns
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- rowSums(is.na(out[, !grepl("^u_", names(out))])) == sum(!grepl("^u_", names(out)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}

#'@export
to_u_hdata_country_year.u_cses_respondent<- function(df) {
  
  #aggregate from cab date to country date to country year
  df %<>% 
    aggregate_cses(df)
  
  unit_table_start <- read_unit_table("u_cses_respondent")
  unit_table_start %<>% 
    select(-u_cses_respondent_id) %>%
    distinct(u_cses_respondent_country, 
             u_cses_respondent_year, 
             .keep_all = TRUE)
  
  df <- merge(df, unit_table_start, by = c("u_cses_respondent_country", 
                                           "u_cses_respondent_year"), 
              all.x = TRUE)
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #duplicate unit cols
  
  #join end unit table
  unit_table_end <- read_unit_table("u_hdata_country_year")
  
  out <- dplyr::left_join(unit_table_end, 
                          df, 
                          by = c("u_hdata_country_year_country" = "u_cses_respondent_country", 
                                 "u_hdata_country_year_year" = "u_cses_respondent_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  #Drop all variables without a single match in the end output unit
   drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
   if (length(drop_cols) > 0) {
     out <- out[, !names(out) %in% drop_cols, drop = FALSE]
   } else {
     out <- out
   }
  
  #set class
  class(out) <- c("u_hdata_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
  
}