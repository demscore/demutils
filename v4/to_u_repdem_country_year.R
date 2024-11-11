#' Translation to u_repdem_country_year
#'
#' Translates compatible dataset to primary unit repdem_country_year
#' 
#' @param df a data.frame.
#' @return data.frame translated to repdem_country_year.
#' @examples 
#' to_u_repdem_country_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_repdem_country_year <- function(df, ...) UseMethod("to_u_repdem_country_year")

#' @export
to_u_repdem_country_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_repdem_country_year!")
}

#' @export
to_u_repdem_country_year.u_repdem_cabinet_date <- function(df) {
  
  #aggregate from cab date to country date to country year
  df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year() 
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #join end unit table
  unit_table_end <- read_unit_table("u_repdem_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
  # Indicate missing values arising from translation. This requires changing class of date columns
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}

#' @export
to_u_repdem_country_year.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_repdem_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_repdem_country_year_year <- df$u_qog_country_year_year
  df$u_repdem_country_year_country<- df$u_qog_country_year_country
  
  #adjust country names
  df %<>% 
    dplyr::mutate(
      u_repdem_country_year_country = case_when(
        u_qog_country_year_country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
        u_qog_country_year_country == "United States of America (the)" ~ "United States",
        TRUE ~ u_repdem_country_year_country
      ))
  
  # Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_repdem_country_year.u_vdem_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_vdem_country_year")
  df %<>% bind_cols(unit_table_start)
  
  
  #duplicate unit columns
  df$u_repdem_country_year_year <- df$u_vdem_country_year_year
  df$u_repdem_country_year_country<- df$u_vdem_country_year_country
  
  
  unit_table_end <- read_unit_table("u_repdem_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_repdem_country_year.u_hdata_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_repdem_country_year_year <- df$u_hdata_country_year_year
  df$u_repdem_country_year_country<- df$u_hdata_country_year_country
  
  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_repdem_country_year.u_complab_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_complab_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  # Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  df$u_repdem_country_year_year <- df$u_complab_country_year_year
  df$u_repdem_country_year_country<- df$u_complab_country_year_country
  
  #join end unit table
  unit_table_end <- read_unit_table("u_repdem_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_repdem_country_year.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  df %<>% bind_cols(unit_table_start)
  
  
  #duplicate unit columns
  df$u_repdem_country_year_year <- df$u_ucdp_orgv_country_year_year_cy
  df$u_repdem_country_year_country <- df$u_ucdp_orgv_country_year_country_cy
  
  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_repdem_country_year.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.numeric(unit_table_start$u_ucdp_country_year_gwno_a)
  df %<>% bind_cols(unit_table_start)
  
  
  #duplicate unit columns
  df$u_repdem_country_year_year <- df$u_ucdp_country_year_year
  df$u_repdem_country_year_country<- df$u_ucdp_country_year_name
  
  
  #join end unit table. Matching country year to cabinet out-year, this can be changed, but seemed to make the most 
  # sense as it would be more likely to represent the actual performance of said cabinet.
  unit_table_end <- read_unit_table("u_repdem_country_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_repdem_country_year_country", "u_repdem_country_year_year"))
  
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
  class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_repdem_country_year.u_cses_respondent <- function(df) {
  
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
unit_table_end <- read_unit_table("u_repdem_country_year")

unit_table_end %<>% 
  dplyr::mutate(
    u_repdem_country_year_country = case_when(
      u_repdem_country_year_country == "United Kingdom" ~ "Great Britain",
      u_repdem_country_year_country == "Czechia" ~ "Czech Republic/Czechia",
      TRUE ~ u_repdem_country_year_country
    ))

out <- dplyr::left_join(unit_table_end, 
                        df, 
                        by = c("u_repdem_country_year_country" = "u_cses_respondent_country", 
                               "u_repdem_country_year_year" = "u_cses_respondent_year"))

# Indicate merge missingness by replacing NAs in all rows without match with -11111
# e contains all empty rows
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
class(out) <- c("u_repdem_country_year", old_ds_class, "data.frame")

stopifnot(nrow(unit_table_end) == nrow(out))

return(out)

}