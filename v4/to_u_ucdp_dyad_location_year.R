#' Translation to u_ucdp_dyad_location_year
#'
#' Translates compatible dataset to primary unit ucdp_dyad_location_year
#' 
#' @param df a data.frame.
#' @return data.frame translated to u_ucdp_dyad_location_year
#' @examples 
#' to_u_ucdp_dyad_location_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_ucdp_dyad_location_year <- function(df, ...) UseMethod("to_u_ucdp_dyad_location_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_ucdp_dyad_location_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_ucdp_dyad_location_year!")
}

#' @export
to_u_ucdp_dyad_location_year.u_vdem_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_vdem_country_year")
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")

  unit_table_end$u_ucdp_dyad_location_year_gwno_loc <- 
    as.integer(unit_table_end$u_ucdp_dyad_location_year_gwno_loc)
  
  df %<>% bind_cols(unit_table_start)
  
  # Change codes for V-Dem CoW to UCDP GW 
  df %<>% 
    mutate(
      u_vdem_country_year_cowcode = case_when(
        u_vdem_country_year_country == "Germany" & u_vdem_country_year_year >= 1991 ~ 260L ,
        u_vdem_country_year_country == "Yemen" & u_vdem_country_year_year >= 1990 ~ 678L ,
        TRUE ~ u_vdem_country_year_cowcode
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_vdem_country_year_year", 
                          "u_ucdp_dyad_location_year_gwno_loc" = "u_vdem_country_year_cowcode"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_ucdp_dyad_location_year.u_hdata_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_hdata_country_year")
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")

  unit_table_end$u_ucdp_dyad_location_year_gwno_loc <- 
    as.integer(unit_table_end$u_ucdp_dyad_location_year_gwno_loc)

  df %<>% bind_cols(unit_table_start)
  
  # Change codes for H-DATA CoW to UCDP GW 
  df %<>% 
    mutate(
      u_hdata_country_year_cowcode = case_when(
        u_hdata_country_year_country == "Germany" & u_hdata_country_year_year >= 1991 ~ 260,
        u_hdata_country_year_country == "Yemen" & u_hdata_country_year_year >= 1990 ~ 678,
        TRUE ~ u_hdata_country_year_cowcode
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_hdata_country_year_year", 
                          "u_ucdp_dyad_location_year_gwno_loc" = "u_hdata_country_year_cowcode"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_ucdp_dyad_location_year.u_qog_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country_year")
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")

  unit_table_end$u_ucdp_dyad_location_year_gwno_loc <- 
    as.integer(unit_table_end$u_ucdp_dyad_location_year_gwno_loc)

  df %<>% bind_cols(unit_table_start)
  
  # Change codes for QoG CoW to UCDP GW 
  df %<>% 
    mutate(
      u_qog_country_year_ccodecow = case_when(
        u_qog_country_year_country == "Germany" & u_qog_country_year_year >= 1991 ~ 260L ,
        u_qog_country_year_country == "Yemen" & u_qog_country_year_year >= 1990 ~ 678L ,
        TRUE ~ u_qog_country_year_ccodecow
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_qog_country_year_year", 
                          "u_ucdp_dyad_location_year_gwno_loc" = "u_qog_country_year_ccodecow"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_ucdp_dyad_location_year.u_repdem_cabinet_date <- function(df) {
  
  df <- df %>% 
    repdem_cabinet_date_to_country_date() %>%
    repdem_country_date_to_country_year() 
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_repdem_country_year_year", 
                          "u_ucdp_dyad_location_year_location" = "u_repdem_country_year_country"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_ucdp_dyad_location_year.u_complab_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_complab_country_year")
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")
  df %<>% bind_cols(unit_table_start)
  
  df %<>% 
    mutate(
      u_complab_country_year_country = case_when(
        u_complab_country_year_country ==	"Bosnia & Herzegovina" ~	"Bosnia-Herzegovina",
        u_complab_country_year_country ==	"Cambodia" ~	"Cambodia (Kampuchea)",
        u_complab_country_year_country ==	"Congo - Brazzaville"	~	"Congo",
        u_complab_country_year_country ==	"Congo - Kinshasa "	~	"DR Congo (Zaire)",
        u_complab_country_year_country ==	"Czechia"	& 
          u_complab_country_year_year <= 1992 ~	"Czechoslovakia",
        u_complab_country_year_country ==	"Czechia"	& 
          u_complab_country_year_year >= 1993 ~	"Czech Republic",
        u_complab_country_year_country ==	"Eswatini" ~	"Kingdom of eSwatini (Swaziland)",
        u_complab_country_year_country ==	"Madagascar" ~	"Madagascar (Malagasy)",
        u_complab_country_year_country ==	"Russia" ~	"Russia (Soviet Union)",
        u_complab_country_year_country ==	"Serbia" ~	"Serbia (Yugoslavia)",
        u_complab_country_year_country ==	"St. Kitts & Nevis"	~	"Saint Kitts and Nevis",
        u_complab_country_year_country ==	"St. Lucia"	~	"Saint Lucia",
        u_complab_country_year_country ==	"St. Vincent & Grenadines"	~	"Saint Vincent and the Grenadines",
        u_complab_country_year_country ==	"Trinidad & Tobago"	~	"Trinidad and Tobago",
        u_complab_country_year_country ==	"United States"	~	"United States of America",
        u_complab_country_year_country ==	"Vietnam"	~	"Vietnam (North Vietnam)",
        u_complab_country_year_country ==	"Yemen"	~	"Yemen (North Yemen)",
        u_complab_country_year_country ==	"Zimbabwe" ~	"Zimbabwe (Rhodesia)",
        TRUE ~ u_complab_country_year_country
      ))
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_complab_country_year_year", 
                          "u_ucdp_dyad_location_year_location" = "u_complab_country_year_country"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_ucdp_dyad_location_year.u_ucdp_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_country_year")
  unit_table_start$u_ucdp_country_year_gwno_a <- 
    as.integer(unit_table_start$u_ucdp_country_year_gwno_a)
  
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")
  unit_table_end$u_ucdp_dyad_location_year_gwno_loc <- 
    as.integer(unit_table_end$u_ucdp_dyad_location_year_gwno_loc)
  df %<>% bind_cols(unit_table_start)
  
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_ucdp_country_year_year", 
                          "u_ucdp_dyad_location_year_gwno_loc" = "u_ucdp_country_year_gwno_a"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

#' @export
to_u_ucdp_dyad_location_year.u_ucdp_orgv_country_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_orgv_country_year")
  unit_table_start$u_ucdp_orgv_country_year_country_id_cy <- 
    as.integer(unit_table_start$u_ucdp_orgv_country_year_country_id_cy)
  
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")
  unit_table_end$u_ucdp_dyad_location_year_gwno_loc <- 
    as.integer(unit_table_end$u_ucdp_dyad_location_year_gwno_loc)
  df %<>% bind_cols(unit_table_start)
  
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_ucdp_orgv_country_year_year_cy", 
                          "u_ucdp_dyad_location_year_gwno_loc" = "u_ucdp_orgv_country_year_country_id_cy"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)

}


  #' @export
to_u_ucdp_dyad_location_year.u_ucdp_dyad_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_ucdp_dyad_year")
  unit_table_start$u_ucdp_dyad_year_dyad_id <- 
    as.character(unit_table_start$u_ucdp_dyad_year_dyad_id)
  
  unit_table_end <- read_unit_table("u_ucdp_dyad_location_year")
  unit_table_end$u_ucdp_dyad_location_year_gwno_loc <- 
    as.integer(unit_table_end$u_ucdp_dyad_location_year_gwno_loc)
  df %<>% bind_cols(unit_table_start)
  
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_ucdp_dyad_location_year_year" = "u_ucdp_dyad_year_year", 
                          "u_ucdp_dyad_location_year_dyad_id" = "u_ucdp_dyad_year_dyad_id"))
  
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
  class(out) <- c("u_ucdp_dyad_location_year", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)

}