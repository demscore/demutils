#' Translation to_u_qog_country
#' 
#' Translates compatible dataset to primary unit u_qog_country.
#' 
#' @param df a data.frame
#'
#' @return data.frame translated to qog_country.
#' @examples 
#' to_u_qog_country(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_qog_country <- function(df, ...) UseMethod("to_u_qog_country")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_qog_country.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_qog_country!")
}

#' @export
to_u_qog_country.u_qog_country_year <- function(df) {
  
  #saving all variables before binding unit table (unit cols), will use in filter below
  all_vars <- colnames(df)
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #filter out obs where all original variables are NA (excluding unit cols)
  df %<>% filter(!if_all((all_of(all_vars)), is.na))
  
  #Keep only max (most recent) obs
  df <- df %>% 
    group_by(u_qog_country_year_country) %>%
    filter(u_qog_country_year_year == max(u_qog_country_year_year)) %>%
    ungroup(.)
  
  #duplicate country unit col
  df$u_qog_country_country <- df$u_qog_country_year_country
  
  #join end unit table
  unit_table_end <- read_unit_table("u_qog_country")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_qog_country_country"))
  
  # Indicate missing values resulting from translation
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
  class(out) <- c("u_qog_country", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}


#' @export
to_u_qog_country.u_qog_region_year <- function(df) {
  
  #saving all variables before binding unit table (unit cols), will use in filter below
  all_vars <- colnames(df)
  
  #save old class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_qog_region_year")
  df %<>% bind_cols(unit_table_start) %>% 
    filter(!if_all((all_vars), is.na)) %>% 
    filter(nchar(u_qog_region_year_region) == 2)
  
  #Keep only max (most recent) obs
  df <- df %>% 
    group_by(u_qog_region_year_region_name) %>%
    filter(u_qog_region_year_year == max(u_qog_region_year_year)) %>%
    ungroup(.)
  
  unit_table_end <- read_unit_table("u_qog_country")
  
  
  df %<>% mutate(u_qog_region_year_region_name = case_when(
    u_qog_region_year_region_name == "Netherlands" ~ "Netherlands (the)",
    u_qog_region_year_region_name == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
    u_qog_region_year_region_name == "Czech Republic" ~ "Czechia",
    TRUE ~ u_qog_region_year_region_name
  ))
  
  
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_qog_country_country" = "u_qog_region_year_region_name")) 
  
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111

# Drop all variables without a single match in the end output unit
    # Drop all variables without a single match in the end output unit
  drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
  if (length(drop_cols) > 0) {
    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
  } else {
    out <- out
  }
  
  #set class
  class(out) <- c("u_qog_country", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return (out)
}

