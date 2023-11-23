#' Translation to u_complab_country_year_track
#'
#' Translates compatible dataset to primary unit complab_country_year.
#' 
#' @param df a data.frame.
#' @return data.frame translated to complab_country_year.
#' @examples 
#' to_u_complab_country_year_track(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_complab_country_year_track <- function(df, ...) UseMethod("to_u_complab_country_year_track")

#' @export
to_u_complab_country_year_track.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_complab_country_year_track!")
}

#' @export
to_u_complab_country_year_track.u_complab_country_year <- function(df, ...) {
  
  #save class
  old_ds_class <- class(df)[2]
  
  #bind unit table
  unit_table_start <- read_unit_table("u_complab_country_year")
  df %<>% bind_cols(unit_table_start)
  
  #duplicate unit cols
  df$u_complab_country_year_track_country <- df$u_complab_country_year_country
  df$u_complab_country_year_track_year <- df$u_complab_country_year_year
  
  #join end unit table
  unit_table_end <- read_unit_table("u_complab_country_year_track")
  
  out <- left_join(unit_table_end, df, 
                   by = c("u_complab_country_year_track_country", "u_complab_country_year_track_year"))
  
  # Indicate merge missingness by replacing NAs in all rows without match with -11111
  # e contains all empty rows
  # Indicate missing values resulting from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
  out[e, !grepl("^u_", names(out))] <- -11111
  
  
  #set class
  class(out) <- c("u_complab_country_year_track", old_ds_class, "data.frame")
  
  stopifnot(nrow(unit_table_end) == nrow(out))
  
  return(out)
}