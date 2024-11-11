#' Translation to_u_qog_country_year
#' 
#' Translates compatible dataset to primary unit vdem_country_date_coder.
#' 
#' @param df a data.frame
#'
#' @return data.frame translated to vdem_country_date_coder.
#' @examples 
#' to_u_vdem_country_date_coder(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_vdem_country_date_coder <- function(df, ...) UseMethod("to_u_vdem_country_date_coder")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_vdem_country_date_coder.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_vdem_country_date_coder!")
}

#' @export
to_u_vdem_country_date_coder.u_vdem_country_date <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_vdem_country_date")
  unit_table_end <- read_unit_table("u_vdem_country_date_coder")
  
  unit_end_original <- unit_table_end
  
  unit_table_end %<>%
    dplyr::mutate(u_vdem_country_date_country_text_id = u_vdem_country_date_coder_country_text_id,
                  u_vdem_country_date_date = u_vdem_country_date_coder_historical_date)
  
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  # Output data
  out <- dplyr::left_join(unit_table_end, 
                             dplyr::bind_cols(unit_table_start, df), 
                             by = c("u_vdem_country_date_country_text_id", 
                                    "u_vdem_country_date_date"))
  # Indicate missing values from translation
  out <- out %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)
  
#  e <- apply(out[, !grepl("^u_", names(out))], 1, function(r) all(is.na(r)))
#  out[e, !grepl("^u_", names(out))] <- -11111

# commented out to incresase speed and bc we knwo all variables have a march
# Drop all variables without a single match in the end output unit
#  drop_cols <- names(out)[sapply(out, function(col) all(col == -11111|col == "-11111"))]
#  if (length(drop_cols) > 0) {
#    out <- out[, !names(out) %in% drop_cols, drop = FALSE]
#  } else {
#    out <- out
#  }
  
  class(out) <- c("u_vdem_country_date", old_ds_class, "data.frame")
  
  return(out)
}