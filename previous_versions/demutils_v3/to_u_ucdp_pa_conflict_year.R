#' Translation to u_ucdp_pa_year_confl
#'
#' Translates compatible dataset to primary unit ucdp_pa_conflict_year
#' 
#' @param df a data.frame.
#' @return data.frame translated to cucdp_pa_conflict_year
#' @examples 
#' to_u_ucdp_pa_conflict_year(df)
#' 
#' @section Note: Unit tables are not passed as function arguments, but are automatically loaded from the \code{ROOT} directory
#'
#' @export
to_u_ucdp_pa_conflict_year <- function(df, ...) UseMethod("to_u_ucdp_pa_conflict_year")
# This function can find the first unit table?
# This function can find the second unit table?

#' @export
to_u_ucdp_pa_conflict_year.default <- function(df, ...) {
  stop("We do not have a method for data from this input unit to u_ucdp_pa_conflict_year!")
}

#' @export
to_u_ucdp_pa_conflict_year.u_ucdp_conflict_year <- function(df) {
  
  #save old class
  old_ds_class <- class(df)[2]
  
  unit_table_start <- read_unit_table("u_ucdp_conflict_year")
  unit_table_end <- read_unit_table("u_ucdp_pa_conflict_year")
  
  
  # Transform unit tables
  unit_end_original <- unit_table_end
  unit_table_end %<>%
    dplyr::mutate(u_ucdp_conflict_year_conflict_id = as.integer(u_ucdp_pa_conflict_year_conflict_id),
                  u_ucdp_conflict_year_year = as.integer(u_ucdp_pa_conflict_year_year))
  
  stopifnot(nrow(unit_end_original) == nrow(unit_table_end))
  
  int_df <- bind_cols(unit_table_start, df) 
  
  
  # Output data
  out <- dplyr::left_join(unit_table_end, int_df,
                             by = c("u_ucdp_conflict_year_conflict_id", 
                                    "u_ucdp_conflict_year_year"))
  
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
  
  class(out) <- c("u_ucdp_pa_conflict_year", old_ds_class, "data.frame")
  
  return(out)
}