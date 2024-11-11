#' @export
clean_latex <- function(df, column_name) {
  df[[column_name]] <- gsub('\\\\', '', df[[column_name]])
  df[[column_name]] <- gsub('\\{\\\\underline', '', df[[column_name]])
  df[[column_name]] <- gsub('textit', '', df[[column_name]])
  df[[column_name]] <- gsub('textbf', '', df[[column_name]])
  df[[column_name]] <- gsub('\\{', '', df[[column_name]])
  df[[column_name]] <- gsub('\\}', '', df[[column_name]])
  return(df)
}
