#' Reshape observation counts to long format
#'
#' Reshapes the obervation counts data.frame, that is to be loaded into the globa environment
#' to a data.frame with one row per variable and output unit. The observations per 
#' variable and output unit are stored in the obs column instead of in one column 
#' for each output unit.
#' 
#' @param df the observation count data.frame that is to be reshaped.
#'
#' @return A reshaped data.frame in long format.
#' 
#' @examples
#' obs_re <- reshape_obs(obs_counts) 
#' 
#' @export
reshape_obs <- function(df) {
  
  df <- df %>% rename(tag_long = variable) 
  
  obs_re <- vbase::wide_to_long(df, id_vars = c("tag_long","from_dataset", "project")) 
  
  obs_re %<>% rename(obs = value) %>%
    rename(unit = variable) %>%
    filter(!is.na(obs))
  
  return(obs_re)
}

#' Reshape observation counts to wide format
#'
#' Shapes the reshaped observation counts data frame back to wide format, but with one
#' column per variable and a unit column storing each output unit.
#' The fnction calls the reshape_obs function first
#' 
#' @param df the observation counts data.frame, obs_re, in long format.
#'
#' @return A reshaped data.frame in wide format.
#' 
#' @examples
#' obs_wide <- reshape_obs_wide(obs_counts) 
#' 
#' @export
reshape_obs_wide <- function (df) {
  
  df <- reshape_obs(df)
  
  df %<>% vbase::long_to_wide(id_vars = "unit", id_var = "tag_long", value_var = "obs")
  
  return(df)
  
}

#' Add project column
#'
#' Adds column indicating from which project ad dataset of variable stems if that
#' information is missing
#' 
#' @param df Data.frame with variable called tag_long.
#'
#' @return Data.frame with new colum called proj.
#' 
#' @examples
#' df <- add_proj(df) 
#' 
#' @export
add_proj <- function (df) {
  
  df <- df %>%
    mutate(project = case_when(
      grepl("^complab", dataset_tag) ~ "complab",
      grepl("^hdata", dataset_tag) ~ "hdata",
      grepl("^qog", dataset_tag) ~ "qog",
      grepl("^repdem", dataset_tag) ~ "repdem",
      grepl("^ucdp", dataset_tag) ~ "ucdp",
      grepl("^vdem", dataset_tag) ~ "vdem"
    )) 
  
  return(df)
}