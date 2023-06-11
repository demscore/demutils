#' ucdp_gedid to country_year
#' 
#' Aggregates from ucdp_gedid to country_year by using the fatality variables 
#' in the ucdp_ged dataset. The fatalities are aggregated by country, year and type of violence (onesided, nonstate, statebased), which are mutually exclusive. 
#' Variables not counting/estimating fatalities are excluded from the aggregation. Those variabes are:
#' are "relid","code_status","type_of_violence","conflict_dset_id",
#' "conflict_new_id","conflict_name","dyad_dset_id","dyad_new_id","dyad_name",
#' "side_a_dset_id","side_a_new_id", "side_a","side_b_dset_id", "side_b_new_id","side_b",
#' "number_of_sources","source_article", "source_office", "source_date", "source_headline"
#' ,"source_original", "where_prec","where_coordinates", "where_description", "adm_1",
#' "adm_2","latitude", "longitude", "geom_wkt", "priogrid_gid", "country", "country_id",
#' "region", "event_clarity","date_prec", "date_start","date_end", "gwnoa", "gwnob"                  
#' 
#' @param df a data.frame
#' @return data.frame aggregated to ucdp_country_year for fatality variables 
#' for each type of violence. 
#' @examples 
#' u_ucdp_gedid_to_country_year(df)
#'
#' @export
u_ucdp_gedid_to_country_year <- function(df) {
  
  # Set active year column to 1 whenever a dyad-year combination in a country 
  # in a year is 1
  ucdp_ged_active <- df %>% 
    group_by(u_ucdp_gedid_country, u_ucdp_gedid_year) %>%
    mutate(ucdp_ged_active_year = as.integer(any(ucdp_ged_active_year > 0L))) %>%
    select(u_ucdp_gedid_country, u_ucdp_gedid_year, ucdp_ged_active_year) %>%
    ungroup()

  # Reshape dataset with new death count variables by types of violence: state-based, 
  # non-state and one-sided (mutually exclusive categories), 
  # to create one observation per country and year that indicates
  # the number of deaths for each type of violence.
  ucdp_ged_deaths <- df %>% 
    mutate(ucdp_ged_type_of_violence = case_when(
      ucdp_ged_type_of_violence == 1 ~ "_state_based",
      ucdp_ged_type_of_violence == 2 ~ "_non_state",
      ucdp_ged_type_of_violence == 3 ~ "_one_sided"
    )) %>%
    group_by(ucdp_ged_type_of_violence, u_ucdp_gedid_country, u_ucdp_gedid_year) %>%
    summarize(ucdp_ged_deaths_a = sum(ucdp_ged_deaths_a, na.rm = TRUE),
              ucdp_ged_deaths_b = sum(ucdp_ged_deaths_b, na.rm = TRUE),
              ucdp_ged_deaths_civilians = sum(ucdp_ged_deaths_civilians, na.rm = TRUE),
              ucdp_ged_deaths_unknown = sum(ucdp_ged_deaths_unknown, na.rm = TRUE),
              ucdp_ged_high = sum(ucdp_ged_high, na.rm = TRUE),
              ucdp_ged_best = sum(ucdp_ged_best, na.rm = TRUE),
              ucdp_ged_low = sum(ucdp_ged_low, na.rm = TRUE), .groups = "keep") %>% 
    ungroup() 
  df_re <- ucdp_ged_deaths %>%
    wide_to_long(id_vars = c("u_ucdp_gedid_country", "u_ucdp_gedid_year", "ucdp_ged_type_of_violence")) %>%
    mutate(variable = paste0(variable, ucdp_ged_type_of_violence)) %>%
    select(-ucdp_ged_type_of_violence, ) %>%
    long_to_wide(id_vars = c("u_ucdp_gedid_country", "u_ucdp_gedid_year"),
                 id_var = "variable",
                 value_var = "value")
  

  #join unit columns
  df <- full_join(df_re, ucdp_ged_active, by = c("u_ucdp_gedid_country", 
                                                 "u_ucdp_gedid_year"))

  # Join in active year variable
  df <- full_join(df_re, ucdp_ged_active, by = c("u_ucdp_gedid_country", 
                                                 "u_ucdp_gedid_year"))
  # Deselect unnecessary columns
  df %<>% 
    distinct(u_ucdp_gedid_country, u_ucdp_gedid_year, .keep_all = TRUE) %>%
    rename(ucdp_ged_active_year_grouped = ucdp_ged_active_year) 
    # %>%
    #select(-ucdp_ged_bestNA, -ucdp_ged_highNA, -ucdp_ged_lowNA, -ucdp_ged_deaths_aNA,
    #       -ucdp_ged_deaths_bNA, -ucdp_ged_deaths_civiliansNA, -ucdp_ged_deaths_unknownNA)
  
  return (df)
}