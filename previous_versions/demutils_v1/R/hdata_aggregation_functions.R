#' Aggregation hdata_minister_date_to_country_date
#' 
#' Aggregates from hdata_minister_date to hdata_country_date
#' 
#' @param df a data.frame
#' @return data.frame aggregated to hdata_country_date.
#' @examples 
#' hdata_minister_date_to_country_date(df)
#'
#' @export
hdata_minister_date_to_country_date <- function(df) {
  
  # Select the highest out date if several ministers in a country came into office on one day
  # to keep only the observation for the minister who has been in office the highest number
  # of days
  
  get_longest_period <- function(df) {
    
    df <- df %>%
      dplyr::group_by(u_hdata_minister_date_country, u_hdata_minister_date_cowcode, u_hdata_minister_date_date_in) %>%
      dplyr::top_n(1, u_hdata_minister_date_date_out) %>%
      dplyr::ungroup()
    
    return(df)
  }
  
  hdata_overlap_adjust <- function(df){
    
    # ignore rows with same in and out date
    
    #df <- df %>% filter(u_hdata_minister_date_date_out != u_hdata_minister_date_date_in)
    
    df <- df %>% dplyr::group_by(u_hdata_minister_date_country) %>% 
      dplyr::arrange(u_hdata_minister_date_date_in, .by_group = TRUE)
    
    #if there is an overlap, subtract 1 day from date_out
    df %<>%
      dplyr::mutate(u_hdata_minister_date_date_out = case_when(
        u_hdata_minister_date_date_out >= lead(u_hdata_minister_date_date_in) &
          u_hdata_minister_date_country == lead(u_hdata_minister_date_country) &
          u_hdata_minister_date_cowcode == lead(u_hdata_minister_date_cowcode) &
          u_hdata_minister_date_date_out != u_hdata_minister_date_date_in ~
          lead(u_hdata_minister_date_date_in) -1,
        TRUE ~ as.Date(u_hdata_minister_date_date_out)
      ))
    
    df$u_hdata_minister_date_year_in <- format(df$u_hdata_minister_date_date_in, "%Y")
    df$u_hdata_minister_date_year_out <- format(df$u_hdata_minister_date_date_out, "%Y")
    
    return(df)
  }
  
  # Function replicates each row in the dataframe with a newdate column which contains the 
  # in- and out date for each country in the same column instead or two separate, 
  # but is completely identical otherwise
  hdata_newdate_cols <- function(df) {
    
    #ignore rows with same in and out date
    #df <- df %>% filter(u_hdata_minister_date_date_out != u_hdata_minister_date_date_in)
    
    date_out_df <- df <- dplyr::filter(df, u_hdata_minister_date_date_in != u_hdata_minister_date_date_out)
    
    date_out_df$u_hdata_minister_date_newdate <- 
      date_out_df$u_hdata_minister_date_date_out
    
    df$u_hdata_minister_date_newdate <- 
      df$u_hdata_minister_date_date_in
    
    df %<>% rbind(date_out_df) %>%
      dplyr::arrange(u_hdata_minister_date_country, u_hdata_minister_date_date_in)
    
    return(df)
    
  }
  
  to_country_date <- function(df) {
    
    df <- get_longest_period(df)
    
    df <- hdata_overlap_adjust(df)
    
    df <- hdata_newdate_cols(df)
    
    return(df)
    
  }
  
  df <- to_country_date(df)
  
  return(df)
}


#' Aggregation hdata_country_date_to_country_year
#' 
#' Aggregates from hdata_country_date to hdata_country_year
#' 
#' @param df a data.frame
#' @return data.frame aggregated to hdata_country_year.
#' @examples 
#' hdata_country_date_to_country_year(df)
#'
#' @export
#' 
hdata_country_date_to_country_year <- function(df) {
  
  # STEP 1: PREPARING DATAFRAME AND ADDING COLUMN WITH ALL YEARS FOR EACH MINISTER
  # reomove newdate column and delete the duplicates resulting from deletion with distinct
  
  df <- dplyr::select(df, -u_hdata_minister_date_newdate) %>% dplyr::distinct(.)
  
  #adjusting country name Prussia/Germany in unit column to just Germany so it matches with other hdata_country_year datasets.
  df$u_hdata_minister_date_country[df$u_hdata_minister_date_country == "Prussia/Germany"] <- "Germany"
  df$u_hdata_minister_date_country[df$u_hdata_minister_date_country == "Russia/USSR"] <- "Russia"
  df$u_hdata_minister_date_country[df$u_hdata_minister_date_country == "Ottoman Empire/Turkey"] <- "Turkey"
  
  country_years <- 
    merge((df$u_hdata_minister_date_country %>% unique()), 
          (min(df$u_hdata_minister_date_year_in):max(df$u_hdata_minister_date_year_out)))
  
  colnames(country_years) <- 
    c('u_hdata_minister_date_country', 'u_hdata_minister_date_year')
  
  # Creating df with all country years represented (ministers spanning multiple years duplicated), and arranging.
  all_min_all_years <- 
    dplyr::inner_join(country_years, df, by = c("u_hdata_minister_date_country")) %>%
    dplyr::filter(u_hdata_minister_date_year >= u_hdata_minister_date_year_in, 
           u_hdata_minister_date_year <= u_hdata_minister_date_year_out) 
  
  all_min_all_years <- dplyr::arrange(all_min_all_years,
                               u_hdata_minister_date_country, 
                               u_hdata_minister_date_year)
  
  
  gen_all_days <- function(d_first, d_last){
    d_first <- as.Date(d_first)
    d_last <- as.Date(d_last)
    
    D <- seq(d_first, d_last, 1) 
    Y <- unique(format(D, "%Y")) 
    
    
    count_all_days <- function(x) length(which(format(D, "%Y") == x)) 
    day_count <- 
      vapply(Y, count_all_days, numeric(1))
    
    return(cbind(unique(format(D, "%Y")), day_count))
  }
  
  # day_count_dataframe creates a df with days per year.
  day_count <- function(k) gen_all_days(df$u_hdata_minister_date_date_in[k],
                                        df$u_hdata_minister_date_date_out[k])
  
  output <- sapply(1:nrow(df), day_count) 
  output <- as.vector(output)
  
  df1 <- data.frame(do.call(rbind, output)) 
  
  country_year_subset <- 
    cbind(all_min_all_years, df1) %>%
    dplyr::select(-V1) %>% 
    dplyr::group_by(u_hdata_minister_date_country, 
             u_hdata_minister_date_year) %>% 
    dplyr::slice_max(as.integer(day_count), n = 1) %>%
    #for 20 duplicates in country_year day counts, 
    #we keep the observation with the later out-date
    dplyr::top_n(1, u_hdata_minister_date_date_out) %>% 
    dplyr::ungroup()
  
  #create country year unit columns
  country_year_subset$u_hdata_country_year_country <- 
    country_year_subset$u_hdata_minister_date_country
  
  country_year_subset$u_hdata_country_year_year <- 
    country_year_subset$u_hdata_minister_date_year
  
  country_year_subset$u_hdata_country_year_cowcode <- 
    country_year_subset$u_hdata_minister_date_cowcode
  
  return(country_year_subset)
}



#' Aggregation hdata_minister_date_to_country_year
#' 
#' Sequentially aggregates from minister_date to country_date, then country_date to country_year
#' 
#' @param df a data.frame
#' @return data.frame aggregated to hdata_country_year.
#' @examples 
#' hdata_minister_date_to_country_year(df)
#'
#' @export
hdata_minister_date_to_country_year <- function(df) {
  
  country_date <- demutils::hdata_minister_date_to_country_date(df)
  country_year <- demutils::hdata_country_date_to_country_year(country_date)
  
  return(country_year)
}


#' Aggregation hdata_diprep_to_country_year
#' 
#' Aggregates from dyad_year to country_year by counting the levels of representation for country 1 in a given year.
#' 
#' @param df a data.frame
#' @return data.frame aggregated to hdata_country_year.
#' @examples 
#' hdata_diprep_to_country_year(df)
#'
#' @export
hdata_dyad_year_to_country_year <- function(df){
  
  df %<>% group_by(u_hdata_dyad_year_country_one, u_hdata_dyad_year_year) %>%
    dplyr::mutate(hdata_diprep_count_diprep_level_0 = sum(hdata_diprep_diprep_dr == 0)) %>%
    dplyr::mutate(hdata_diprep_count_diprep_level_1 = sum(hdata_diprep_diprep_dr == 1)) %>%
    dplyr::mutate(hdata_diprep_count_diprep_level_2 = sum(hdata_diprep_diprep_dr == 2)) %>%
    dplyr::mutate(hdata_diprep_count_diprep_level_3 = sum(hdata_diprep_diprep_dr == 3)) %>%
    dplyr::mutate(hdata_diprep_count_diprep_level_9 = sum(hdata_diprep_diprep_dr == 9)) %>%
    # Original variables should not be selectable in country-year format
    dplyr::select(-hdata_diprep_country_name2, -hdata_diprep_cow_code2, -hdata_diprep_vdem_code2,
           -hdata_diprep_diprep_dr) %>%
    distinct(u_hdata_dyad_year_country_one, u_hdata_dyad_year_year, .keep_all = TRUE)
  
  return (df)
  
}