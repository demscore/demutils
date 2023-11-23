
#' repdem_cabinet_date_to_country_date
#' 
#' Aggregates from repdem_cabinet_date to repdem_country_date
#' 
#' @param df a data.frame
#' @return data.frame aggregated to repdem_country_date.
#' @examples 
#' repdem_cabinet_date_to_country_date(df)
#'
#' @export
repdem_cabinet_date_to_country_date <- function(df) {
  
  #bind in unit cols
  unit_table_start <- read_unit_table("u_repdem_cabinet_date")
  df %<>% bind_cols(unit_table_start)
  
  #overlap adjustment function
  repdem_overlap_adjust <- function(df){
    
    df %<>% 
      dplyr::group_by(u_repdem_cabinet_date_country) %>% 
      dplyr::arrange(u_repdem_cabinet_date_date_in, .by_group = TRUE) %>%
      #if there is an overlap, subtract 1 day from date_out
      dplyr::mutate(u_repdem_cabinet_date_date_out = case_when(
        u_repdem_cabinet_date_date_out >= lead(u_repdem_cabinet_date_date_in) &
          u_repdem_cabinet_date_country == lead(u_repdem_cabinet_date_country) ~
          lead(u_repdem_cabinet_date_date_in) -1,
        TRUE ~ as.Date(u_repdem_cabinet_date_date_out)
      )) %>%
	  dplyr::ungroup(.)
    
    return(df)
  }
  
  
  # Function replicates each row in the dataframe with a newdate column which contains the 
  # in- and out date for each cabinet in the same coulmn instead or two separate, 
  # but is completely identical otherwise
  repdem_newdate_cols <- function(df) {
    
    date_out_df <- df <- filter(df, u_repdem_cabinet_date_date_in != u_repdem_cabinet_date_date_out)
    
    date_out_df$u_repdem_cabinet_date_newdate <- 
      date_out_df$u_repdem_cabinet_date_date_out
    
    df$u_repdem_cabinet_date_newdate <- 
      df$u_repdem_cabinet_date_date_in
    
    df %<>% 
      rbind(date_out_df) %>%
      dplyr::arrange(u_repdem_cabinet_date_country, u_repdem_cabinet_date_date_in)
    
    return(df)
  }
  
  to_country_date <- function(df) {
    
    df <- repdem_overlap_adjust(df)
    
    df <- repdem_newdate_cols(df)
    
    return(df)
    
  }
  #remove empty rows function
	remove_empty_rows <- function(df) {
		ind <- !apply(select(df, -matches("^u_")), 1, allNA)
		return(df[ind, ])
	}

	df %<>% remove_empty_rows

	df <- to_country_date(df)
  
	df %<>% 
		dplyr::rename(newdate = u_repdem_cabinet_date_newdate)

	stopifnot(no_duplicates(df, c("u_repdem_cabinet_date_country", 
		"newdate")))
  	return(df)
  
}


#' repdem_country_date_to_country_year
#' 
#' Aggregates from repdem_country_date to repdem_country_year
#' 
#' @param df a data.frame
#' @return data.frame aggregated to repdem_country_year.
#' @examples 
#' repdem_country_date_to_country_year(df)
#'
#' @export
repdem_country_date_to_country_year <- function(df) {
  
  # STEP 1: PREPARING DATAFRAME AND ADDING COLUMN WITH ALL YEARS FOR EACH CABINET
  # reomove newdate column and delete the duplicates resulting from deletion with distinct
  df <- dplyr::select(df, -newdate) %>% dplyr::distinct(.)
  
  country_years <- 
    data.frame(merge((unique(df$u_repdem_cabinet_date_country)), 
                     (min(df$u_repdem_cabinet_date_in_year):max(df$u_repdem_cabinet_date_out_year)))) 
  
  colnames(country_years) <- 
    c('u_repdem_cabinet_date_country', 'u_repdem_cabinet_date_year')
  
  
  # Creating df with all country years represented (cabinets spanning multiple years duplicated), and arranging.
  all_cabs_all_years <-
    dplyr::inner_join(country_years, df, by = c("u_repdem_cabinet_date_country")) %>%
    dplyr::filter(u_repdem_cabinet_date_year >= u_repdem_cabinet_date_in_year, 
           u_repdem_cabinet_date_year <= u_repdem_cabinet_date_out_year) %>%
    dplyr::arrange(u_repdem_cabinet_date_country, 
            u_repdem_cabinet_date_year)
  
  
  # STEP 2 & 3: CREATING DATAFRAME WITH DAYS PER YEAR 
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
  day_count_dataframe <- function(k) gen_all_days(df$u_repdem_cabinet_date_date_in[k],
                                                  df$u_repdem_cabinet_date_date_out[k])
  
  output <- sapply(1:nrow(df), day_count_dataframe)
  as.vector(output)
  
  df1 <- data.frame(do.call(rbind, output)) 
  
  
  # STEP 4: BIND DATAFRAMES AND DELETE UNNECESSARY COLUMN WITH YEARS
  
  country_year_subset <- 
    cbind(all_cabs_all_years, df1) %>%
    dplyr::select(-V1) %>% 
    dplyr::group_by(u_repdem_cabinet_date_country, 
                    u_repdem_cabinet_date_year) %>%
    dplyr::slice_max(as.integer(day_count), n = 1) %>%
    dplyr::filter(!(u_repdem_cabinet_date_cab_name == "Adenauer V" & 
                      u_repdem_cabinet_date_year == 1960)) %>%
    dplyr::filter(!(u_repdem_cabinet_date_cab_name == "Kurz II" & 
                      u_repdem_cabinet_date_year == 2019)) %>%
    dplyr::ungroup(.)
  
  #create country year unit columns
  country_year_subset$u_repdem_country_year_country <- 
    country_year_subset$u_repdem_cabinet_date_country
  
  country_year_subset$u_repdem_country_year_year <- 
    country_year_subset$u_repdem_cabinet_date_year
  
  #removing the unnecessary day_count column used in these functions
  country_year_subset <- select(country_year_subset, -day_count)
	
	stopifnot(no_duplicates(country_year_subset,
		c("u_repdem_country_year_country", "u_repdem_country_year_year")))

  return(country_year_subset)
  
}

#' repdem_cabinet_date_to_country_year
#' 
#' Sequentially aggregates from cabinet_date to country_date, then country_date to country_year
#' 
#' @param df a data.frame
#' @return data.frame aggregated to repdem_country_year.
#' @examples 
#' repdem_cabinet_date_to_country_year(df)
#'
#' @export
repdem_cabinet_date_to_country_year <- function(df) {
  
  country_date <- demutils::repdem_cabinet_date_to_country_date(df)
  country_year <- demutils::repdem_country_date_to_country_year(country_date)
  
  return(country_year)
}
