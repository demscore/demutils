get_children <- function(var_tag, bool, children_df) {
	if (!bool) {
		return(var_tag)
	}
	children_df %>%
		dplyr::filter(head_var %in% var_tag) %$% tag_long %>% c(var_tag, .)
}

set_stata_labels <- function(x, labels) {
   	if (!inherits(x, "data.frame"))
       	stop("Stata labels can only be set on data frames", call. = FALSE)
   	if (length(labels) != ncol(x))
       	stop("Length mismatch between labels and columns", call. = FALSE)
   	attr(x, "var.labels") <- labels
   	x
}

autogen_load_ref_data <- function(args, POSTGRES_TABLES_DIR, LOCAL) {
	VARIABLES_LONG_TAGS <- args$variables
	DJANGO_DB_HOST_DATA <- args$DJANGO_DB_HOST_DATA
	DJANGO_DB_NAME_DATA <- args$DJANGO_DB_NAME_DATA
	DJANGO_DB_PASSWORD_DATA <- args$DJANGO_DB_PASSWORD_DATA
	DJANGO_DB_USER_DATA <- args$DJANGO_DB_USER_DATA

	# DATA_OUTFILE_TYPE <- 
	# 	case_when(
	# 		FILE_FORMAT == "CSV" ~ "csv",
	# 		FILE_FORMAT == "R" ~ "rds",
	# 		FILE_FORMAT == "STATA" ~ "dta")

	ll <- list()

	# When using a database
	if (isTRUE(!LOCAL)) {
		creds <- c(host = DJANGO_DB_HOST_DATA, 
					dbname = DJANGO_DB_NAME_DATA,
					user = DJANGO_DB_USER_DATA,
					password = DJANGO_DB_PASSWORD_DATA,
					port = 5432)
		db <- do.call(DBI::dbConnect,
					  c(drv = RPostgreSQL::PostgreSQL(max.con = 50), creds))
		ll$head_var_parents <- DBI::dbGetQuery(db, "SELECT * FROM head_var_parents;")
		if (isTRUE(any(VARIABLES_LONG_TAGS %in% ll$head_var_parents$head_var))) {
			ll$head_var_children <- DBI::dbGetQuery(db, "SELECT * FROM head_var_children;")
				
		}

		DBI::dbDisconnect(db)
	} else {
		ll$head_var_parents <- readRDS(file.path(POSTGRES_TABLES_DIR, 
										  "head_var_parents.rds"))
		if (isTRUE(any(VARIABLES_LONG_TAGS %in% ll$head_var_parents$head_var))) {
			ll$head_var_children <- readRDS(file.path(POSTGRES_TABLES_DIR, 
											   "head_var_children.rds"))
		}
	}

	return(ll)
}

autogen_read_unit_data <- function(ll, args, UNIT_DATA_DIR, POSTGRES_TABLES_DIR,
                                   UNIT_TABLE_DIR) {
  VARIABLES_LONG_TAGS <- args$variables
  INCLUDE_UNIT_COLS <- isTRUE(args$include_unit_cols)
  OUTPUT_UNIT_TAG <- args$output_unit_tag
  head_var_parents <- ll$head_var_parents
  remove_empty_rows <- isTRUE(args$remove_empty_rows)
  select_all_rows <- isTRUE(args$select_all_rows)
  
  country <- as.character(args$country)
  country <- strsplit(country, split = ",", fixed = TRUE) %>% unlist
  
  conflict_location <- as.character(args$conflict_location)
  conflict_location <- strsplit(conflict_location, split = ",", fixed = TRUE) %>% unlist
  
  region <- as.character(args$region)
  region <- strsplit(region, split = ",", fixed = TRUE) %>% unlist
  
  min_year <- args$`min_year`
  max_year <- args$`max_year`
  min_date <- args$`min_date`
  max_date <- args$`max_date`
  
  #------------------------------
  
  print(args$country)
  print(country)
  
  print(args$conflict_location)
  print(conflict_location)
  
  min_year_num <- as.numeric(args$min_year)
  stopifnot(is.numeric(min_year_num))
  print(min_year_num)
  str(min_year_num)
  
  max_year_num <- as.numeric(args$max_year)
  stopifnot(is.numeric(max_year_num))
  print(max_year_num)
  str(max_year_num)
  
  #------------------------------
  
  if (isTRUE(any(VARIABLES_LONG_TAGS %in% head_var_parents$head_var))) {
    head_var_children <- ll$head_var_children
    head_var_children %<>% dplyr::arrange(head_var_children_id)
    bool_head <- VARIABLES_LONG_TAGS %in% head_var_parents$head_var
    outvars <- Map(get_children, 
                   var_tag = VARIABLES_LONG_TAGS,
                   bool = bool_head,
                   children_df = vbase::list_replicate(head_var_children, length(VARIABLES_LONG_TAGS))
    ) %>% unlist
    
    VARIABLES_LONG_TAGS <- outvars
  }
  
  # Maximum length of arguments to pass to R?
  # https://serverfault.com/questions/163371/linux-command-line-character-limit
  
  # Read files
  ll <- lapply(VARIABLES_LONG_TAGS, function(v) {
    readRDS(file.path(UNIT_DATA_DIR, OUTPUT_UNIT_TAG, paste0(v, ".rds")))
  })
  
  # Are there column name clashes in the chosen column names?
  # If so, rename them to the long names that include the original dataset_tag.
  # We could also keep one in the original form anbd rename only the duplicates.
  short_names <- lapply(ll, function(df) {names(df)}) %>% unlist
  long_names <- lapply(ll, function(df) {
    paste(class(df)[2], names(df), sep = "_")
  }) %>% unlist
  dupls <- duplicated(short_names) | duplicated(short_names, fromLast = TRUE)
  outnames <- short_names
  outnames[dupls] <- long_names[dupls]
  
  
  # Merge files into a data.frame and rename columns
  outdf <- suppressMessages(dplyr::bind_cols(ll)) %>% setNames(., outnames)
  
  unit_names <- NULL
  # Add unit columns
  if (INCLUDE_UNIT_COLS) {
    utable <- readRDS(file.path(UNIT_TABLE_DIR, paste0(OUTPUT_UNIT_TAG, ".rds")))
    unit_names <- names(utable)
    outdf <- dplyr::bind_cols(utable, outdf)
  }
  
  
  # If we decide that all NAs from merges are replaced with -11111
  if (remove_empty_rows) {
    
    non_u_cols <- grep("^(?!u_).", names(outdf), perl = TRUE)
    non_u_col_names <- names(outdf)[non_u_cols]
    
    outdf <- outdf[rowSums(data.frame(outdf[, non_u_col_names]) != -11111, na.rm = TRUE) > 0, ]
    
  }

  # If we decide that all NAs from merges are replaced with -11111
  if (remove_empty_rows) {
    
    non_u_cols <- grep("^(?!u_).", names(outdf), perl = TRUE)
    non_u_col_names <- names(outdf)[non_u_cols]
    
    outdf <- outdf[rowSums(data.frame(outdf[, non_u_col_names]) != -11111, na.rm = TRUE) > 0, ]
    
  }

  # Year selection
  if (length(min_year_num) == 0 & length(max_year_num) == 0) {
    
    outdf <- outdf} else if (OUTPUT_UNIT_TAG %in% c("u_demscore_country_year", "u_qog_country_year", 
                             "u_vdem_country_year", "u_complab_country_year", 
                             "u_hdata_country_year", "u_repdem_country_year",
                             "u_ucdp_country_year", "u_ucdp_orgv_country_year",
                             # Location units
                             "u_ucdp_conflict_year", "u_ucdp_dyad_year", "u_ucdp_gedid",
                             # Region units
                             "u_qog_region_year")) {
      
    u_year <- c("u_vdem_country_year_year", "u_demscore_country_year_year",
                "u_qog_country_year_year", "u_complab_country_year_year",
                "u_hdata_country_year_year", "u_repdem_country_year_year",
                "u_ucdp_orgv_country_year_year_cy", "u_ucdp_country_year_year",
                "u_ucdp_conflict_year_year", "u_ucdp_dyad_year_year",
                "u_ucdp_gedid_year", "u_qog_region_year_year")   
    
    
    year_column <- dplyr::intersect(u_year, names(outdf))     
    
    outdf <- outdf[outdf[[year_column]] >= min_year_num & 
                     outdf[[year_column]] <= max_year_num, ]
    
  }


  # Filter option for countries
  if(select_all_rows) {
    
    outdf <- outdf 
    
    # Specify to which units the selection option applies
  } else if (OUTPUT_UNIT_TAG %in% c("u_demscore_country_year", "u_qog_country_year", 
                                    "u_vdem_country_year", "u_complab_country_year", 
                                    "u_hdata_country_year", "u_repdem_country_year",
                                    "u_ucdp_country_year", "u_ucdp_orgv_country_year")) {
    
    u_country <- c("u_vdem_country_year_country", "u_demscore_country_year_country",
                   "u_qog_country_year_country", "u_complab_country_year_country",
                   "u_hdata_country_year_country", "u_repdem_country_year_country",
                   "u_ucdp_orgv_country_year_country_cy", "u_ucdp_country_year_name")
    
    country_column <- dplyr::intersect(u_country, names(outdf))
    
    print(args$country)
    print(country)
    
    outdf <- outdf[outdf[[country_column]] %in% country,]
    
  } else if (OUTPUT_UNIT_TAG %in% c("u_ucdp_conflict_year",
                                    "u_ucdp_dyad_year",
                                    "u_ucdp_gedid")) {
    
    u_conf <- c("u_ucdp_conflict_year_location", "u_ucdp_dyad_year_location",
                "u_ucdp_gedid_country")
    
    conflict_column <- dplyr::intersect(u_conf, names(outdf))
    
    outdf <- outdf[outdf[[conflict_column]] %in% conflict_location,]
    
  } else if (OUTPUT_UNIT_TAG %in% c("u_qog_region_year")) {
    
    u_region <- c("u_qog_region_year_region_name")
    
    region_column <- dplyr::intersect(u_region, names(outdf))
    
    print(args$region)
    print(region)
    
    outdf <- outdf[outdf[[region_column]] %in% region,]
    
  } else if (OUTPUT_UNIT_TAG %in% c("u_repdem_cabinet_date", 
                                    "u_hdata_cabinet_date", 
                                    "u_vdem_country_date",
                                    "u_hdata_minister_date")) {
    
    # Min date corresponds to in_date and max_date to out date for cabinet units, columns are the same for vdem
    u_country <- c("u_repdem_cabinet_date_country", "u_hdata_cabinet_date_country", 
                   "u_vdem_country_date_country_name", "u_hdata_minister_date_country")
    u_min_date <- c("u_repdem_cabinet_date_date_in", "u_hdata_cabinet_date_date_in", 
                    "u_vdem_country_date_date", "u_hdata_minister_date_date_in")
    u_max_date <- c("u_repdem_cabinet_date_date_out", "u_hdata_cabinet_date_date_out", 
                    "u_vdem_country_date_date", "u_hdata_minister_date_date_out")
    
    country_column <- dplyr::intersect(u_country, names(outdf))
    min_column <- dplyr::intersect(u_min_date, names(outdf))
    max_column <- dplyr::intersect(u_max_date, names(outdf))
    
    #print(args$country)
    print(country)
    
    #print(args$min_date)
    print(min_date)
    
    #print(args$max_date)
    print(max_date)
    
    outdf <- outdf[outdf[[country_column]] == country & 
                     outdf[min_column] >= min_date & 
                     outdf[max_column] <= max_date, ]
    
  }  
  
  
  class(outdf) <- c(OUTPUT_UNIT_TAG, "data.frame")
  attr(outdf, "long_names") <- VARIABLES_LONG_TAGS
  return(outdf)
  
}

autogen_write_file <- function(outdf, args, POSTGRES_TABLES_DIR) {

	INCLUDE_UNIT_COLS <- isTRUE(args$include_unit_cols)
	OUTFILE <- args$outfile
	FILE_FORMAT <- args$file_format
	
	#----
	OUTPUT_UNIT_TAG <- args$output_unit_tag
	long_names <- attr(outdf, "long_names")
	
	utable <- readRDS(file.path(UNIT_TABLE_DIR, paste0(OUTPUT_UNIT_TAG, ".rds")))
	unit_names <- names(utable)
	#----

	if (FILE_FORMAT == "CSV") {
		data.table::fwrite(outdf, OUTFILE)
	}

	if (FILE_FORMAT == "R") {
		saveRDS(outdf, OUTFILE)
	}

	if (FILE_FORMAT == "STATA") {
		variables <- readRDS(file.path(POSTGRES_TABLES_DIR, "labels.rds"))
		if (INCLUDE_UNIT_COLS) {
			variables %<>% dplyr::filter(tag_long %in% c(unit_names, long_names))
			labels <- variables$label[match(c(unit_names, long_names), 
										variables$tag_long)]
		} else {
			variables %<>% dplyr::filter(tag_long %in% long_names)
			labels <- variables$label[match(long_names, 
										variables$tag_long)]
		}
	
		outdf <- set_stata_labels(outdf, labels)
		vbase::write_file(outdf,
 	              OUTFILE,
 	              version = 118,
  	              data.label = "Demscore")
	}
	return(invisible(NULL))
}

#' @title Create Demscore dataset file
#' @description Creates the demscore dataset from command line arguments,
#' prepped postgres tables, and the unit data and unit tables.
#' The apropriate command line arguments and environment variables must be defined.
#' @param args Command line arguments as a list (usually passed on from docopt)
#' \itemize{
#' \item help (flag): Show help message
#' \item include_unit_cols (flag): Include unit table variables
#' \item download_id (value): Download id
#' \item project (value): Project tag
#' \item dataset (value): Dataset tag
#' \item thematic (value): Theme tag
#' \item output_unit_tag (value): Output unit tag
#' \item variables (value): Variable long tags
#' \item outfile (value): Output file
#' \item file_format (value): Output file format (R, CSV, STATA)
#' \item remove_empty_rows (flag): Remove empty rows.
#' \item DJANGO_DB_HOST_DATA (value): Django database host
#' \item DJANGO_DB_NAME_DATA (value): Django database name
#' \item DJANGO_DB_USER_DATA (value): Django database user
#' \item DJANGO_DB_PASSWORD_DATA (value): Django database password
#' \item data_version (value): Data version
#' }
#' @param POSTGRES_TABLES_DIR Path to prepped postgres tables
#' @param UNIT_DATA_DIR Path to unit data
#' @param UNIT_TABLE_DIR Path to unit tables
#' @param LOCAL Logical, TRUE if the function should not connect to a database,
#' FALSE if it should, connection details are passed in `args`
#' @return Returns NULL.
#' @details The dataset can be created in one of four modes, variable selection,
#' project selection, dataset selection and thematic selection. The mode is
#' determined by the command line arguments.
#' @export
generate_data <- function(args, POSTGRES_TABLES_DIR, UNIT_DATA_DIR, LOCAL, 
	UNIT_TABLE_DIR, prep = FALSE) {

	if (prep) {
		prepare_autogen()
	}
	
	autogen_load_ref_data(args, POSTGRES_TABLES_DIR, LOCAL) %>%
	autogen_read_unit_data(., args, UNIT_DATA_DIR, POSTGRES_TABLES_DIR, 
		UNIT_TABLE_DIR) %>%
	autogen_write_file(., args, POSTGRES_TABLES_DIR)
}