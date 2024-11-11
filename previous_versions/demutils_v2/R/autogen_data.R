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
	# If so, rename them to the long names that include the original dataset tag.
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

	#if (remove_empty_rows) {
	#	empty_rows <- apply(outdf[, !names(outdf) %in% unit_names, drop = FALSE], 1, allNA)
	#	outdf <- outdf[!empty_rows, , drop = FALSE]
	#}

	# If we decide that all NAs from merges are replaced with -11111
	if (remove_empty_rows) {
	  
	  non_u_columns <- grep("^(?!u_).", names(outdf), perl = TRUE)
	  
	  outdf <- outdf[rowSums(outdf[, non_u_columns] != -11111, na.rm = TRUE) > 0, ]
	
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