#' @export
read_datasets <- function(tag, db, original = FALSE, assign = FALSE, ...) {
	vbase::info("Reading the following datasets: " %^% tag)
	df <- load_datasets(db) 
	df <- df[!is.na(df$tag), ]
	stopifnot(tag %in% df$tag)
	
	ll <- lapply(tag, function(ta) {
	  
	  ROOT <- Sys.getenv("ROOT_DIR")
	  
		if (original) {
			ds <- vbase::read_file(file.path(ROOT, df$original_path[df$tag == ta]), ...) %>% vbase::untibble(.)
		} else {
			ds <- vbase::read_file(file.path(ROOT, df$path[df$tag == ta]), ...) %>% vbase::untibble(.)
		}
		
		class(ds) <- c(ta, "data.frame")
		return(ds)
	}) %>% setNames(tag)

	if (assign) {
		lapply(names(ll), function(ta) {
			assign(ta, ll[[ta]], envir = globalenv())
		})
		vbase::info("Assigning datasets to names in global environment.")
		return(NULL)
	}
	
	if (length(ll) == 1) 
		return(ll[[1]])
	return(ll)
	
}

#' @export
read_unit_table <- function(unit_tag, ROOT_DIR = Sys.getenv("ROOT_DIR"), msg = FALSE, ...) {
	f <- file.path(ROOT_DIR, "unit_tables", paste0(unit_tag, ".rds"))
	if(!file.exists(f))
		stop("Unit table with this tag does not exist!")
	df <- read_file(f, msg = msg, ...)
	class(df) <- c(unit_tag, "data.frame")
	return(df)
}

#' @export
write_dataset <- function(o, f, tag, ...) {
  stopifnot(!grepl("^u_", tag))
  stopifnot(is.data.frame(o))
  if(any((lapply(o, . %>% class %>% length) %>% unlist) != 1))
    stop("Columns must have 1 class")
  class(o) <- c(tag, "data.frame")
  o <- ungroup(o)
  attributes(o) <- 
    list(names = attr(o, "names"), 
         class = attr(o, "class"),
         row.names = attr(o, "row.names"))
  vbase::write_file(o, f, ...)
}

#' @export
write_unit_table <- function(o, f, tag, ...) {
	stopifnot(grepl("^u_", tag))
	stopifnot(is.data.frame(o))
	class(o) <- c(tag, "data.frame")
	o <- ungroup(o)
	attributes(o) <- 
	  list(names = attr(o, "names"), 
	       class = attr(o, "class"),
	       row.names = attr(o, "row.names"))
	vbase::write_file(o, f, ...)
}



#' @title Read unit data
#' @description Read data from unit_data/ directory for a
#' given unit tag and dataset tag. If a variable is not found an warning 
#' is issued. If none of the variables are found, then an error is raised.
#' @param unit Unit tag
#' @param ds_tag Dataset tag
#' @param variables Variables table from postgres
#' @param datasets Datasets table from postgres
#' @param ... Additional arguments to \code{\link{vbase::read_file}}
#' @return Returns a data.frame for variables in the specified dataset from
#' the specified output unit. The data.frame has as class first the 
#' output unit tag, then the dataset tag, and then "data.frame".
#' @export
read_unit_data <- function(unit, ds_tag, variables, datasets, ...) {

	rename_short_to_long_columns <- function(ll) {
		Map(function(df, nn) {
			names(df) <- nn
			return(df)
		}, df = ll, nn = names(ll))
	}

	
	ds_id <- datasets$dataset_id[datasets$tag == ds_tag]
	variables %<>% 
		dplyr::filter(dataset_id %in% ds_id) %>%
		dplyr::filter(!grepl("^u_", tag)) %>%
		dplyr::arrange(variable_id)

	vars <- variables$tag_long

	ll_files <- file.path(Sys.getenv("ROOT_DIR"), "unit_data", unit, 
		paste0(vars, ".rds"))
	file_exists <- file.exists(ll_files)
	
	if (any(!file_exists)) {
		warn("These variables do not exist for dataset " %^% ds_tag %^% 
		 " for unit " %^% unit %^% ": " %^% vars[!file_exists])
	}
	
	ll_files <- ll_files[file_exists]
	if (length(ll_files) == 0L) {
		info("Output unit: " %^% unit %^% 
		" Dataset tag: " %^% ds_tag)
		stop("There are no variables for this selection!")
	}

	ll <- lapply(ll_files, function(f) {read_file(f, ...)})
	nn <- ll_files %>% basename %>% tools::file_path_sans_ext(.)
	names(ll) <- nn

	unit_table <- read_unit_table(unit, ...)

	# Check for wrongly sized data!
	v <- lapply(ll, nrow) %>% unlist
	if (!all(v == nrow(unit_table))) {
		info("Some unit data files have different number of rows than related unit table!")
	  info("Current dataset: " %^% ds_tag %^% 
	         " Current Output Unit " %^% unit)
		info(ll_files[which(v != nrow(unit_table))] %>% basename)
		stop("ERROR")
	}



	ll <- rename_short_to_long_columns(ll)
	df <- dplyr::bind_cols(ll)
	class(df) <- c(unit, ds_tag, "data.frame")

	# Last check:
	stopifnot(ncol(df) != 0L)
	stopifnot(`Wrong number of rows` = nrow(df) == nrow(unit_table))
	return(df)
}