
#' @export
load_datasets <- function(db) {
	datatets <- 
		dplyr::tbl(db, "datasets") %>% 
		dplyr::collect(n = Inf) %>% 
		vbase::untibble(.)
}

#' @export
dataset_info <- function(tag, db) {
	df <- load_datasets(db) 
	df <- df[!is.na(df$tag), ]
	return(df[df$tag == tag, ])
}

#' @export
download_vars <- function(tag, db) {
	id <- dataset_info(tag, db)$dataset_id
	DBI::dbGetQuery(db, "SELECT * FROM variables WHERE dataset_id = " %^% id %^% ";")
}


