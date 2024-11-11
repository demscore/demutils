



#' @export
aggregate_cy <- function(ds, ...) UseMethod("aggregate_cy")

#' @export
aggregate_cy.pandem_ts <- function(ds, ...) {
	tag <- class(ds)[1]
	df <- dataset_info(tag, db)
	vars <- download_vars(tag, db) %>% dplyr::select(original_tag, aggregation)	
	
	# First transform to country-date
	
	# Not really necessary now

	# Then loop over columns and 

	# apply on each year and column



}



