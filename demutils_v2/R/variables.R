
#' @export
get_variable_tag <- function(v, variables) {
	out <- variables$tag[variables$tag_long == v]
	if (length(out) != 1) {
		stop(paste0("ERROR: More or less than one variable tag found: ", v))
	}
	return(out)
}

#' @export
get_dataset_tag <- function(v, variables, datasets) {
	ds_id <- variables$dataset_id[variables$tag_long == v]
	out <- datasets$tag[datasets$dataset_id == ds_id]
	if (length(out) != 1)
		stop("ERROR: More or less than one dataset tag found!")
	return(out)
}