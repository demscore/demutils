#' @export
left_join_class <- function(x, y, by, 
		suffix = c("_" %^% class(x)[1], "_" %^% class(y)[1]), 
		...) {
	stopifnot(class(x)[1] == class(y)[1])
	df <- dplyr::left_join(x, y, by = by, suffix = suffix, ...)
	return(df)
}

#' @export
full_join_class <- function(x, y, by, 
		suffix = c("_" %^% class(x)[1], "_" %^% class(y)[1]), 
		...) {
	stopifnot(class(x)[1] == class(y)[1])
	df <- dplyr::full_join(x, y, by = by, suffix = suffix, ...)
	return(df)
}

#' @export
no_missing_values <- function(df, cols) {
	bool <- !any(is.na(df[, cols]))
	if (bool) {
		return(TRUE)
	} else {
		missing_value_freq(df, cols)
		return(FALSE)
	}
}

#' @export
left_join_dem <- function(df1, df2, by = by, ...) {

	# Is by a named vector?
	if (!is.null(names(by))) {
		by1 <- names(by)
		by2 <- setNames(by, NULL)
		by1[by1 == ""] <- by2[by1 == ""]
	} else {
		by1 <- by
		by2 <- by
	}

	# Are there missing values in the merge columns?
	if (!no_missing_values(df1, by1)) {
		stop("Missing values in merge columns in df1...")
	}
	if (!no_missing_values(df2, by2)) {
		stop("Missing values in merge columns in df2...")
	}

	# Are there duplicates in the merge columns?
	stopifnot(`Duplicates in merge columns in df1: ` = no_duplicates(df1, by1))
	stopifnot(`Duplicates in merge columns in df2: ` = no_duplicates(df2, by2))

	# Are the data types the same for both variables?
	v_class1 <- df1[, by1, drop = FALSE] %>% lapply(class) %>% unlist
	v_class2 <- df2[, by2, drop = FALSE] %>% lapply(class) %>% unlist
	v_compare <- v_class1 == v_class2
	if (any(!v_compare)) {
		print(v_class1[!v_compare])
		print(v_class2[!v_compare])
		stop("Merge columns are of different type! Change the type of one of them!")
	}

	out <- left_join(df1, df2, by, ...)
	stopifnot(`The output data.frame created duplicate rows!` = nrow(out) == nrow(df1))
	return(out)

}