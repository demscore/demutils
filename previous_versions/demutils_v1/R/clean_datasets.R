
#' @export
clean_column_names <- function(v_char, remove_digits = FALSE) {
	v_char_original <- v_char
  	v_char_clean <- 
	  	v_char %>%
    	gsub(" ", "_", ., fixed = TRUE) %>%
    	gsub(".", "_", ., fixed = TRUE) %>%
    	gsub("-", "_", ., fixed = TRUE) %>%
    	gsub("(", "", ., fixed = TRUE) %>%
    	gsub(")", "", ., fixed = TRUE) %>%
    	gsub("[", "", ., fixed = TRUE) %>%
    	gsub("]", "", ., fixed = TRUE) %>%
  	  gsub("_$", "_dollar", ., fixed = TRUE) %>%
    	# Turn all elements inside [] into empty string
    	gsub("[?^+*!]", "", .)
	if (remove_digits)
		v_char_clean %<>% gsub("\\d+", "", .)
	# All lower cap
    v_char_clean %<>% tolower(.)
	stopifnot(length(unique(v_char_clean)) == length(v_char_original))
	return(v_char_clean)
}

#' @export
no_duplicate_names <- function(v) {
  length(v) == length(unique(v))
}

#' @export
fix_ucdp_milc <- function(df) {
# 1. need to fill in NA values of row 3 with values from row 2 or 1, 
# collapsing downward.
    row_1 <- df[1,]
    row_2 <- df[2,]
    row_3 <- df[3,]
    coal_1 <- coalesce(row_3, row_2)
    coal_2 <- coalesce(coal_1, row_1)
# 2. remove rows 1, 2, and 3.
    df <- df %>% slice(-c(1, 2, 3))
# 3. Replace df column names with new names.
    colnames(df) <- coal_2
    colnames(df)[84:91] <- c(
      "indirect_talks1",
      "indirect_talks2",
      "direct_talks1",
      "direct_talks2",
      "unclear_talks1",
      "unclear_talks2",
      "bilateral_talks1",
      "bilateral_talks2")
    colnames(df)[15] <- c(
      "active_year")
    colnames(df)[99] <-c(
      "year_of_war")
    return(df)
}





