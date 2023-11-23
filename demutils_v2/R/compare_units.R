#' @export
compare_country_years <- function(utable1, utable2, namecol1, namecol2,
	yearcol1, yearcol2) {
	utable1$year <- utable1[[yearcol1]]
	utable2$year <- utable2[[yearcol2]]
	utable1$country <- utable1[[namecol1]]
	utable2$country <- utable2[[namecol2]]
	utable1 %<>% select(country, year)
	utable2 %<>% select(country, year)
	out <- list(
		dplyr::anti_join(utable1, utable2, by = c("country", "year")) %>% 
			arrange(country, year),
		dplyr::anti_join(utable2, utable1, by = c("country", "year")) %>% 
			arrange(country, year),
		dplyr::inner_join(utable1, utable2, by = c("country", "year")) %>% 
			arrange(country, year)) %>% 
		setNames(c("left_has", "right_has", "common"))

	out$country_summary <- lapply(out, function(df) {
		df %>% 
			group_by(country) %>%
			summarize(min_year = min(year),
				max_year = max(year),
				n_years = n(),
				max_min_diff = max_year - min_year + 1)
	})

	return(out)
}

#' @export
compare_country_names <- function(utable1, utable2, namecol1, namecol2) {
	list(
		setdiff(utable1[[namecol1]], utable2[[namecol2]]) %>% sort,
		setdiff(utable2[[namecol2]], utable1[[namecol1]]) %>% sort,
		intersect(utable1[[namecol1]], utable2[[namecol2]]) %>% sort) %>%
	setNames(c("left_has", "right_has", "common"))
}