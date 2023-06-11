#' @export
disaggregate_cy <- function(ds, ...) UseMethod("disaggregate_cy")

#' @export
disaggregate_cy.pandem_cs <- function(ds, db) {
	ds$year <- 2020
	ds <- vbase::recycle_df(ds, 2)
	ds$year[1:(nrow(ds)/2)] <- 2021
	return(ds)
}