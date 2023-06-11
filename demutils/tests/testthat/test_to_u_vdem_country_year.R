test_that("to_u_vdem_country_year.u_qog_country_year", {
	# Check weird input values
    expect_error(to_u_vdem_country_year(NaN))
	expect_error(to_u_vdem_country_year(NA))

	# Check empty data.frame
	df <- data.frame()
	class(df) <- c("u_qog_country_year", "vdem_cy", "data.frame")
	expect_error(to_u_vdem_country_year(df))

	# Check for weird values inside df
	df <- data.frame(a = c(1, 2, NaN))
	class(df) <- c("u_qog_country_year", "vdem_cy", "data.frame")
	expect_error(to_u_vdem_country_year(df))

	m <- matrix(c(1, 2, NaN))
	class(m) <- c("u_qog_country_year", "vdem_cy", "matrix")
	expect_error(to_u_vdem_country_year(m))


})