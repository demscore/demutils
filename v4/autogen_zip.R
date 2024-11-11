
#' @export
generate_zip <- function(args, envir) {
	# Environment
	POSTGRES_TABLES_DIR <- envir$POSTGRES_TABLES_DIR
	REF_STATIC_DIR <- envir$REF_STATIC_DIR
	UNIT_DATA_DIR <- envir$UNIT_DATA_DIR
	LOCAL <- envir$LOCAL
	UNIT_TABLE_DIR <- envir$UNIT_TABLE_DIR

	# Parse some arguments
	FILE_FORMAT <- args$file_format
	DATA_OUTFILE_TYPE <- 
		dplyr::case_when(
			FILE_FORMAT == "CSV" ~ "csv",
			FILE_FORMAT == "R" ~ "rds",
			FILE_FORMAT == "STATA" ~ "dta")
	OUTFILE <- args$outfile

	# Generate data
	temp_file_data <- file.path(tempdir(), paste0("data.", DATA_OUTFILE_TYPE))
	cat(paste0("Creating data file at location: ", temp_file_data), sep = "\n")
	args$outfile <- temp_file_data
	generate_data(args, POSTGRES_TABLES_DIR, UNIT_DATA_DIR, LOCAL,
		UNIT_TABLE_DIR)

	# Generate codebook
	temp_file_codebook <- file.path(tempdir(), "codebook.pdf")
	cat(paste0("Creating codebook file at location: ", temp_file_codebook), sep = "\n")
	args$outfile <- temp_file_codebook
	create_codebook(args, REF_STATIC_DIR, POSTGRES_TABLES_DIR, LOCAL)

	# Generate zip file
	cat(paste0("Creating zip file at location: ", OUTFILE), sep = "\n")
	zip(OUTFILE, 
		files = c(temp_file_data, temp_file_codebook,
			tools::file_path_as_absolute(file.path(REF_STATIC_DIR, "methodology.pdf"))), 
		flags = "-j")
	cat(paste0("File available at ", OUTFILE), sep = "\n")
}