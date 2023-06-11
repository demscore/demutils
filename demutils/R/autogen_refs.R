
codebook_load_data <- function(args, POSTGRES_TABLES_DIR, LOCAL) {
	DJANGO_DB_HOST_DATA <- args$DJANGO_DB_HOST_DATA
	DJANGO_DB_NAME_DATA <- args$DJANGO_DB_NAME_DATA
	DJANGO_DB_PASSWORD_DATA <- args$DJANGO_DB_PASSWORD_DATA
	DJANGO_DB_USER_DATA <- args$DJANGO_DB_USER_DATA

	# Load meta data
	# If LOCAL is FALSE then load meta data from database!
	if (isTRUE(!LOCAL)) {

		creds <- c(host = DJANGO_DB_HOST_DATA, 
					dbname = DJANGO_DB_NAME_DATA,
					user = DJANGO_DB_USER_DATA,
					password = DJANGO_DB_PASSWORD_DATA,
					port = 5432)
		db <- do.call(DBI::dbConnect, c(drv = RPostgreSQL::PostgreSQL(max.con = 50), 
		            creds))

		# Load database tables
		# Read variable information
		cb_section <- dplyr::tbl(db, "cb_section") %>% dplyr::collect(n = Inf)
		datasets <- dplyr::tbl(db, "datasets") %>% dplyr::collect(n = Inf)
		units <- dplyr::tbl(db, "units") %>% dplyr::collect(n = Inf)
		projects <- dplyr::tbl(db, "projects") %>% dplyr::collect(n = Inf)
		unit_variables <- dplyr::tbl(db, "unit_variables") %>% dplyr::collect(n = Inf)
		thematic_datasets <- dplyr::tbl(db, "thematic_datasets") %>% 
			dplyr::collect(n = Inf)
		thematic_datasets_variables <- dplyr::tbl(db, "thematic_datasets_variables") %>% 
			dplyr::collect(n = Inf)
		merge_scores <- dplyr::tbl(db, "merge_scores") %>% dplyr::collect(n = Inf)
		
		variables <- dplyr::tbl(db, "variables") %>% dplyr::collect(n = Inf)
		
	
		DBI::dbDisconnect(db)
	} else {
		# When not using a database
		cb_section <- readRDS(file.path(POSTGRES_TABLES_DIR, "cb_section.rds"))
		datasets <- readRDS(file.path(POSTGRES_TABLES_DIR, "datasets.rds"))
		units <- readRDS(file.path(POSTGRES_TABLES_DIR, "units.rds"))
		projects <- readRDS(file.path(POSTGRES_TABLES_DIR, "projects.rds"))
		unit_variables <- readRDS(file.path(POSTGRES_TABLES_DIR, 
										"unit_variables.rds"))
		variables <- readRDS(file.path(POSTGRES_TABLES_DIR, "variables.rds"))
		thematic_datasets <- readRDS(file.path(POSTGRES_TABLES_DIR, 
											"thematic_datasets.rds"))
		thematic_datasets_variables <- readRDS(file.path(POSTGRES_TABLES_DIR, 
											"thematic_datasets_variables.rds"))
		merge_scores <- readRDS(file.path(POSTGRES_TABLES_DIR, "merge_scores.rds"))

	}


	# We create a named list that is the first argument to codebook_filter_variables
	return(list(variables = variables, cb_section = cb_section, datasets = datasets, 
		units = units, projects = projects, unit_variables = unit_variables,
		thematic_datasets = thematic_datasets, 
		thematic_datasets_variables = thematic_datasets_variables, 
		merge_scores = merge_scores))
}


codebook_filter_variables <- function(ll, args, mode = "variables") {
	VARIABLES_LONG_TAGS <- args$variables
	stopifnot(mode %in% c("variables", "dataset", "project", "thematic"))

	if (mode == "variables") {
		ll$variables <- ll$variables %>% dplyr::filter(tag_long %in% VARIABLES_LONG_TAGS)
	}

	if (mode == "dataset") {
		ll$variables <- ll$variables %>% dplyr::filter(dataset_tag == args$dataset)
	}

	if (mode == "project") {
		ll$variables <- ll$variables %>% dplyr::filter(project_short == args$project)
	}

	if (mode == "thematic") {
		# Get data and arguments
		thematic_datasets <- ll$thematic_datasets
		thematic_datasets_variables <- ll$thematic_datasets_variables
		THEME <- args$thematic

		# Filter variables
		thematic_datasets %<>% dplyr::filter(tag == THEME)
		thematic_datasets_variables %<>% 
			dplyr::filter(thematic_dataset_id %in% 
						  thematic_datasets$thematic_dataset_id)
		ll$variables <- ll$variables %>% 
			dplyr::filter(tag_long %in% 
						  thematic_datasets_variables$tag_long)
	}

	return(ll)
}



codebook_prepare_data <- function(ll, args, include_unit_variables = TRUE) {
	# This function calls tables and creates the .tex files for reference documents
  variables <- ll$variables
	cb_section <- ll$cb_section
	datasets <- ll$datasets
	units <- ll$units
	projects <- ll$projects
	unit_variables <- ll$unit_variables
	merge_scores <- ll$merge_scores
	
	
	# Arguments
	DOWNLOAD_ID <- args$download_id
	

	# Main data.frame from variables
	df <- variables

	variable_template <- c(
	"\\paragraph{ {{name}} ({{tag_adjusted}})} \\mbox{}\n\n",
	
	"\\hangindent=1cm\n\\emph{Long tag}: {{tag_long_adjusted}}\n\n", 
	
	"\\hangindent=1cm\n\\emph{Original tag}: {{original_tag_adjusted}}\n\n",
	
	"{{#dataset_citation}}",
	"\\hangindent=1cm\n\\emph{Dataset citation}: {{dataset_citation}}\n\n",
	"{{/dataset_citation}}",
	
	"{{#citation}}",
	"\\hangindent=1cm\n\\emph{Variable citation}: {{citation}}\n\n",
	"{{/citation}}",
	
	"{{#merge_id}}",
	"\\hangindent=1cm\n\\emph{Merge scores}: \n\n",
	"{{/merge_id}}",
	
	"{{#obs_sum_org}}",
	"\\hangindent=1cm\n\\emph{Non-missing observations in original unit}: Sum: {{obs_sum_org}}, Percent: {{obs_percent_org}}\n\n",
	"{{/obs_sum_org}}",
	
	"{{#end_matched_sum}}",
	"\\hangindent=1cm\n\\emph{Non-missing observations in chosen unit}: Sum: {{end_matched_sum}}, Percent: {{end_matched_percent}}\n\n",
	"{{/end_matched_sum}}",
	
	"{{#lost_obs_sum}}",
	"\\hangindent=1cm\n\\emph{Lost observations in chosen unit}: Sum: {{lost_obs_sum}} Percent: {{lost_obs_percent}}\n\n",
	"{{/lost_obs_sum}}",
	
	
	"\\hangindent=1cm\n\\emph{Description}: \\\\ {{cb_entry}} \n\n"
	)

	if (!is.null(args$output_unit_tag)) {
	  merge_scores %<>% 
	    dplyr::filter(unit_tag == args$output_unit_tag) %>%
	    dplyr::select(merge_id, tag_long, obs_percent_org, obs_sum_org, end_matched_sum, 
	           end_matched_percent, lost_obs_percent, lost_obs_sum)
	  
	  df <- dplyr::left_join(df, merge_scores, by = c("tag_long"))
	  
	}
	
	
	# Loop over project, dataset, variable
	# We want to order the datasets!
	# We also want to order by output units!

	# Prepare variable entries
	df %<>% 
		dplyr::arrange(project_short, dataset_tag, variable_id) %>%
		dplyr::mutate(
			tag_adjusted = ifelse(grepl("^vdem_", tag_long),
				gsub("_[01]$", "", tag),
				tag),
			tag_long_adjusted = ifelse(grepl("^vdem_", tag_long),
				gsub("_[01]$", "", tag_long),
				tag_long),
			original_tag_adjusted = ifelse(grepl("^vdem_", tag_long),
				gsub("_[01]$", "", original_tag),
				original_tag))
			
	tex_variables_entries <- 
		render_snippet(df, variable_template)

	df %<>%	
		dplyr::mutate(final_variable_entry = tex_variables_entries %>%
			gsub("\\\\\\\\$", "", .) %>%
			gsub("\n+$", "", .) %>%
			gsub("\\\\\\\\$", "", .) %>%
			gsub("\n+$", "", .) %>%
			gsub("\\\\\\\\$", "", .))
	
	# Reduce to cb_section level
	dff <- 
		df %>%
		dplyr::select(project_short, dataset_tag, cb_section, variable_id, 
			   final_variable_entry) %>%
		dplyr::mutate(id = factor(paste(dataset_tag, cb_section, sep = "_"), 
						   levels = paste(dataset_tag, cb_section, sep = "_") %>% unique) %>% 
			as.integer) %>%
		dplyr::group_by(dataset_tag, cb_section) %>%
		dplyr::arrange(variable_id) %>%
		dplyr::summarize(
			id = dplyr::first(id),
			project_short = dplyr::first(project_short),
			dataset_tag = dplyr::first(dataset_tag),
			cb_section_entry = paste(final_variable_entry, collapse = "")) %>%
		dplyr::ungroup() %>%
		dplyr::arrange(id) %>%
		dplyr::select(-id)

	# Prepare cb_section intros
	cb_section_intros_df <- 
		cb_section %>%
		dplyr::select(cb_section = cb_section_tag,
			   dataset_tag, 
			   cb_section_intro,
			   cb_section_name) %>%
		dplyr::mutate(cb_section_intro = paste0("\\subsubsection{", cb_section_name, "}", "\n", cb_section_intro)) %>%
		dplyr::select(-cb_section_name)

	# Prepend cb_section intros
	dff %<>%
		dplyr::left_join(cb_section_intros_df, by = c("dataset_tag", "cb_section")) %>%
		dplyr::mutate(cb_section_entry = 
			paste(cb_section_intro, cb_section_entry, sep = "\n"))

	# Reduce to dataset level
	dff %<>%
		dplyr::select(project_short, dataset_tag, cb_section_entry) %>%
		dplyr::group_by(dataset_tag) %>%
		dplyr::summarize(
			project_short = dplyr::first(project_short),
			dataset_tag = dplyr::first(dataset_tag),
			dataset_entry = paste(cb_section_entry, collapse = ""))

	# Prepare dataset intros
	dataset_df <- 
		datasets %>%
		dplyr::mutate(cb_entry  = gsub("\n", "\\\\\\\\\n", cb_entry)) %>% 
		dplyr::select(name, dataset_tag = tag, dataset_intro = cb_entry) %>%
		dplyr::mutate(dataset_intro = paste0("\\subsection{", name, "}", 
			"\n\\noindent ", dataset_intro)) %>%
		dplyr::select(-name)

	# Prepend dataset intros
	dff %<>%
		dplyr::left_join(dataset_df, by = "dataset_tag") %>%
		dplyr::mutate(dataset_entry = 
			paste(dataset_intro, dataset_entry, sep = "\n"))


	# Reduce to project level
	dff %<>%
		dplyr::group_by(project_short) %>%
		dplyr::summarize(
			project_short = dplyr::first(project_short),
			project_entry = paste(dataset_entry, collapse = ""))

	# Prepare project intros
	project_df <- projects %>% 
		dplyr::select(project_short, project_intro = cb_entry,
		   project_cb_title = cb_title) %>%
		dplyr::mutate(project_intro = 
			paste0("\\section{", project_cb_title, "}", 
				   "\n\\noindent ", project_intro)) %>%
		dplyr::select(-project_cb_title)

	# Prepend project intros
	dff %<>%
		dplyr::left_join(project_df, by = "project_short") %>%
		dplyr::mutate(project_entry = 
			paste(project_intro, project_entry, sep = "\n"))


	outtex <- dff$project_entry %>% paste(collapse = "\n")

	# Read static files
	intro <- readLines(file.path(REF_STATIC_DIR, "intro.tex"), encoding = "UTF-8")
	outro <- readLines(file.path(REF_STATIC_DIR, "outro.tex"), encoding = "UTF-8")
	intro <- c(intro, DOWNLOAD_ID, "\\n")

	if (isTRUE(include_unit_variables)) { 
		unit_variables %<>% 
			dplyr::arrange(unit_variables_id) %>% 
			dplyr::filter(unit_tag == args$output_unit_tag)
		unit_tex <- 
			paste("\\subsection{Output Unit Identifier Variables in the Chosen Unit}", 
 			     "\n\\noindent ", 
			     paste(unit_variables$tag %^% ": " %^% unit_variables$cb_entry, 
				 	   collapse = "\\\\\\\\"), "\n",
			 sep = "\n")
	} else {
		unit_tex <- NULL
	}

	if (!is.null(args$thematic)) { 
		THEME <- args$thematic
		thematic_cb_entry <- ll$thematic_datasets %>% 
			dplyr::filter(tag == THEME) %$% cb_entry
		
		thematic_tex <- 
			paste("\\subsection{Thematic Dataset}", 
 			     "\n\\noindent ", 
			     thematic_cb_entry, "\n",
			 sep = "\n")
	} else {
		thematic_tex <- NULL
	}

	bibtex <- create_bibtex_entries(variables)

	# Attach intro and outro
	outtex <- c(intro, thematic_tex, unit_tex, outtex, outro)
	return(list(outtex = outtex, bibtex = bibtex))
}

create_bibtex_entries <- function(variables) {
	variables_bibtex <- variables$bibtex
	datasets_bibtex <- variables$dataset_bibtex

	bibtex <- c(variables_bibtex, datasets_bibtex) %>% unique %>%
		paste(., collapse = "\n\n") %>%
		clean_bibtex_duplicates
	return(bibtex)
}

clean_bibtex_duplicates <- function(v) {
	strsplit(v, "@") %>% 
		unlist %>% 
		unique %>%
		paste(., collapse = "@")
}



codebook_build_pdf_from_tex <- function(ll, args, REF_STATIC_DIR) {
	outtex <- ll$outtex
	bibtex <- ll$bibtex
	OUTFILE <- args$outfile
	
	# Create temporary directory
	temp_dir <- tempdir()
	cat(paste0("Creating tempdir for codebook creation: ", temp_dir), sep = "\n")

	# Write autofilled tex and bibtex files in temporary directory
	writeLines(outtex, file.path(temp_dir, "codebook.tex"))
	writeLines(bibtex, file.path(temp_dir, "refs.bib"))
	
	# Copy static codebook files to temporary directory
	file.copy(file.path(REF_STATIC_DIR, "codebookfrontpage.pdf"), 
		file.path(temp_dir, "/codebookfrontpage.pdf"),
		overwrite = TRUE)

	
	# Create pdf from tex file and bibtex file and codebook frontpage
	tex_file <- file.path(temp_dir, "codebook.tex")
	system("latexmk -cd -f -e '$max_repeat=4' -silent -jobname=codebook -pdf" %^% 
		" -pdflatex='pdflatex -interaction=nonstopmode' " %^% 
		tex_file %^%"; cp " %^%
		file.path(temp_dir, "codebook.pdf") %^% " " %^% OUTFILE, intern = TRUE,
		ignore.stdout = TRUE, ignore.stderr = TRUE) %>% invisible %>% quiet

}

create_variable_codebook <- function(args, 
	REF_STATIC_DIR = "~/data/demscore_next_release/autogenerated_refs/static", 
	POSTGRES_TABLES_DIR = "~/data/demscore_next_release/autogenerated_refs/refs_prepped",
	LOCAL = TRUE) {
	
	args %>%
		codebook_load_data(., POSTGRES_TABLES_DIR, LOCAL) %>%
		codebook_filter_variables(., args, mode = "variables") %>%
		codebook_prepare_data(., args) %>%
		codebook_build_pdf_from_tex(., args, REF_STATIC_DIR)
}

create_project_codebook <- function(args, 
	REF_STATIC_DIR = "~/data/demscore_next_release/autogenerated_refs/static", 
	POSTGRES_TABLES_DIR = "~/data/demscore_next_release/autogenerated_refs/refs_prepped",
	LOCAL = TRUE) {
	
	args %>%
		codebook_load_data(., POSTGRES_TABLES_DIR, LOCAL) %>%
		codebook_filter_variables(., args, mode = "project") %>%
		codebook_prepare_data(., args, include_unit_variables = FALSE) %>%
		codebook_build_pdf_from_tex(., args, REF_STATIC_DIR)
}

create_dataset_codebook <- function(args, 
	REF_STATIC_DIR = "~/data/demscore_next_release/autogenerated_refs/static", 
	POSTGRES_TABLES_DIR = "~/data/demscore_next_release/autogenerated_refs/refs_prepped",
	LOCAL = TRUE) {
	
	args %>%
		codebook_load_data(., POSTGRES_TABLES_DIR, LOCAL) %>%
		codebook_filter_variables(., args, mode = "dataset") %>%
		codebook_prepare_data(., args, include_unit_variables = FALSE) %>%
		codebook_build_pdf_from_tex(., args, REF_STATIC_DIR)
}

create_thematic_dataset_codebook <- function(args, REF_STATIC_DIR, 
	POSTGRES_TABLES_DIR, LOCAL) {
	
	args %>%
		codebook_load_data(., POSTGRES_TABLES_DIR, LOCAL) %>%
		codebook_filter_variables(., args, mode = "thematic") %>%
		codebook_prepare_data(., args, include_unit_variables = FALSE) %>%
		codebook_build_pdf_from_tex(., args, REF_STATIC_DIR)
}



#' @title Create codebook
#' @description Creates the demscore codebook from command line arguments
#' and directories containing static codebook files and prepped postgres tables.
#' The appropriate command line arguments and environment variables must be defined.
#' @param args Command line arguments as a list (usually passed on from docopt)
#' \itemize{
#' \item help (flag): Show help message
#' \item include_unit_cols (flag): Include unit table variables
#' \item download_id (value): Download id
#' \item project (value): Project tag
#' \item dataset (value): Dataset tag
#' \item thematic (value): Theme tag
#' \item output_unit_tag (value): Output unit tag
#' \item variables (value): Variable long tags
#' \item outfile (value): Output file
#' \item file_format (value): Output file format (R, CSV, STATA)
#' \item DJANGO_DB_HOST_DATA (value): Django database host
#' \item DJANGO_DB_NAME_DATA (value): Django database name
#' \item DJANGO_DB_USER_DATA (value): Django database user
#' \item DJANGO_DB_PASSWORD_DATA (value): Django database password
#' }
#' @param REF_STATIC_DIR Path to static codebook files
#' @param POSTGRES_TABLES_DIR Path to prepped postgres tables
#' @param LOCAL Logical, TRUE if the function should not connect to a database,
#' FALSE if it should, connection details are passed in `args`
#' @return Returns NULL.
#' @details The codebook can be created in one of four modes, variable selection,
#' project selection, dataset selection and thematic selection. The mode is
#' determined by the command line arguments.
#' @export
create_codebook <- function(args, REF_STATIC_DIR, 
	POSTGRES_TABLES_DIR, LOCAL, prep = FALSE) {

	if (prep) {
		prepare_autogen()
	}

	mode <- dplyr::case_when(
		length(args$variables) > 0L ~ "variables",
		!is.null(args$project) ~ "project",
		!is.null(args$dataset) ~ "dataset",
		!is.null(args$thematic) ~ "thematic"
		#TRUE ~ stop("Unknown mode, check combinations of command line arguments!")
	)

	if (mode == "variables") {
		create_variable_codebook(args, REF_STATIC_DIR, POSTGRES_TABLES_DIR, LOCAL)
	}
	if (mode == "project") {
		create_project_codebook(args, REF_STATIC_DIR, POSTGRES_TABLES_DIR, LOCAL)
	}
	if (mode == "dataset") {
		create_dataset_codebook(args, REF_STATIC_DIR, POSTGRES_TABLES_DIR, LOCAL)
	}
	if (mode == "thematic") {
		create_thematic_dataset_codebook(args, REF_STATIC_DIR, POSTGRES_TABLES_DIR, LOCAL)
	}
}
