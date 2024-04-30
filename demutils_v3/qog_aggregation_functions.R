#' QoG Agency Instruction to Year
#' 
#' Aggregates from qog_agency_inst to a year level that can then be translated to country-year.
#' Groups by year using the year of the agency instruction and calculates the sum of all boolean
#' variables that have the value 1 in the given year. Hence, the output dataframe is showing how many
#' agencies were affected by instructions that are e.g. laws.
#' 
#' @param df a data.frame
#' @return data.frame aggregated to year level for boolean variables 
#' @examples 
#' agency_inst_to_year(df)
#'
#' @export
agency_inst_to_year <- function(df) {

    df %<>%
      select(qog_qad_inst_agency_law, qog_qad_inst_agency_unitary, qog_qad_inst_agency_board, 
             qog_qad_inst_agency_brepun, qog_qad_inst_agency_brepoth, qog_qad_inst_agency_bchair, 
             qog_qad_inst_agency_committee, qog_qad_inst_agency_crepun, qog_qad_inst_agency_crepun,
             qog_qad_inst_agency_crepoth, qog_qad_inst_agency_cchair, qog_qad_inst_agency_council, 
             qog_qad_inst_agency_acrepbu, qog_qad_inst_agency_crepun, qog_qad_inst_agency_acrepoth, 
             qog_qad_inst_agency_acchair, qog_qad_inst_agency_adjud, qog_qad_inst_agency_collab, 
             qog_qad_inst_agency_county, qog_qad_inst_agency_edu, qog_qad_inst_agency_eval, 
             qog_qad_inst_agency_info, qog_qad_inst_agency_permit, qog_qad_inst_agency_police, 
             qog_qad_inst_agency_policy, qog_qad_inst_agency_redist, qog_qad_inst_agency_report, 
             qog_qad_inst_agency_research, qog_qad_inst_agency_rule, qog_qad_inst_agency_super, 
         u_qog_agency_inst_agency_instruction) 

    df$u_qog_country_year_year <- as.integer(substr(df$u_qog_agency_inst_agency_instruction, 1, 4))

    df[, 1:28][df[, 1:28] == 9999] <- NA

    df %<>%
      select(-u_qog_agency_inst_agency_instruction) %>%
      group_by(u_qog_country_year_year) %>%
      summarize(across(.cols = everything(), .fns = ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum")) %>%
      ungroup(.)

}



#' QoG Agency Fiscal Year to Year
#' 
#' Aggregates from qog_agency_year to a year level by summing up the budget variables for each year.
#' 
#' @param df a data.frame
#' @return data.frame aggregated to year level for budget variables 
#' @examples 
#' agency_bud_to_year_sum(df)
#' @export
agency_bud_to_year_sum <- function(df) {

df %<>%
  select(qog_qad_bud_budget_ibudget, qog_qad_bud_budget_fbudget, qog_qad_bud_budget_obudget, 
         qog_qad_bud_budget_amend, qog_qad_bud_budget_withdr, qog_qad_bud_budget_reserv, 
         qog_qad_bud_budget_overrun, qog_qad_bud_budget_overrunef, qog_qad_bud_budget_overrunsb,
         qog_qad_bud_budget_saving, qog_qad_bud_budget_savingef, qog_qad_bud_budget_savingsb, 
         qog_qad_bud_budget_credit, u_qog_agency_year_agency_fy) 

df$u_qog_country_year_year <- as.integer(df$u_qog_agency_year_agency_fy)

df %<>%
  group_by(u_qog_agency_year_agency_fy) %>%
  summarize(across(.cols = everything(), .fns = ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"))

}