#' V-Dem Party-Country-Date to V-Dem Country Year
#' 
#' Aggregates from V-Dem Party-Country-Year to Country Year by only keeping the 
#' observations for the largest party per country-year using the variable v2paseatshare
#' 
#' @param df a data.frame
#' @return data.frame aggregated to country year level
#' @examples 
#' aggregate_vparty_highest_seatshare(df)
#'
#' @export
aggregate_vparty_highest_seatshare <- function(df) {

    df %<>% 
      dplyr::group_by(u_vdem_party_country_year_country_name, 
                      u_vdem_party_country_year_year) %>%
      dplyr::filter(vdem_vparty_v2paseatshare == max(vdem_vparty_v2paseatshare)) %>%
      dplyr::ungroup(.)
    
      return(df)
  
}

### ============================================================================
### New V-Party Aggregation
### ============================================================================
### Prepare Dataframes
### ============================================================================

prep_w_cy <- function(df, ...) {

  df %<>% filter(vdem_vparty_year >= 1970)
  
  # Create variable on whether the party is in opposition (3) or government (0, 1 or 2)
  # 1 = governemnt, 0 = opposition
  df$v2pagovsup_dum <- NA
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 0] <- 1
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 1] <- 1
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 2] <- 1
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 3] <- 0
  
  df$seatshare_1 <- df$vdem_vparty_v2paseatshare*0.01
  df$pavote_1 <- df$vdem_vparty_v2pavote*0.01
  
  df %<>% mutate(vdem_vparty_country_name = case_when(
    vdem_vparty_country_name == 'Czech Republic' ~ 'Czechia',
    TRUE ~ vdem_vparty_country_name
  ))
  
  variables <- tbl(db, "variables") %>% collect(n = Inf)
  datasets <- tbl(db, "datasets") %>% collect(n = Inf)
  df_cy <- read_unit_data("u_vdem_country_year", "vdem_cy",  variables, datasets)
  
  #vdem_cy <- read_datasets("vdem_cy", db)
  df_cy %<>% 
    select(vdem_cy_country_name, vdem_cy_country_text_id, 
           vdem_cy_country_id, vdem_cy_year, starts_with("vdem_cy_v2x_polyarchy"))
  
  out <- df %>% 
    left_join(df_cy, df, by = c("vdem_vparty_country_name" = "vdem_cy_country_name", 
                                "vdem_vparty_country_text_id" = "vdem_cy_country_text_id",
                                "vdem_vparty_country_id" = "vdem_cy_country_id",
                                "vdem_vparty_year" = "vdem_cy_year"))
  
  return(out)
  
}


prep_wo_cy <- function(df) {
  
  df %<>% filter(vdem_vparty_year >= 1970)
  
  # Create variable on whether the party is in opposition (3) or government (0, 1 or 2)
  # 1 = governemnt, 0 = opposition
  df$v2pagovsup_dum <- NA
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 0] <- 1
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 1] <- 1
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 2] <- 1
  df$v2pagovsup_dum[df$vdem_vparty_v2pagovsup == 3] <- 0
  
  df$seatshare_1 <- df$vdem_vparty_v2paseatshare*0.01
  df$pavote_1 <- df$vdem_vparty_v2pavote*0.01
  
  df %<>% mutate(vdem_vparty_country_name = case_when(
    vdem_vparty_country_name == 'Czech Republic' ~ 'Czechia',
    TRUE ~ vdem_vparty_country_name
  ))
  
  out <- df
  
  return(out)
  
}

### ============================================================================
### PSDI
### ============================================================================

create_psdi <- function(df, ...){
  
  psdi_raw <- prep_w_cy(df)
  
  # Creating Party-System Democracy Index 
  psdi_raw %<>%  
    dplyr::select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                  vdem_vparty_country_name, vdem_vparty_year, 
                  v2pagovsup_dum, vdem_vparty_e_regionpol_6c,
                  seatshare_1, vdem_vparty_v2xpa_antiplural, 
                  vdem_cy_v2x_polyarchy) %>%
    dplyr::group_by(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                    vdem_vparty_e_regionpol_6c, vdem_vparty_country_name, 
                    vdem_vparty_year, v2pagovsup_dum, 
                    vdem_cy_v2x_polyarchy
    ) %>% 
    dplyr::summarize(
      wm_autpol = weighted.mean(vdem_vparty_v2xpa_antiplural, 
                                seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  ## PSDI
  # We spread the df creating one column with the weighted antipluralistic mean
  # by government (1) and opposition (0)
  psdi_data <- psdi_raw %>%
    tidyr::spread(v2pagovsup_dum, wm_autpol) 
  
  # rename government and opposition to 0 to 1
  # rescale government and opposition to 0 to 1
  psdi_data$v2xpas_democracy_opposition <- psdi_data$`0`
  # if there is no opposition, then we force a 0 (= opposition is not allowed to run 
  psdi_data$v2xpas_democracy_opposition_adj <- ifelse(is.na(psdi_data$v2xpas_democracy_opposition), 0, 
                                                      psdi_data$v2xpas_democracy_opposition)
  psdi_data$v2xpas_democracy_government <- psdi_data$`1`
  
  # PARTY SYSTEM ONLY
  psdi_data$psdi <- 1-(psdi_data$v2xpas_democracy_government + psdi_data$v2xpas_democracy_opposition_adj)
  
  ## Check this with Fabio
  ## psdi_data$psdi_re <- scales::rescale(psdi_data$psdi, to = c(0, 1))
  psdi_data$v2xpas_democracy <- scales::rescale(psdi_data$psdi, to = c(0, 1))
  
  psdi_data %<>%
    dplyr::mutate(v2xpas_democracy_adj = ifelse(vdem_cy_v2x_polyarchy >.5, v2xpas_democracy, "NA")) %>%
    dplyr::mutate(v2xpas_democracy_adj = ifelse(vdem_cy_v2x_polyarchy <=.5 & 
                                                  psdi_data$v2xpas_democracy_opposition_adj == 0, 
                                                0, v2xpas_democracy)) %>%
    as.data.frame()
  
  # Select and arrange
  psdi_data %<>% select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                        vdem_vparty_country_name, vdem_vparty_year, 
                        vdem_vparty_v2xpas_democracy = v2xpas_democracy_adj, 
                        vdem_vparty_v2xpas_democracy_government = v2xpas_democracy_government,
                        vdem_vparty_v2xpas_democracy_opposition = v2xpas_democracy_opposition_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id)
  
  return(psdi_data)
  
}

create_psdi_codelow <- function(df, ...){
  
  psdi_raw_low <- prep_w_cy(df)
  
  psdi_raw_low %<>% dplyr::select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                                  vdem_vparty_e_regionpol_6c,vdem_vparty_country_name, 
                                  vdem_vparty_year, v2pagovsup_dum, 
                                  vdem_cy_v2x_polyarchy_codelow,
                                  seatshare_1, vdem_vparty_v2xpa_antiplural_codelow) %>%
    dplyr::group_by(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                    vdem_vparty_e_regionpol_6c, vdem_vparty_country_name, 
                    vdem_vparty_year, v2pagovsup_dum,
                    vdem_cy_v2x_polyarchy_codelow) %>% 
    dplyr::summarise(
      wm_autpol_low = weighted.mean(vdem_vparty_v2xpa_antiplural_codelow, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  psdi_data_low <- psdi_raw_low %>%
    tidyr::spread(v2pagovsup_dum, wm_autpol_low) 
  
  # rescale governmment and opposition to 0 to 1
  psdi_data_low$v2xpas_democracy_opposition_codelow <- psdi_data_low$`0`
  
  # if there is no opposition, then we force a 0 (= opposition is not allowed to run)
  
  psdi_data_low$v2xpas_democracy_opposition_codelow_adj <- 
    ifelse(is.na(psdi_data_low$v2xpas_democracy_opposition_codelow), 0,
           psdi_data_low$v2xpas_democracy_opposition_codelow)
  
  psdi_data_low$v2xpas_democracy_government_codelow <- psdi_data_low$`1`
  
  # PARTY SYSTEM ONLY
  psdi_data_low$psdi_low <- 
    1-(psdi_data_low$v2xpas_democracy_government_codelow + psdi_data_low$v2xpas_democracy_opposition_codelow_adj)
  
  psdi_data_low$v2xpas_democracy_codelow <- scales::rescale(psdi_data_low$psdi_low, to = c(0, 1))
  
  psdi_data_low %<>%
    dplyr::mutate(v2xpas_democracy_codelow_adj = 
                    ifelse(vdem_cy_v2x_polyarchy_codelow > .5, v2xpas_democracy_codelow, "NA")) %>%
    dplyr::mutate(v2xpas_democracy_codelow_adj = 
                    ifelse(vdem_cy_v2x_polyarchy_codelow <= .5 & psdi_data_low$v2xpas_democracy_opposition_codelow_adj == 0,
                           0, v2xpas_democracy_codelow)) %>%
    as.data.frame() %>%
    select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
           vdem_vparty_country_name, vdem_vparty_year,
           vdem_vparty_v2xpas_democracy_codelow = v2xpas_democracy_codelow_adj,
           vdem_vparty_v2xpas_democracy_opposition_codelow = v2xpas_democracy_opposition_codelow_adj,
           vdem_vparty_v2xpas_democracy_government_codelow = v2xpas_democracy_government_codelow) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(psdi_data_low)
  
}

create_psdi_codehigh <- function(df, ...){
  
  psdi_raw_high <- prep_w_cy(df)
  
  psdi_raw_high %<>% dplyr::select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                                   vdem_vparty_e_regionpol_6c, vdem_vparty_country_name, 
                                   vdem_vparty_year, v2pagovsup_dum, 
                                   vdem_cy_v2x_polyarchy_codehigh, seatshare_1, 
                                   vdem_vparty_v2xpa_antiplural_codehigh) %>%
    dplyr::group_by(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                    vdem_vparty_e_regionpol_6c, vdem_vparty_country_name, 
                    vdem_vparty_year, v2pagovsup_dum,
                    vdem_cy_v2x_polyarchy_codehigh) %>% 
    dplyr::summarise(
      wm_autpol_high = weighted.mean(vdem_vparty_v2xpa_antiplural_codehigh, 
                                     seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  psdi_data_high <- psdi_raw_high %>%
    tidyr::spread(v2pagovsup_dum, wm_autpol_high) 
  
  # rescale governmment and opposition to 0 to 1
  psdi_data_high$v2xpas_democracy_opposition_codehigh <- psdi_data_high$`0`
  
  # if there is no opposition, then we force a 0 (= opposition is not allowed to run)
  
  psdi_data_high$v2xpas_democracy_opposition_codehigh_adj <- 
    ifelse(is.na(psdi_data_high$v2xpas_democracy_opposition_codehigh), 0,
           psdi_data_high$v2xpas_democracy_opposition_codehigh)
  
  psdi_data_high$v2xpas_democracy_government_codehigh <- psdi_data_high$`1`
  
  # PARTY SYSTEM ONLY
  psdi_data_high$psdi_high <- 
    1-(psdi_data_high$v2xpas_democracy_government_codehigh + psdi_data_high$v2xpas_democracy_opposition_codehigh_adj)
  
  psdi_data_high$v2xpas_democracy_codehigh <- scales::rescale(psdi_data_high$psdi_high, to = c(0, 1))
  
  psdi_data_high %<>%
    dplyr::mutate(v2xpas_democracy_codehigh_adj = ifelse(vdem_cy_v2x_polyarchy_codehigh > .5, v2xpas_democracy_codehigh, "NA")) %>%
    dplyr::mutate(v2xpas_democracy_codehigh_adj = ifelse(vdem_cy_v2x_polyarchy_codehigh <= .5 & 
                                                           psdi_data_high$v2xpas_democracy_opposition_codehigh_adj == 0,
                                                         0, v2xpas_democracy_codehigh)) %>%
    as.data.frame() %>%
    select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
           vdem_vparty_country_name, vdem_vparty_year,
           vdem_vparty_v2xpas_democracy_codehigh = v2xpas_democracy_codehigh_adj,
           vdem_vparty_v2xpas_democracy_opposition_codehigh = v2xpas_democracy_opposition_codehigh_adj,
           vdem_vparty_v2xpas_democracy_government_codehigh = v2xpas_democracy_government_codehigh) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(psdi_data_high)
  
}

### ============================================================================
### PSLRI
### ============================================================================

create_pslri <- function(df, ...){
  
  lrps_raw <- prep_wo_cy(df)
  
  lrps_raw %<>% dplyr::select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                              vdem_vparty_country_name, vdem_vparty_year, 
                              v2pagovsup_dum, seatshare_1, vdem_vparty_v2pariglef) %>%
    dplyr::group_by(vdem_vparty_country_id, vdem_vparty_country_text_id,
                    vdem_vparty_country_name, vdem_vparty_year, v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_lr = weighted.mean(vdem_vparty_v2pariglef, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  lr_data <- lrps_raw %>%
    tidyr::spread(v2pagovsup_dum, wm_lr) 
  
  lr_data$v2xpas_economic_opposition <- lr_data$`0`
  lr_data$v2xpas_economic_opposition_adj <- ifelse(is.na(lr_data$v2xpas_economic_opposition), 0,
                                                   lr_data$v2xpas_economic_opposition)
  lr_data$v2xpas_economic_government <- lr_data$`1`
  lr_data$lrps <- lr_data$v2xpas_economic_government + lr_data$v2xpas_economic_opposition_adj
  lr_data$v2xpas_economic <- scales::rescale(lr_data$lrps, to = c(0, 1))
  
  # drop Lithuania because it's weird
  lr_data <- lr_data[lr_data$vdem_vparty_country_name != "Lithuania", ]
  
  # codebook is missing economic_opposition
  lr_data %<>% select(vdem_vparty_country_id, vdem_vparty_country_text_id, 
                      vdem_vparty_country_name, vdem_vparty_year,
                      vdem_vparty_v2xpas_economic = v2xpas_economic,
                      vdem_vparty_v2xpas_economic_government = v2xpas_economic_government,
                      vdem_vparty_v2xpas_economic_opposition = v2xpas_economic_opposition_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_text_id, -vdem_vparty_country_id)
  
  return(lr_data)
  
}  

create_pslri_codelow <- function(df, ...) {
  
  lrps_raw_low <- prep_wo_cy(df)
  
  lrps_raw_low %<>% dplyr::select(vdem_vparty_country_name, vdem_vparty_year, 
                                  vdem_vparty_country_text_id, 
                                  vdem_vparty_country_id, v2pagovsup_dum,
                                  seatshare_1, vdem_vparty_v2pariglef_codelow) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_year, 
                    vdem_vparty_country_text_id, vdem_vparty_country_id, 
                    v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_lrps_low = weighted.mean(vdem_vparty_v2pariglef_codelow, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  lrps_data_low <- lrps_raw_low %>%
    tidyr::spread(v2pagovsup_dum, wm_lrps_low) 
  
  # rescale governmment and opposition to 0 to 1
  lrps_data_low$v2xpas_economic_opposition_codelow <- lrps_data_low$`0`
  
  lrps_data_low$v2xpas_economic_opposition_codelow_adj <- 
    ifelse(is.na(lrps_data_low$v2xpas_economic_opposition_codelow), 0, lrps_data_low$v2xpas_economic_opposition_codelow)
  
  lrps_data_low$v2xpas_economic_government_codelow <- lrps_data_low$`1`
  
  lrps_data_low$lrps_low <-  lrps_data_low$v2xpas_economic_government_codelow + 
    lrps_data_low$v2xpas_economic_opposition_codelow_adj
  
  lrps_data_low$v2xpas_economic_codelow <- scales::rescale(lrps_data_low$lrps_low, to=c(0,1))
  
  lrps_data_low <- lrps_data_low[lrps_data_low$vdem_vparty_country_name != "Lithuania", ]
  
  lrps_data_low %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                            vdem_vparty_v2xpas_economic_codelow = v2xpas_economic_codelow, 
                            vdem_vparty_v2xpas_economic_opposition_codelow = v2xpas_economic_opposition_codelow_adj,
                            vdem_vparty_v2xpas_economic_government_codelow = v2xpas_economic_government_codelow) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(lrps_data_low)
  
}

create_pslri_codehigh <- function(df, ...) {
  
  lrps_raw_high  <- prep_wo_cy(df)
  
  lrps_raw_high %<>% dplyr::select(vdem_vparty_country_name, vdem_vparty_year, 
                                   vdem_vparty_country_text_id, vdem_vparty_country_id, 
                                   v2pagovsup_dum, seatshare_1, 
                                   vdem_vparty_v2pariglef_codehigh) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_year, 
                    vdem_vparty_country_text_id, vdem_vparty_country_id, 
                    v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_lrps_high = weighted.mean(vdem_vparty_v2pariglef_codehigh, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  lrps_data_high <- lrps_raw_high %>%
    tidyr::spread(v2pagovsup_dum, wm_lrps_high) 
  
  # rescale governmment and opposition to 0 to 1
  lrps_data_high$v2xpas_economic_opposition_codehigh <- lrps_data_high$`0`
  
  lrps_data_high$v2xpas_economic_opposition_codehigh_adj <- 
    ifelse(is.na(lrps_data_high$v2xpas_economic_opposition_codehigh), 0, lrps_data_high$v2xpas_economic_opposition_codehigh)
  
  lrps_data_high$v2xpas_economic_government_codehigh <- lrps_data_high$`1`
  
  lrps_data_high$lrps_high <-  lrps_data_high$v2xpas_economic_government_codehigh + 
    lrps_data_high$v2xpas_economic_opposition_codehigh_adj
  
  lrps_data_high$v2xpas_economic_codehigh <- scales::rescale(lrps_data_high$lrps_high, to=c(0,1))
  
  lrps_data_high <- lrps_data_high[lrps_data_high$vdem_vparty_country_name != "Lithuania", ]
  
  lrps_data_high %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                             vdem_vparty_v2xpas_economic_codehigh = v2xpas_economic_codehigh, 
                             vdem_vparty_v2xpas_economic_government_codehigh = v2xpas_economic_government_codehigh, 
                             vdem_vparty_v2xpas_economic_opposition_codehigh = v2xpas_economic_opposition_codehigh_adj)%>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(lrps_data_high)
  
}

### ============================================================================
### PSREI
### ============================================================================

create_psrei <- function(df, ...) {
  
  relps_raw <- prep_wo_cy(df)
  
  relps_raw %<>% dplyr::select(vdem_vparty_country_text_id, 
                               vdem_vparty_country_id, vdem_vparty_country_name, 
                               vdem_vparty_year, v2pagovsup_dum, seatshare_1,
                               vdem_vparty_v2parelig) %>%
    dplyr::group_by(vdem_vparty_country_name, 
                    vdem_vparty_country_text_id, vdem_vparty_country_id, 
                    vdem_vparty_year, v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_rel = weighted.mean(vdem_vparty_v2parelig, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  # we spread the df creating one column with the weighted antipluralistic mean
  # by government (1) and opposition (0)
  relps_data <- relps_raw %>%
    tidyr::spread(v2pagovsup_dum, wm_rel) 
  
  # rescale governmment and opposition to 0 to 1
  relps_data$v2xpas_religion_opposition <- relps_data$`0`
  
  relps_data$v2xpas_religion_opposition_adj <- ifelse(is.na(relps_data$v2xpas_religion_opposition), 0,
                                                      relps_data$v2xpas_religion_opposition)
  relps_data$v2xpas_religion_government <- relps_data$`1`
  
  relps_data$relps <- relps_data$v2xpas_religion_government + relps_data$v2xpas_religion_opposition_adj
  relps_data$relps_re <- scales::rescale(relps_data$relps, to = c(0, 1))
  
  # reversing the direction of it, now 1 is high religion and 0 no religion
  relps_data$v2xpas_religion <- 1-relps_data$relps_re 
  
  relps_data %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                         vdem_vparty_v2xpas_religion = v2xpas_religion, 
                         vdem_vparty_v2xpas_religion_government = v2xpas_religion_government,
                         vdem_vparty_v2xpas_religion_opposition = v2xpas_religion_opposition_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_text_id, -vdem_vparty_country_id,
           -vdem_vparty_country_name, -vdem_vparty_year)
  
  return(relps_data)
  
}

create_psrei_codelow <- function(df, ...){
  
  relps_raw_low <- prep_wo_cy(df)
  
  relps_raw_low %<>% dplyr::select(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                                   vdem_vparty_country_id, vdem_vparty_year, 
                                   v2pagovsup_dum, seatshare_1, 
                                   vdem_vparty_v2parelig_codelow) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                    vdem_vparty_country_id, vdem_vparty_year, v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_relps_low = weighted.mean(vdem_vparty_v2parelig_codelow, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  relps_data_low <- relps_raw_low %>%
    tidyr::spread(v2pagovsup_dum, wm_relps_low) 
  
  # rescale governmment and opposition to 0 to 1
  relps_data_low$v2xpas_religion_opposition_codelow <- relps_data_low$`0`
  
  relps_data_low$v2xpas_religion_opposition_codelow_adj <- 
    ifelse(is.na(relps_data_low$v2xpas_religion_opposition_codelow), 0, 
           relps_data_low$v2xpas_religion_opposition_codelow)
  
  relps_data_low$v2xpas_religion_government_codelow <- relps_data_low$`1`
  
  relps_data_low$relps_low <- relps_data_low$v2xpas_religion_government_codelow + 
    relps_data_low$v2xpas_religion_opposition_codelow_adj
  
  relps_data_low$relps_low_re <- scales::rescale(relps_data_low$relps_low, to = c(0, 1))
  
  # reversing the direction of it, now 1 is high religion and 0 no religion
  relps_data_low$v2xpas_religion_codelow <- 1-relps_data_low$relps_low_re 
  
  relps_data_low %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                             vdem_vparty_v2xpas_religion_codelow = v2xpas_religion_codelow, 
                             vdem_vparty_v2xpas_religion_government_codelow = v2xpas_religion_government_codelow,
                             vdem_vparty_v2xpas_religion_opposition_codelow = v2xpas_religion_opposition_codelow_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(relps_data_low)
  
}

create_psrei_codehigh <- function(df, ...){
  
  relps_raw_high <- prep_wo_cy(df)
  
  relps_raw_high %<>% dplyr::select(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                                    vdem_vparty_country_id, vdem_vparty_year, 
                                    v2pagovsup_dum, seatshare_1, vdem_vparty_v2parelig_codehigh) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                    vdem_vparty_country_id, vdem_vparty_year, v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_relps_high = weighted.mean(vdem_vparty_v2parelig_codehigh, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  relps_data_high <- relps_raw_high %>%
    tidyr::spread(v2pagovsup_dum, wm_relps_high) 
  
  # rescale governmment and opposition to 0 to 1
  relps_data_high$v2xpas_religion_opposition_codehigh <- relps_data_high$`0`
  
  relps_data_high$v2xpas_religion_opposition_codehigh_adj <- 
    ifelse(is.na(relps_data_high$v2xpas_religion_opposition_codehigh), 0, 
           relps_data_high$v2xpas_religion_opposition_codehigh)
  
  relps_data_high$v2xpas_religion_government_codehigh <- relps_data_high$`1`
  
  relps_data_high$relps_high <- relps_data_high$v2xpas_religion_government_codehigh + 
    relps_data_high$v2xpas_religion_opposition_codehigh_adj
  
  relps_data_high$relps_high_re <- scales::rescale(relps_data_high$relps_high, to = c(0, 1))
  
  # reversing the direction of it, now 1 is high religion and 0 no religion
  relps_data_high$v2xpas_religion_codehigh <- 1-relps_data_high$relps_high_re 
  
  relps_data_high %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                              vdem_vparty_v2xpas_religion_codehigh = v2xpas_religion_codehigh, 
                              vdem_vparty_v2xpas_religion_government_codehigh = v2xpas_religion_government_codehigh,
                              vdem_vparty_v2xpas_religion_opposition_codehigh = v2xpas_religion_opposition_codehigh_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(relps_data_high)
  
}


### ============================================================================
### PSEXI
### ============================================================================

create_psexi <- function(df, ...){
  
  etps_raw <- prep_wo_cy(df)
  
  etps_raw$eth_policy <- 2*(etps_raw$vdem_vparty_v2paculsup) + 
    0.5*(etps_raw$vdem_vparty_v2paimmig + etps_raw$vdem_vparty_v2pawomlab)
  
  etps_raw %<>% dplyr::select(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                              vdem_vparty_country_id, vdem_vparty_year, 
                              v2pagovsup_dum, seatshare_1, eth_policy) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                    vdem_vparty_country_id, vdem_vparty_year, 
                    v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_et = weighted.mean(eth_policy, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  ## etps ###
  # we spread the df creating one column with the weighted antipluralistic mean
  # by government (1) and opposition (0)
  etps_data <- etps_raw %>%
    tidyr::spread(v2pagovsup_dum, wm_et) 
  
  # rescale governmment and opposition to 0 to 1
  etps_data$v2xpas_exclusion_opposition <- etps_data$`0`
  
  etps_data$v2xpas_exclusion_opposition_adj <- ifelse(is.na(etps_data$v2xpas_exclusion_opposition), 0,
                                                      etps_data$v2xpas_exclusion_opposition)
  
  etps_data$v2xpas_exclusion_government <- etps_data$`1`
  
  etps_data$etps <-  etps_data$v2xpas_exclusion_government+etps_data$v2xpas_exclusion_opposition_adj
  
  etps_data$etps_re <- scales::rescale(etps_data$etps, to = c(0, 1))
  
  etps_data$v2xpas_exclusion <- 1-etps_data$etps_re
  
  etps_data %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                        vdem_vparty_v2xpas_exclusion = v2xpas_exclusion,
                        vdem_vparty_v2xpas_exclusion_government = v2xpas_exclusion_government, 
                        vdem_vparty_v2xpas_exclusion_opposition = v2xpas_exclusion_opposition_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_text_id, -vdem_vparty_country_id,
           -vdem_vparty_country_name, -vdem_vparty_year)
  
  return(etps_data)
  
}

create_psexi_codelow <- function(df, ...) {
  
  etps_raw <- prep_wo_cy(df)
  
  # Creating the Exclusion Dimension of political parties ##
  etps_raw$eth_policy_codelow <- 
    2*( etps_raw$vdem_vparty_v2paculsup_codelow) + 0.5*(etps_raw$vdem_vparty_v2paimmig_codelow + 
                                                          etps_raw$vdem_vparty_v2pawomlab_codelow)
  
  etps_raw_low <- etps_raw %>% 
    dplyr::select(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                  vdem_vparty_country_id, vdem_vparty_year, 
                  v2pagovsup_dum, seatshare_1, eth_policy_codelow) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                    vdem_vparty_country_id, vdem_vparty_year, 
                    v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_etps_low = weighted.mean(eth_policy_codelow, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  etps_data_low <- etps_raw_low %>%
    tidyr::spread(v2pagovsup_dum, wm_etps_low) 
  
  # rescale governmment and opposition to 0 to 1
  etps_data_low$v2xpas_exclusion_opposition_codelow <- etps_data_low$`0`
  
  etps_data_low$v2xpas_exclusion_opposition_codelow_adj <- 
    ifelse(is.na(etps_data_low$v2xpas_exclusion_opposition_codelow), 0, 
           etps_data_low$v2xpas_exclusion_opposition_codelow)
  
  etps_data_low$v2xpas_exclusion_government_codelow <- etps_data_low$`1`
  
  etps_data_low$etps_low <- etps_data_low$v2xpas_exclusion_government_codelow + 
    etps_data_low$v2xpas_exclusion_opposition_codelow_adj
  
  etps_data_low$etps_low_re <- scales::rescale(etps_data_low$etps_low, to = c(0, 1))
  etps_data_low$v2xpas_exclusion_codelow <-1-etps_data_low$etps_low_re
  
  etps_data_low %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                            vdem_vparty_v2xpas_exclusion_codelow = v2xpas_exclusion_codelow,
                            vdem_vparty_v2xpas_exclusion_government_codelow = v2xpas_exclusion_government_codelow, 
                            vdem_vparty_v2xpas_exclusion_opposition_codelow = v2xpas_exclusion_opposition_codelow_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(etps_data_low)
  
}

create_psexi_codehigh <- function(df, ...) {
  
  etps_raw <- prep_wo_cy(df)
  
  # Creating the Exclusion Dimension of political parties ##
  etps_raw$eth_policy_codehigh <- 
    2*( etps_raw$vdem_vparty_v2paculsup_codehigh) + 0.5*(etps_raw$vdem_vparty_v2paimmig_codehigh + 
                                                           etps_raw$vdem_vparty_v2pawomlab_codehigh)
  
  etps_raw_high <- etps_raw %>% 
    dplyr::select(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                  vdem_vparty_country_id, vdem_vparty_year, v2pagovsup_dum, 
                  seatshare_1, eth_policy_codehigh) %>%
    dplyr::group_by(vdem_vparty_country_name, vdem_vparty_country_text_id, 
                    vdem_vparty_country_id, vdem_vparty_year, 
                    v2pagovsup_dum) %>% 
    dplyr::summarise(
      wm_etps_high = weighted.mean(eth_policy_codehigh, seatshare_1),
      .groups = 'drop') %>%
    as.data.frame()
  
  etps_data_high <- etps_raw_high %>%
    tidyr::spread(v2pagovsup_dum, wm_etps_high) 
  
  # rescale governmment and opposition to 0 to 1
  etps_data_high$v2xpas_exclusion_opposition_codehigh <- etps_data_high$`0`
  
  etps_data_high$v2xpas_exclusion_opposition_codehigh_adj <- 
    ifelse(is.na(etps_data_high$v2xpas_exclusion_opposition_codehigh), 0, 
           etps_data_high$v2xpas_exclusion_opposition_codehigh)
  
  etps_data_high$v2xpas_exclusion_government_codehigh <- etps_data_high$`1`
  
  etps_data_high$etps_high <- etps_data_high$v2xpas_exclusion_government_codehigh + 
    etps_data_high$v2xpas_exclusion_opposition_codehigh_adj
  
  etps_data_high$etps_high_re <- scales::rescale(etps_data_high$etps_high, to = c(0, 1))
  etps_data_high$v2xpas_exclusion_codehigh <-1-etps_data_high$etps_high_re
  
  etps_data_high %<>% select(starts_with("vdem_vparty_country"), vdem_vparty_year, 
                             vdem_vparty_v2xpas_exclusion_codehigh = v2xpas_exclusion_codehigh,
                             vdem_vparty_v2xpas_exclusion_government_codehigh = v2xpas_exclusion_government_codehigh, 
                             vdem_vparty_v2xpas_exclusion_opposition_codehigh = v2xpas_exclusion_opposition_codehigh_adj) %>%
    arrange(vdem_vparty_country_id, vdem_vparty_year) %>%
    select(-vdem_vparty_country_id, -vdem_vparty_country_text_id,
           -vdem_vparty_year, -vdem_vparty_country_name)
  
  return(etps_data_high)
  
}