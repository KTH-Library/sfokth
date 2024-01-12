#' Number of publications by year from DiVA like publication list
#'
#' @param data publication list
#' @param startyear the first publicaiton year to consider
#' @param stopyear the last publication year to consider
#' @export
npubs_diva <- function(data, startyear, stopyear){

  if(missing(startyear))
    startyear <- suppressWarnings(min(data$Doc_Year, na.rm = TRUE))
  if(missing(stopyear))
    stopyear <- suppressWarnings(max(data$Doc_Year, na.rm = TRUE))

  data <- data |>
    filter(Doc_Year %in% startyear:stopyear) |>
    mutate(Publication_Year = as.character(Doc_Year),
           iswos = !is.na(ISI),
           isscop = !is.na(ScopusId))

  t1 <- data %>%
    group_by(Publication_Year) %>%
    summarise(P = n(),
              P_wos = sum(iswos),
              P_scop = sum(isscop))

  t2 <-
    data %>%
    summarise(P = n(),
              P_wos = sum(iswos),
              P_scop = sum(isscop)) |>
  mutate(Publication_Year = 'Total')

  bind_rows(t1, t2)
}

#' Indicators by year from Bibstat for DiVA like publication list
#'
#' @param con connection to Bibstat
#' @param data publication list
#' @param startyear the first publicaiton year to consider
#' @param stopyear the last publication year to consider
#' @param cfyear the last publication year to count field normalized citation indicators for
#' @param n_auth set to a field name with number of unit authors to use fractional counts
#' @export
indicators_diva <- function(con, data, startyear, stopyear, cfyear, n_auth = NULL){

  if(missing(startyear))
    startyear <- suppressWarnings(min(data$Doc_Year, na.rm = TRUE))
  if(missing(stopyear))
    stopyear <- suppressWarnings(max(data$Doc_Year, na.rm = TRUE))
  if(missing(cfyear))
    cfyear <- stopyear - 1

  UTs <- data |>
    filter(Doc_Year %in% startyear:stopyear,
           !is.na(ISI)) |>
    pull(ISI) |>
    unique()

  indicators_ut(con, UTs, yearly = TRUE, startyear, stopyear, cfyear, n_auth)
}

#' Indicators by year or individually from Bibstat for list of UT numbers
#'
#' @param con connection to Bibstat
#' @param uts list of UT numbers
#' @param yearly set to true to group by year, otherwise individual indicators
#' @param startyear the first publicaiton year to consider
#' @param stopyear the last publication year to consider
#' @param cfyear the last publication year to use field normalized citation indicators
#' @param n_auth set to a field name with number of unit authors to use fractional counts
#' @export
indicators_ut <- function(con, uts, yearly = TRUE, startyear, stopyear, cfyear, n_auth = NULL){

  if(missing(startyear))
    startyear <- suppressWarnings(min(data$Doc_Year, na.rm = TRUE))
  if(missing(stopyear))
    stopyear <- suppressWarnings(max(data$Doc_Year, na.rm = TRUE))
  if(missing(cfyear))
    cfyear <- stopyear - 1

  metrics <- con |>
    tbl("Doc_metrics") |>
    filter(UT %in% !!uts) |>
    collect() |>
    mutate(include_cf = (Doc_type_code_rev %in% c("AR", "RV") & Publication_year <= cfyear),
           include_jcf = Doc_type_code_rev %in% c("AR", "RV", "ED", "LE"),
           Publication_year = as.character(Publication_year))

  if(!yearly)
    return(metrics |>
             select(UT, Doc_type_code_rev, Publication_year, C_sciwo, C_scxwo, C_sciw3, C_scxw3, C_scxw3_Perc,
                    FRV_sciwo, FRV_scxwo, FRV_scxw3,
                    Cf_scxwo, Top25_scxwo, Top10_scxwo, Top5_scxwo, Top1_scxwo, Jcf, Q1, Q2, Q3,
                    No_authors, No_addresses, No_organizations, No_countries, Subject_category, Subj_weights, include_cf, include_jcf) |>
             mutate(across(c("Cf_scxwo", "Top25_scxwo", "Top10_scxwo", "Top5_scxwo", "Top1_scxwo"), \(x) if_else(include_cf, x, NA)),
                    across(c("Jcf", "Q1", "Q2", "Q3"), \(x) if_else(include_jcf, x, NA))))

  if(is.null(n_auth)){

    indics_year <- metrics |>
      group_by(Publication_year) |>
      summarise(P = n(),
                C_self = sum(C_sciwo, na.rm = TRUE),
                C_noself = sum(C_scxwo, na.rm = TRUE),
                C_avg = mean(C_scxwo, na.rm = TRUE),
                P_jcf = sum(include_jcf),
                Jcf = mean(Jcf[include_jcf], na.rm = TRUE),
                top20 = sum(Top20[include_jcf], na.rm = TRUE),
                top20share = mean(Top20[include_jcf], na.rm = TRUE),
                P_cf = sum(include_cf),
                Cf = mean(Cf_scxwo[include_cf], na.rm = TRUE),
                top1 = sum(Top1_scxwo[include_cf], na.rm = TRUE),
                top1share = mean(Top1_scxwo[include_cf], na.rm = TRUE),
                top5 = sum(Top5_scxwo[include_cf], na.rm = TRUE),
                top5share = mean(Top5_scxwo[include_cf], na.rm = TRUE),
                top10 = sum(Top10_scxwo[include_cf], na.rm = TRUE),
                top10share = mean(Top10_scxwo[include_cf], na.rm = TRUE),
                top25 = sum(Top25_scxwo[include_cf], na.rm = TRUE),
                top25share = mean(Top25_scxwo[include_cf], na.rm = TRUE))

    indics_tot <- metrics |>
      summarise(P = n(),
                C_self = sum(C_sciwo, na.rm = TRUE),
                C_noself = sum(C_scxwo, na.rm = TRUE),
                C_avg = mean(C_scxwo, na.rm = TRUE),
                P_jcf = sum(include_jcf),
                Jcf = mean(Jcf[include_jcf], na.rm = TRUE),
                top20 = sum(Top20[include_jcf], na.rm = TRUE),
                top20share = mean(Top20[include_jcf], na.rm = TRUE),
                P_cf = sum(include_cf),
                Cf = mean(Cf_scxwo[include_cf], na.rm = TRUE),
                top1 = sum(Top1_scxwo[include_cf], na.rm = TRUE),
                top1share = mean(Top1_scxwo[include_cf], na.rm = TRUE),
                top5 = sum(Top5_scxwo[include_cf], na.rm = TRUE),
                top5share = mean(Top5_scxwo[include_cf], na.rm = TRUE),
                top10 = sum(Top10_scxwo[include_cf], na.rm = TRUE),
                top10share = mean(Top10_scxwo[include_cf], na.rm = TRUE),
                top25 = sum(Top25_scxwo[include_cf], na.rm = TRUE),
                top25share = mean(Top25_scxwo[include_cf], na.rm = TRUE)) |>
      mutate(Publication_year = "Total")

  } else {

    auth <- data |>
      filter(Doc_Year %in% startyear:stopyear,
             !is.na(ISI)) |>
      rename(UT = ISI,
             n_auth = !!n_auth) |>
      select(UT, n_auth) |>
      distinct()

    indics_year <- metrics |>
      inner_join(auth, by = "UT") |>
      mutate(w = if_else(n_auth > No_authors, 1, n_auth / No_authors)) |>
      group_by(Publication_year) |>
      summarise(P = sum(w),
                C_self = sum(w * C_sciwo, na.rm = TRUE),
                C_noself = sum(w * C_scxwo, na.rm = TRUE),
                C_avg = mean(w * C_scxwo, na.rm = TRUE),
                P_jcf = sum(w * include_jcf),
                Jcf = weighted.mean(Jcf[include_jcf], w[include_jcf], na.rm = TRUE),
                top20 = sum(w[include_jcf] * Top20[include_jcf], na.rm = TRUE),
                top20share = weighted.mean(Top20[include_jcf], w[include_jcf], na.rm = TRUE),
                P_cf = sum(w * include_cf),
                Cf = weighted.mean(Cf_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top1 = sum(w[include_cf] * Top1_scxwo[include_cf], na.rm = TRUE),
                top1share = weighted.mean(Top1_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top5 = sum(w[include_cf] * Top5_scxwo[include_cf], na.rm = TRUE),
                top5share = weighted.mean(Top5_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top10 = sum(w[include_cf] * Top10_scxwo[include_cf], na.rm = TRUE),
                top10share = weighted.mean(Top10_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top25 = sum(w[include_cf] * Top25_scxwo[include_cf], na.rm = TRUE),
                top25share = weighted.mean(Top25_scxwo[include_cf], w[include_cf], na.rm = TRUE))

    indics_tot <- metrics |>
      inner_join(auth, by = "UT") |>
      mutate(w = if_else(n_auth > No_authors, 1, n_auth / No_authors)) |>
      summarise(P = sum(w),
                C_self = sum(w * C_sciwo, na.rm = TRUE),
                C_noself = sum(w * C_scxwo, na.rm = TRUE),
                C_avg = mean(w * C_scxwo, na.rm = TRUE),
                P_jcf = sum(w * include_jcf),
                Jcf = weighted.mean(Jcf[include_jcf], w[include_jcf], na.rm = TRUE),
                top20 = sum(w[include_jcf] * Top20[include_jcf], na.rm = TRUE),
                top20share = weighted.mean(Top20[include_jcf], w[include_jcf], na.rm = TRUE),
                P_cf = sum(w * include_cf),
                Cf = weighted.mean(Cf_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top1 = sum(w[include_cf] * Top1_scxwo[include_cf], na.rm = TRUE),
                top1share = weighted.mean(Top1_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top5 = sum(w[include_cf] * Top5_scxwo[include_cf], na.rm = TRUE),
                top5share = weighted.mean(Top5_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top10 = sum(w[include_cf] * Top10_scxwo[include_cf], na.rm = TRUE),
                top10share = weighted.mean(Top10_scxwo[include_cf], w[include_cf], na.rm = TRUE),
                top25 = sum(w[include_cf] * Top25_scxwo[include_cf], na.rm = TRUE),
                top25share = weighted.mean(Top25_scxwo[include_cf], w[include_cf], na.rm = TRUE)) |>
      mutate(Publication_year = "Total")
  }

  bind_rows(indics_year, indics_tot)
}


#' Number of publications by year from masterfile like publication list
#'
#' @param data publication list
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @export
npubs_master <- function(data, startyear, stopyear) {

  Publication_Year <- WebofScience_ID <- ScopusID <- Unit_Fraction <- iswos <- isscop <- NULL

  if(missing(startyear)) {
    startyear <- suppressWarnings(min(data$Publication_Year, na.rm = TRUE))
  }
  if(missing(stopyear)) {
    stopyear <- suppressWarnings(max(data$Publication_Year, na.rm = TRUE))
  }

  data <- data %>%
    filter(Publication_Year %in% startyear:stopyear) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           iswos = !is.na(WebofScience_ID),
           isscop = !is.na(ScopusID))

  t1 <- data %>%
    group_by(Publication_Year) %>%
    summarise(P_full = n(),
              P_frac = sum(Unit_Fraction),
              P_wos_full = sum(iswos),
              P_wos_frac = sum(iswos*Unit_Fraction),
              P_scop_full = sum(isscop),
              P_scop_frac = sum(isscop*Unit_Fraction))

  t2 <-
    data %>%
    summarise(P_full = n(),
              P_frac = sum(Unit_Fraction),
              P_wos_full = sum(iswos),
              P_wos_frac = sum(iswos*Unit_Fraction),
              P_scop_full = sum(isscop),
              P_scop_frac = sum(isscop*Unit_Fraction)) %>%
    mutate(Publication_Year = 'Total')

  bind_rows(t1, t2)

}


#' Citation indicators by year from masterfile like publication list, full counts
#'
#' @param data publication list
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @param analysisyear the year of analysis, defaults to current year
#' @import dplyr
#' @importFrom stats weighted.mean
#' @export
indics_full_master <- function(data, startyear, stopyear, analysisyear) {

  Publication_Year <- Publication_Type_WoS <- WebofScience_ID <-
    ScopusID <- scop_doctype <- Unit_Fraction <- iswos <- isscop <-
    Citations <- Jtop20 <- Ptop1 <- Ptop10 <- Ptop25 <- cf <- jcf <-
    scop_Jtop20 <- scop_Ptop1 <- scop_Ptop10 <- scop_Ptop25 <-
    scop_cscxo <- scop_fwci_x <- scop_snip <- NULL

  if(missing(startyear)) {
    startyear <- suppressWarnings(min(data$Publication_Year, na.rm = TRUE))
  }
  if(missing(stopyear)) {
    stopyear <- suppressWarnings(max(data$Publication_Year, na.rm = TRUE))
  }
  if(missing(analysisyear)) {
    analysisyear <- as.integer(format(Sys.Date(), "%Y"))
  }

  wos <- data %>%
    filter(Publication_Year %in% startyear:stopyear,
           Publication_Type_WoS %in% c("Article", "Review")) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           iswos = !is.na(WebofScience_ID),
           cf = if_else(Publication_Year < analysisyear - 2, cf, NA_real_),
           Ptop1 = if_else(Publication_Year < analysisyear - 2, Ptop1, NA_real_),
           Ptop10 = if_else(Publication_Year < analysisyear - 2, Ptop10, NA_real_),
           Ptop25 = if_else(Publication_Year < analysisyear - 2, Ptop25, NA_real_)
    )

  scop <- data %>%
    filter(Publication_Year %in% startyear:stopyear,
           scop_doctype %in% c("Article", "Review")) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           isscop = !is.na(ScopusID),
           scop_fwci_x = if_else(Publication_Year < analysisyear - 2, scop_fwci_x, NA_real_),
           scop_Ptop1 = if_else(Publication_Year < analysisyear - 2, scop_Ptop1, NA_integer_),
           scop_Ptop10 = if_else(Publication_Year < analysisyear - 2, scop_Ptop10, NA_integer_),
           scop_Ptop25 = if_else(Publication_Year < analysisyear - 2, scop_Ptop25, NA_integer_)
    )

  wos_year <- wos %>%
    group_by(Publication_Year) %>%
    summarise(P_wos = sum(iswos, na.rm = TRUE),
              C = sum(Citations, na.rm = TRUE),
              C_avg = mean(Citations, na.rm = TRUE),
              Cf = mean(cf, na.rm = TRUE),
              Top1p = sum(Ptop1, na.rm = TRUE),
              Top1share = mean(Ptop1, na.rm = TRUE),
              Top10p = sum(Ptop10, na.rm = TRUE),
              Top10share = mean(Ptop10, na.rm = TRUE),
              Top25p = sum(Ptop25, na.rm = TRUE),
              Top25share = mean(Ptop25, na.rm = TRUE),
              Jcf = mean(jcf, na.rm = TRUE),
              Top20p = sum(Jtop20, na.rm = TRUE),
              Top20share = mean(Jtop20, na.rm = TRUE),
              .groups = "drop"
    )

  wos_total <- wos %>%
    summarise(P_wos = sum(iswos, na.rm = TRUE),
              C = sum(Citations, na.rm = TRUE),
              C_avg = mean(Citations, na.rm = TRUE),
              Cf = mean(cf, na.rm = TRUE),
              Top1p = sum(Ptop1, na.rm = TRUE),
              Top1share = mean(Ptop1, na.rm = TRUE),
              Top10p = sum(Ptop10, na.rm = TRUE),
              Top10share = mean(Ptop10, na.rm = TRUE),
              Top25p = sum(Ptop25, na.rm = TRUE),
              Top25share = mean(Ptop25, na.rm = TRUE),
              Jcf = mean(jcf, na.rm = TRUE),
              Top20p = sum(Jtop20, na.rm = TRUE),
              Top20share = mean(Jtop20, na.rm = TRUE)
    ) %>%
    mutate(Publication_Year = 'Total')

  scop_year <- scop %>%
    group_by(Publication_Year) %>%
    summarise(P_scop = sum(isscop, na.rm = TRUE),
              C_scop = sum(scop_cscxo, na.rm = TRUE),
              C_avg_scop = mean(scop_cscxo, na.rm = TRUE),
              Cf_scop = mean(scop_fwci_x, na.rm = TRUE),
              Top1p_scop = sum(scop_Ptop1, na.rm = TRUE),
              Top1share_scop = mean(scop_Ptop1, na.rm = TRUE),
              Top10p_scop = sum(scop_Ptop10, na.rm = TRUE),
              Top10share_scop = mean(scop_Ptop10, na.rm = TRUE),
              Top25p_scop = sum(scop_Ptop25, na.rm = TRUE),
              Top25share_scop = mean(scop_Ptop25, na.rm = TRUE),
              Jcf_scop = mean(scop_snip, na.rm = TRUE),
              Top20p_scop = sum(scop_Jtop20, na.rm = TRUE),
              Top20share_scop = mean(scop_Jtop20, na.rm = TRUE),
              .groups = "drop"
    )

  scop_total <- scop %>%
    summarise(P_scop = sum(isscop, na.rm = TRUE),
              C_scop = sum(scop_cscxo, na.rm = TRUE),
              C_avg_scop = mean(scop_cscxo, na.rm = TRUE),
              Cf_scop = mean(scop_fwci_x, na.rm = TRUE),
              Top1p_scop = sum(scop_Ptop1, na.rm = TRUE),
              Top1share_scop = mean(scop_Ptop1, na.rm = TRUE),
              Top10p_scop = sum(scop_Ptop10, na.rm = TRUE),
              Top10share_scop = mean(scop_Ptop10, na.rm = TRUE),
              Top25p_scop = sum(scop_Ptop25, na.rm = TRUE),
              Top25share_scop = mean(scop_Ptop25, na.rm = TRUE),
              Jcf_scop = mean(scop_snip, na.rm = TRUE),
              Top20p_scop = sum(scop_Jtop20, na.rm = TRUE),
              Top20share_scop = mean(scop_Jtop20, na.rm = TRUE)
    ) %>%
    mutate(Publication_Year = 'Total')

  scop <- bind_rows(scop_year, scop_total)
  wos <- bind_rows(wos_year, wos_total)

  wos %>% full_join(scop, by = "Publication_Year")
}


#' Citation indicators by year from masterfile like publication list, fractional counts
#'
#' @param data publication list
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @param analysisyear the year of analysis, defaults to current year
#' @import dplyr
#' @importFrom stats weighted.mean
#' @export
indics_frac_master <- function(data, startyear, stopyear, analysisyear) {

  Publication_Year <- Publication_Type_WoS <- WebofScience_ID <-
    ScopusID <- scop_doctype <- Unit_Fraction <- iswos <- isscop <-
    Citations <- Jtop20 <- Ptop1 <- Ptop10 <- Ptop25 <- cf <- jcf <-
    scop_Jtop20 <- scop_Ptop1 <- scop_Ptop10 <- scop_Ptop25 <-
    scop_cscxo <- scop_fwci_x <- scop_snip <- NULL

  if(missing(startyear)) {
    startyear <- suppressWarnings(min(data$Publication_Year, na.rm = TRUE))
  }
  if(missing(stopyear)) {
    stopyear <- suppressWarnings(max(data$Publication_Year, na.rm = TRUE))
  }
  if(missing(analysisyear)) {
    analysisyear <- as.integer(format(Sys.Date(), "%Y"))
  }


  wos <- data %>%
    filter(Publication_Year %in% startyear:stopyear,
           Publication_Type_WoS %in% c("Article", "Review")) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           iswos = !is.na(WebofScience_ID),
           cf = if_else(Publication_Year < analysisyear - 2, cf, NA_real_),
           Ptop1 = if_else(Publication_Year < analysisyear - 2, Ptop1, NA_real_),
           Ptop10 = if_else(Publication_Year < analysisyear - 2, Ptop10, NA_real_),
           Ptop25 = if_else(Publication_Year < analysisyear - 2, Ptop25, NA_real_)
    )

  scop <- data %>%
    filter(Publication_Year %in% startyear:stopyear,
           scop_doctype %in% c("Article", "Review")) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           isscop = !is.na(ScopusID),
           scop_fwci_x = if_else(Publication_Year < analysisyear - 2, scop_fwci_x, NA_real_),
           scop_Ptop1 = if_else(Publication_Year < analysisyear - 2, scop_Ptop1, NA_integer_),
           scop_Ptop10 = if_else(Publication_Year < analysisyear - 2, scop_Ptop10, NA_integer_),
           scop_Ptop25 = if_else(Publication_Year < analysisyear - 2, scop_Ptop25, NA_integer_)
    )

  wos_year <-
    wos %>%
    group_by(Publication_Year) %>%
    summarise(P_wos = sum(iswos*Unit_Fraction, na.rm = TRUE),
              C = sum(Citations*Unit_Fraction, na.rm = TRUE),
              C_avg = weighted.mean(Citations, Unit_Fraction, na.rm = TRUE),
              Cf = weighted.mean(cf, Unit_Fraction, na.rm = TRUE),
              Top1p = sum(Ptop1*Unit_Fraction, na.rm = TRUE),
              Top1share = weighted.mean(Ptop1, Unit_Fraction, na.rm = TRUE),
              Top10p = sum(Ptop10*Unit_Fraction, na.rm = TRUE),
              Top10share = weighted.mean(Ptop10, Unit_Fraction, na.rm = TRUE),
              Top25p = sum(Ptop25*Unit_Fraction, na.rm = TRUE),
              Top25share = weighted.mean(Ptop25, Unit_Fraction, na.rm = TRUE),
              Jcf = weighted.mean(jcf, Unit_Fraction, na.rm = TRUE),
              Top20p = sum(Jtop20*Unit_Fraction, na.rm = TRUE),
              Top20share = weighted.mean(Jtop20, Unit_Fraction, na.rm = TRUE),
              .groups = "drop"
    )

  wos_total <- wos %>%
    summarise(P_wos = sum(iswos*Unit_Fraction, na.rm = TRUE),
              C = sum(Citations*Unit_Fraction, na.rm = TRUE),
              C_avg = weighted.mean(Citations, Unit_Fraction, na.rm = TRUE),
              Cf = weighted.mean(cf, Unit_Fraction, na.rm = TRUE),
              Top1p = sum(Ptop1*Unit_Fraction, na.rm = TRUE),
              Top1share = weighted.mean(Ptop1, Unit_Fraction, na.rm = TRUE),
              Top10p = sum(Ptop10*Unit_Fraction, na.rm = TRUE),
              Top10share = weighted.mean(Ptop10, Unit_Fraction, na.rm = TRUE),
              Top25p = sum(Ptop25*Unit_Fraction, na.rm = TRUE),
              Top25share = weighted.mean(Ptop25, Unit_Fraction, na.rm = TRUE),
              Jcf = weighted.mean(jcf, Unit_Fraction, na.rm = TRUE),
              Top20p = sum(Jtop20*Unit_Fraction, na.rm = TRUE),
              Top20share = weighted.mean(Jtop20, Unit_Fraction, na.rm = TRUE)
    ) %>%
    mutate(Publication_Year = "Total")

  scop_year <- scop %>%
    group_by(Publication_Year) %>%
    summarise(P_scop = sum(isscop*Unit_Fraction, na.rm = TRUE),
              C_scop = sum(scop_cscxo*Unit_Fraction, na.rm = TRUE),
              C_avg_scop = weighted.mean(scop_cscxo, Unit_Fraction, na.rm = TRUE),
              Cf_scop = weighted.mean(scop_fwci_x, Unit_Fraction, na.rm = TRUE),
              Top1p_scop = sum(scop_Ptop1*Unit_Fraction, na.rm = TRUE),
              Top1share_scop = weighted.mean(scop_Ptop1, Unit_Fraction, na.rm = TRUE),
              Top10p_scop = sum(scop_Ptop10*Unit_Fraction, na.rm = TRUE),
              Top10share_scop = weighted.mean(scop_Ptop10, Unit_Fraction, na.rm = TRUE),
              Top25p_scop = sum(scop_Ptop25*Unit_Fraction, na.rm = TRUE),
              Top25share_scop = weighted.mean(scop_Ptop25, Unit_Fraction, na.rm = TRUE),
              Jcf_scop = weighted.mean(scop_snip, Unit_Fraction, na.rm = TRUE),
              Top20p_scop = sum(scop_Jtop20*Unit_Fraction, na.rm = TRUE),
              Top20share_scop = weighted.mean(scop_Jtop20, Unit_Fraction, na.rm = TRUE),
              .groups = "drop"
    )

  scop_total <- scop %>%
    summarise(P_scop = sum(isscop*Unit_Fraction, na.rm = TRUE),
              C_scop = sum(scop_cscxo*Unit_Fraction, na.rm = TRUE),
              C_avg_scop = weighted.mean(scop_cscxo, Unit_Fraction, na.rm = TRUE),
              Cf_scop = weighted.mean(scop_fwci_x, Unit_Fraction, na.rm = TRUE),
              Top1p_scop = sum(scop_Ptop1*Unit_Fraction, na.rm = TRUE),
              Top1share_scop = weighted.mean(scop_Ptop1, Unit_Fraction, na.rm = TRUE),
              Top10p_scop = sum(scop_Ptop10*Unit_Fraction, na.rm = TRUE),
              Top10share_scop = weighted.mean(scop_Ptop10, Unit_Fraction, na.rm = TRUE),
              Top25p_scop = sum(scop_Ptop25*Unit_Fraction, na.rm = TRUE),
              Top25share_scop = weighted.mean(scop_Ptop25, Unit_Fraction, na.rm = TRUE),
              Jcf_scop = weighted.mean(scop_snip, Unit_Fraction, na.rm = TRUE),
              Top20p_scop = sum(scop_Jtop20*Unit_Fraction, na.rm = TRUE),
              Top20share_scop = weighted.mean(scop_Jtop20, Unit_Fraction, na.rm = TRUE)
    ) %>%
    mutate(Publication_Year = "Total")

  scop <- bind_rows(scop_year, scop_total)
  wos <- bind_rows(wos_year, wos_total)

  wos %>% full_join(scop, by = "Publication_Year")
}
