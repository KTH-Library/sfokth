#' Number of publications by year from masterfile like publication list
#'
#' @param data publication list
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @export
npubs <- function(data, startyear, stopyear) {

  Publication_Year <- WebofScience_ID <- ScopusID <- Unit_Fraction <- iswos <- isscop <- NULL

  if(missing(startyear)) {
    startyear <- suppressWarnings(min(data$Publication_Year, na.rm = TRUE))
  }
  if(missing(stopyear)) {
    stopyear <- suppressWarnings(max(data$Publication_Year, na.rm = TRUE))
  }



  t1 <-
    data %>%
    filter(Publication_Year %in% startyear:stopyear) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           iswos = !is.na(WebofScience_ID),
           isscop = !is.null(ScopusID)) %>%
    group_by(Publication_Year) %>%
    summarise(P_full = n(),
              P_frac = sum(Unit_Fraction),
              P_wos_full = sum(iswos),
              P_wos_frac = sum(iswos*Unit_Fraction),
              P_scop_full = sum(isscop),
              P_scop_frac = sum(isscop*Unit_Fraction))

  t2 <-
    data %>%
    filter(Publication_Year %in% startyear:stopyear) %>%
    mutate(iswos = !is.na(WebofScience_ID),
           isscop = !is.null(ScopusID)) %>%
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
indics_full <- function(data, startyear, stopyear, analysisyear) {

  Publication_Year <- WebofScience_ID <- ScopusID <- iswos <- isscop <-
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

  data <- data %>%
    filter(Publication_Year %in% startyear:stopyear,
           Publication_Type_WoS %in% c("Article", "Review")) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           iswos = !is.na(WebofScience_ID),
           isscop = !is.null(ScopusID),
           cf = if_else(Publication_Year < analysisyear - 2, cf, NA_real_),
           Ptop1 = if_else(Publication_Year < analysisyear - 2, Ptop1, NA_real_),
           Ptop10 = if_else(Publication_Year < analysisyear - 2, Ptop10, NA_real_),
           Ptop25 = if_else(Publication_Year < analysisyear - 2, Ptop25, NA_real_),
           scop_fwci_x = if_else(Publication_Year < analysisyear - 2, scop_fwci_x, NA_real_),
           scop_Ptop1 = if_else(Publication_Year < analysisyear - 2, scop_Ptop1, NA_integer_),
           scop_Ptop10 = if_else(Publication_Year < analysisyear - 2, scop_Ptop10, NA_integer_),
           scop_Ptop25 = if_else(Publication_Year < analysisyear - 2, scop_Ptop25, NA_integer_)
    )


  t1 <-
    data %>%
    group_by(Publication_Year) %>%
    summarise(P_wos = sum(iswos),
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
              P_scop = sum(isscop),
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
    )

  t2 <-
    data %>%
    summarise(P_wos = sum(iswos),
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

  bind_rows(t1, t2)
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
indics_frac <- function(data, startyear, stopyear, analysisyear) {

  Publication_Year <- WebofScience_ID <- ScopusID <- Unit_Fraction <- iswos <- isscop <-
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

  data <- data %>%
    filter(Publication_Year %in% startyear:stopyear,
           Publication_Type_WoS %in% c("Article", "Review")) %>%
    mutate(Publication_Year = as.character(Publication_Year),
           iswos = !is.na(WebofScience_ID),
           isscop = !is.null(ScopusID),
           cf = if_else(Publication_Year < analysisyear - 2, cf, NA_real_),
           Ptop1 = if_else(Publication_Year < analysisyear - 2, Ptop1, NA_real_),
           Ptop10 = if_else(Publication_Year < analysisyear - 2, Ptop10, NA_real_),
           Ptop25 = if_else(Publication_Year < analysisyear - 2, Ptop25, NA_real_),
           scop_fwci_x = if_else(Publication_Year < analysisyear - 2, scop_fwci_x, NA_real_),
           scop_Ptop1 = if_else(Publication_Year < analysisyear - 2, scop_Ptop1, NA_integer_),
           scop_Ptop10 = if_else(Publication_Year < analysisyear - 2, scop_Ptop10, NA_integer_),
           scop_Ptop25 = if_else(Publication_Year < analysisyear - 2, scop_Ptop25, NA_integer_)
    )


  t1 <-
    data %>%
    group_by(Publication_Year) %>%
    summarise(P_wos = sum(iswos*Unit_Fraction),
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
              P_scop = sum(isscop*Unit_Fraction),
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
    )

  t2 <-
    data %>%
    summarise(P_wos = sum(iswos*Unit_Fraction),
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
              P_scop = sum(isscop*Unit_Fraction),
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
    mutate(Publication_Year = 'Total')

  bind_rows(t1, t2)
}
