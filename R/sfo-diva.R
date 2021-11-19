#' Get URL for "csv02" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @export
diva_url_csv02 <- function(baseurl, startyear, stopyear) {
  sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear)
}

#' Get URL for "csvall2" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @export
diva_url_csvall2 <- function(baseurl, startyear, stopyear) {
  sprintf('http://%s/smash/export.jsf?format=csvall2&aq=[[]]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc', baseurl, startyear, stopyear)
}


