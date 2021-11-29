#' Get "csv02" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @import dplyr
#' @export
fetch_diva_csv02 <- function(baseurl, startyear, stopyear) {
  url <- sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear)
  csv02 <- read.csv(url, encoding = "UTF-8")
  names(csv02)[1] <- "PID"
  csv02 %>% mutate(PID = trimws(PID), Id = tolower(trimws(Id)))
}

#' Get "csvall2" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @export
fetch_diva_csvall2 <- function(baseurl, startyear, stopyear) {
  url <- sprintf('http://%s/smash/export.jsf?format=csvall2&aq=[[]]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc', baseurl, startyear, stopyear)
  csvall2 <- read.csv(url, encoding = "UTF-8")
  names(csvall2)[1] <- "PID"
  csvall2 %>% mutate(PID = trimws(PID))
}


#' Get "csv02" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param authors list of authors
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @import dplyr
#' @export
searchauth_diva_csv02 <- function(baseurl, authors, startyear, stopyear) {
  authq <- paste(paste0('%22', authors, '%22'), collapse = "%20OR%20")
  url <- sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])+authorId:(%s)&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear, authq)
  csv02 <- read.csv(url, encoding = "UTF-8")
  names(csv02)[1] <- "PID"
  csv02 %>% mutate(PID = trimws(PID), Id = tolower(trimws(Id)))
}

#' Get "csvall2" export from DiVA, by authorId
#'
#' @param baseurl the base url for the DiVA instance
#' @param authors list of authors
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @export
searchauth_diva_csvall2 <- function(baseurl, authors, startyear, stopyear) {
  authq <- paste(glue('[{{"authorId":"{authors}"}}]'), collapse = ",")
  url <- sprintf('http://%s/smash/export.jsf?format=csvall2&aq=[%s]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc', baseurl, authq, startyear, stopyear)
  csvall2 <- read.csv(url, encoding = "UTF-8")
  names(csvall2)[1] <- "PID"
  csvall2 %>% mutate(PID = trimws(PID))
}
