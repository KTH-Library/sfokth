### TODO; Inför rlang::chr_unserialise_unicode på lämpliga ställen ###

#' Get "csv02" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @import dplyr
#' @importFrom utils read.csv
#' @export
fetch_diva_csv02 <- function(baseurl, startyear, stopyear) {
  url <- sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear)
  csv02 <- read.csv(url, encoding = "UTF-8")
  names(csv02)[1] <- "PID"
  csv02 |> mutate(PID = trimws(PID),
                   Id = tolower(trimws(Id)),
                   OrganisationIds = as.character(OrganisationIds))
}

#' Get "csvall2" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom stringr str_pad
#' @importFrom rlang chr_unserialise_unicode
#' @export
fetch_diva_csvall2 <- function(baseurl, startyear, stopyear) {

  ISI <- NULL

  url <- sprintf('http://%s/smash/export.jsf?format=csvall2&aq=[[]]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc', baseurl, startyear, stopyear)
  csvall2 <- read.csv(url, encoding = "UTF-8")
  names(csvall2)[1] <- "PID"

  if(nrow(csvall2) > 0) {
    csvall2 <-  csvall2 |>
      mutate(PID = trimws(PID),
             ISI = str_pad(ISI, width = 15, side = "left", pad = "0"),
             StartPage = as.character(StartPage),
             EndPage = as.character(EndPage),
             SeriesISSN = as.character(SeriesISSN),
             SeriesEISSN = as.character(SeriesEISSN),
             ISBN = as.character(ISBN),
             ArticleId = as.character(ArticleId),
             PublicationType = chr_unserialise_unicode(PublicationType))
  }

  csvall2
}


#' Get "csv02" export from DiVA, search by authorId
#'
#' @param baseurl the base url for the DiVA instance
#' @param authors list of authors (Id and/or Orcid)
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @import dplyr
#' @importFrom utils read.csv
#' @export
searchauth_diva_csv02 <- function(baseurl, authors, startyear, stopyear) {
  authq <- paste(paste0('%22', authors, '%22'), collapse = "%20OR%20")
  url <- sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])+authorId:(%s)&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear, authq)
  csv02 <- read.csv(url, encoding = "UTF-8")
  names(csv02)[1] <- "PID"
  csv02 |> mutate(PID = trimws(PID),
                   Id = tolower(trimws(Id)),
                   OrganisationIds = as.character(OrganisationIds))
}

#' Get "csvall2" export from DiVA, search by authorId
#'
#' @param baseurl the base url for the DiVA instance
#' @param authors list of authors (Id and/or Orcid)
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom stringr str_pad
#' @importFrom rlang chr_unserialise_unicode
#' @export
searchauth_diva_csvall2 <- function(baseurl, authors, startyear, stopyear) {

  ISI <- PID <- NULL

  authq <- paste(paste0('[{authorId:"', authors, '"}]'), collapse = ",")
  url <- sprintf('https://%s/smash/export.jsf?format=csvall2&aq=[%s]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc',
          baseurl,
          authq,
          startyear,
          stopyear)
  csvall2 <- read.csv(url, encoding = "UTF-8")
  names(csvall2)[1] <- "PID"

  if(nrow(csvall2) > 0) {
    csvall2 <-  csvall2 |>
      mutate(PID = trimws(PID),
             ISI = str_pad(ISI, width = 15, side = "left", pad = "0"),
             StartPage = as.character(StartPage),
             EndPage = as.character(EndPage),
             SeriesISSN = as.character(SeriesISSN),
             SeriesEISSN = as.character(SeriesEISSN),
             ISBN = as.character(ISBN),
             ArticleId = as.character(ArticleId),
             PublicationType = chr_unserialise_unicode(PublicationType))
  }

  csvall2
}
