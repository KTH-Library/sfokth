### TODO; Inför rlang::chr_unserialise_unicode på lämpliga ställen ###

#' Get "csv02" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @param savefile an optional filename to save the csv in
#' @import dplyr curl
#' @export
fetch_diva_csv02 <- function(baseurl, startyear, stopyear, savefile = NULL) {

  if(is.null(savefile))
    savefile <- paste(tempfile(), "csv", sep = ".")


  url <- sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear)
  curl_download(url, savefile)
  csv02 <- read.csv(savefile, encoding = "UTF-8")

  csv02 |> mutate(PID = trimws(PID),
                   Id = tolower(trimws(Id)),
                   OrganisationIds = as.character(OrganisationIds))
}

#' Get "csvall2" export from DiVA
#'
#' @param baseurl the base url for the DiVA instance
#' @param startyear the first publication year to consider
#' @param stopyear the last publication year to consider
#' @param savefile an optional filename to save the csv in
#' @import dplyr curl
#' @importFrom stringr str_pad
#' @importFrom rlang chr_unserialise_unicode
#' @export
fetch_diva_csvall2 <- function(baseurl, startyear, stopyear, savefile = NULL) {

  ISI <- NULL

  if(is.null(savefile))
    savefile <- paste(tempfile(), "csv", sep = ".")

  url <- sprintf('http://%s/smash/export.jsf?format=csvall2&aq=[[]]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc', baseurl, startyear, stopyear)
  curl_download(url, destfile = savefile)
  csvall2 <- read.csv(savefile, encoding = "UTF-8")

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
#' @param savefile an optional filename to save the csv in
#' @import dplyr curl
#' @export
searchauth_diva_csv02 <- function(baseurl, authors, startyear, stopyear, savefile = NULL) {

  if(is.null(savefile))
    savefile <- paste(tempfile(), "csv", sep = ".")

  authq <- paste(paste0('%22', authors, '%22'), collapse = "%20OR%20")
  url <- sprintf('http://%s/dice/csv02?query=-publicationTypeCode:studentThesis%%20+year:([%d%%20TO%%20%d])+authorId:(%s)&start=0&rows=1000000&sort=author_sort%%20asc', baseurl, startyear, stopyear, authq)
  curl_download(url, destfile = savefile)
  csv02 <- read.csv(savefile, encoding = "UTF-8")

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
#' @param savefile an optional filename to save the csv in
#' @import dplyr curl
#' @importFrom stringr str_pad
#' @importFrom rlang chr_unserialise_unicode
#' @export
searchauth_diva_csvall2 <- function(baseurl, authors, startyear, stopyear, savefile = NULL) {

  ISI <- PID <- NULL

  if(is.null(savefile))
    savefile <- paste(tempfile(), "csv", sep = ".")

  authq <- paste(paste0('[{authorId:"', authors, '"}]'), collapse = ",")
  url <- sprintf('https://%s/smash/export.jsf?format=csvall2&aq=[%s]&aqe=[]&aq2=[[{"dateIssued":{"from":"%d","to":"%d"}},{"publicationTypeCode":["review","bookReview","monographLicentiateThesis","article","comprehensiveLicentiateThesis","book","manuscript","patent","dissertation","conferenceProceedings","monographDoctoralThesis","report","comprehensiveDoctoralThesis","collection","chapter","conferencePaper","other"]},{"contentTypeCode":["refereed","science","other"]}]]&onlyFullText=false&noOfRows=100000&sortOrder=author_sort_asc',
          baseurl,
          authq,
          startyear,
          stopyear)
  curl_download(url, destfile = savefile)
  csvall2 <- read.csv(savefile, encoding = "UTF-8")

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
