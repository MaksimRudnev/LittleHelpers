#' Downaload ESS data directly from website to R object with labels
#'
#' @param round Number of round - from 1 to 8.
#' @param country Vector of country [iso2c](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) codes, or "all".
#' @param user Your email which you used to register in ESS website.
#' @param version Version of the dataset in the form of character '02_1' (v2.1). Check the ESS website for the versions. Sometimes it matters. If NULL (default) the latest version is downloaded.
#' @param clean Logical. If the downloaded files should be removed after reading in.
#'
#' @details Don't put more than one country or more than one round - it won't work. This function will expire when ESS updates its data versions, but it happens about twice a year and can be fixed manually.
#'
#' @details `ess` package is tuned up for Stata users and sometimes do not get all the labels; it can only download one country data at a time; when it downloads several rounds, you get a list of data instead of integrated dataset.
#'
#' @seealso  \link{label_book}
#'
#' @examples
#' \donttest{
#' ESS2 <- download_ess(round=2, country="all", "mymail@gmail.com")
#' ESS6.Russia <- download_ess(round=6, country="RU", "mymail@gmail.com")
#' }
#'
#' @md
#'
#' @export
#'
download_ess <- function(round, country="all", user, version = NULL, clean = FALSE) {

  #1.Create url
  if(country!="all") {
    download.url <- paste("http://www.europeansocialsurvey.org/file/download?f=ESS",
                          round, country, ".spss.zip&c=", country, "&y=", round*2+2000, sep="")
  } else {
    if(is.null(version)) {
    version <-  c(`1` = "06_6",
                  `2` = "03_6",
                  `3` = "03_7",
                  `4` = "04_5",
                  `5` = "03_4",
                  `6` = "02_4",
                  `7` = "02_2",
                  `8` = "02_1")[round] }
    download.url <- paste("http://www.europeansocialsurvey.org/file/download?f=ESS", round, "e", version, ".spss.zip&c=&y=", round*2+2000, sep="")
  }

  #2. Download data

  require(httr)

  #Authenticate
  #auten<-POST("http://www.europeansocialsurvey.org/user/login", body = list(u=user))
  auten<-POST(paste("http://www.europeansocialsurvey.org/user/login?u=",user, sep=""))
  # Download
  data.file <- GET(download.url)
  # Write temporary file
  writeBin(content(data.file, "raw"), paste(tempdir(), "file.zip"))
  # Unzip
  path<-utils::unzip(paste(tempdir(), "file.zip"))

  #3. Read in with haven package

  require(haven)

  d <- read_spss(path[length(path)])

  if(clean) file.remove(path[length(path)])

    return(d)
}

