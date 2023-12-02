

#' Get SRF Articles
#'
#' @param url URL to the article
#' @param table_name name of the table
#' @param browser browser to use
#' @param port port for silenium
#'
#' @return creates an SQL table
#' @export
#'
#' @examples get_article("https://www.srf.ch/news/schweiz/lawinengefahr-in-der-schweiz-lawinenprognostiker-zurzeit-ist-die-lawinensituation-kritisch")
get_article <- function(url, table_name = "SRFArticles", browser = "firefox", port = 5443L){ #add an addtional statement for library name

  if (!require("RSelenium", character.only = TRUE)) {
    install.packages("RSelenium")
    library(RSelenium)
  }
  if (!require("stringr", character.only = TRUE)) {
    install.packages("stringr")
    library(stringr)
  }
  if (!require("DBI", character.only = TRUE)) {
    install.packages("DBI")
    library(DBI)
  }
  if (!require("lubridate", character.only = TRUE)) {
    install.packages("lubridate")
    library(lubridate)
  }

  if (!(exists("con"))) {
    con = dbConnect(RSQLite::SQLite(), ":memory:")
    con <<- con
  }

  tryCatch({
    # Attempt to read the table
    your_data <- dbReadTable(con, table_name)
  }, error = function(e) {
    table_string = sprintf("CREATE TABLE %s (
    articleID INT(8),
    title TEXT PRIMARY KEY,
    portal VARCHAR(256),
    rubric1 VARCHAR(256),
    rubric2 VARCHAR(256),
    einleitung TEXT,
    published_date DATE,
    mod_date DATE,
    entry_date DATETIME DEFAULT CURRENT_DATE
    )", table_name)
    dbSendQuery(con, table_string)
  })

  articleSQl <- sprintf("SELECT COUNT(*) FROM %s", table_name)
  num_art <- dbSendQuery(con, articleSQl)
  num_df <- dbFetch(num_art, n = -1)
  articleID <- as.character(num_df[1][1] + 1)

  #check if the driver work -

  if (!(exists("driver"))) {
    driver <- rsDriver(browser = browser, port = port, chromever = NULL)
    rs = driver$client
    driver <<- driver
    rs <<- rs
  }

  rs$navigate(url)
  Sys.sleep(1)
  html1 = rs$getPageSource()

  #articleID - check if there is something
  titel = str_match_all(html1, '<meta name="DC.title" content="([^"]+)">')[[1]][, 2]
  portal = str_match_all(html1, '<meta name="srf.portal" content="([^"]+)">')[[1]][, 2]
  rubric1 = str_match_all(html1, '<meta name="srf.rubric1" content="([^"]+)">')[[1]][, 2]
  rubric2 = str_match_all(html1, '<meta name="srf.rubric2" content="([^"]+)">')[[1]][, 2]
  einleitung = str_match_all(html1, '<meta property="og:description" content="([^"]+)">')[[1]][, 2]
  published_date = str_match_all(html1, '<meta property="article:published_time" content="([^"]+)">')[[1]][, 2]
  mod_date = str_match_all(html1, '<meta property="article:modified_time" content="([^"]+)">')[[1]][, 2]

  if (length(portal) == 0) {
    portal = "NA"
  }

  if (length(rubric1) == 0) {
    rubric1 = "NA"
  }

  if (length(rubric2) == 0) {
    rubric2 = "NA"
  }

  if (length(published_date) == 0) {
    published_date = "NA"
  }
  else {
    parsed_datetime <- as.POSIXct(published_date)
    published_date <- format(parsed_datetime, "%Y-%m-%d")
  }

  if (length(mod_date) == 0) {
    mod_date = "NA"
  }
  else {
    parsed_datetime <- as.POSIXct(mod_date)
    mod_date <- format(parsed_datetime, "%Y-%m-%d")
  }

  if (length(einleitung) == 0) {
    einleitung = "NA"
  }


  #modify date

  entry <- sprintf("INSERT INTO %s
  (articleID, title, portal, rubric1, rubric2, einleitung, published_date, mod_date)
  VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                   table_name, articleID, titel, portal, rubric1, rubric2, einleitung, published_date, mod_date)

  dbSendStatement(con, entry)

}
