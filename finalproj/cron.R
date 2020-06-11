library(DBI)
library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)

readRenviron(".Renviron")

host <- "ruby.db.elephantsql.com"

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "avlkfgtq",
  user = "avlkfgtq", password = Sys.getenv("mykey"), host = host
)


html <- read_html("https://www.op.gg/champion/statistics")
# read the info from the websit


champname <- html %>% 
  html_nodes("a") %>%
  html_nodes("div.champion-index__champion-item__name") %>% 
  html_text()
# scrap all the campion names


champurl <- html %>% 
  html_nodes("div.champion-index__champion-list") %>% 
  html_nodes("a") %>% 
  html_attr("href")
# scrap all the corresponding urls of each champion


position <- html %>% 
  html_nodes("a") %>%
  html_nodes("div.champion-index__champion-item__positions") %>% 
  html_text() 
# scrap champions' available positions

champnum <- c(1:length(champname))


i = 1
while (i <= length(champname)){
  position[i] <-  position[i] %>% 
    str_match_all("[A-Z][a-z]+")
  position[[i]] = replace(position[[i]], position[[i]] == "Middle", "Mid")
  # replace "Middle" with "Mid" for better future use
  position[[i]] = replace(position[[i]], position[[i]] == "Bottom", "Bot")
  # replace "Bottom" with "Bot" for better future use
  i = i+1
}
# Separate the positions


champinfo <- tibble(name = champname, url = champurl, num = champnum, posi = position) %>% 
  mutate(url = paste(champinfo$url, sep = ""))
# combine the info into a tibble


m = 1
n = 1
rname <- character(0)
rposi <- character(0)
rrate <- list()
rpick <- list()
while (m <= length(champname)){
  while(n <= length(champinfo$posi[[m]])){
    reurl <- read_html(paste(champinfo$url[m], "/", tolower(champinfo$posi[[m]][n,1]), sep = ""))
    rname <- c(rname, champinfo$name[m])
    rposi <- c(rposi, champinfo$posi[[m]][n,1])
    rwin <- reurl %>% 
      html_nodes("div.champion-box-content") %>% 
      html_nodes("div.champion-stats-trend-rate") %>% 
      html_text() %>% 
      str_match_all("[0-9]+[.]?[0-9]?")
    rrate <- c(rrate, rwin[[1]][1,1])
    rpick <- c(rpick, rwin[[2]][1,1])
    n = n +1
  }
  n = 1
  m = m+1
}

specinfo <- tibble(name = rname, position = rposi, win_rate = as.numeric(rrate), pick_rate = as.numeric(rpick))

if (!("specinfo" %in% dbListTables(con))) {
  # create table if it doesn't exist
  con %>% dbWriteTable(
    "specinfo",
    data.frame(time = as_datetime(double(0)), action = integer(0))
  )
}

dbWriteTable(con, "specinfo", specinfo, overwrite = TRUE)

dbDisconnect(con)
