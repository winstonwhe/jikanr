find_tag <- function(Name, sourceType = "") {
  webpage <- xml2::read_html(paste("https://myanimelist.net/search/all?q=", gsub(" ", "%20", Name), sep = ""))
  rank_data_html <- rvest::html_nodes(webpage, '.hoverinfo_trigger')
  rank_data_text <- unique(rvest::html_attr(rank_data_html, 'href'))
  rank_data_url <- rank_data_text[grepl(sourceType, rank_data_text)]
  gsub('.*/', "", dirname(rank_data_url))[utils::menu(choices = basename(rank_data_url), title = paste('Which ', sourceType, ' do you want to choose?', sep = ""))]
}


find_tag_anime <- function(Name) {
  find_tag(Name, sourceType = '/anime/')
}

find_tag_manga <- function(Name) {
  find_tag(Name, sourceType = '/manga/')
}

find_tag_people <- function(Name) {
  find_tag(Name, sourceType = '/people/')
}

find_tag_character <- function(Name) {
  find_tag(Name, sourceType = '/character/')
}
