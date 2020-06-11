#' Find the correct name for the search item
#'
#' @param name the MAL name of interest
#' @param sourceType the type of medium of the search i.e. manga, anime, character, person
#'
#' @importFrom attempt stop_if_all
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom utils menu
#' @export
#' @rdname find_tag
#'
#' @return returns the index number for the search item; combines with general_info family of functions to gather the actual data
#'
#' @examples
#'
#' \dontrun{
#' find_tag('bakemonogatari')
#' find_tag_anime('naruto')
#' find_tag_manga('Opus')
#' find_tag_character('Tomie')
#' find_tag_people('hana kana')
#' }
#'

find_tag <- function(name = NULL, sourceType = "") {
  # make sure that an actual variable name was supplied
  attempt::stop_if_all(name, is.null, "You need to specify a name to search")
  # scrapping the MAL website to find all possible names related to the search
  webpage <- xml2::read_html(paste("https://myanimelist.net/search/all?q=", gsub(" ", "%20", name), sep = ""))
  rank_data_html <- rvest::html_nodes(webpage, '.hoverinfo_trigger')
  rank_data_text <- unique(rvest::html_attr(rank_data_html, 'href'))
  rank_data_url <- rank_data_text[grepl(sourceType, rank_data_text)]
  # adding the option for user to determine which name they actually want to have returned from the search results
  gsub('.*/', "", dirname(rank_data_url))[utils::menu(choices = basename(rank_data_url), title = paste('Which ', sourceType, ' do you want to choose?', sep = ""))]
}

#' @rdname find_tag
#' @export
find_tag_anime <- function(name) {
  find_tag(name, sourceType = '/anime/')
}

#' @rdname find_tag
#' @export
find_tag_manga <- function(name) {
  find_tag(name, sourceType = '/manga/')
}

#' @rdname find_tag
#' @export
find_tag_people <- function(name) {
  find_tag(name, sourceType = '/people/')
}

#' @rdname find_tag
#' @export
find_tag_character <- function(name) {
  find_tag(name, sourceType = '/character/')
}
