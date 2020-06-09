#' Find MAL data
#'
#' @param name the MAL name of interest
#' @param type the type of medium of the search i.e. manga, anime, character, person
#'
#' @importFrom attempt stop_if_all
#' @importFrom rlang parse_expr
#' @importFrom httr GET
#' @importFrom httr content
#' @export
#' @rdname infogeneral
#'
#' @return the results from the search
#' @examples
#' \dontrun{
#' info_general("naruto", type = 'anime')
#' info_anime('naruto')
#' info_manga('naruto)
#' info_character("naruto")
#' info_people("hana kana")
#' }
#'
info_general <- function(name = NULL, type = NULL) {
  # make sure that an actual variable name was supplied
  attempt::stop_if_all(name, is.null, "You need to specify a name to search")
  # Check if internet connection exists
  check_internet()
  if(!is.null(type)) {
    # quick scrape to return all possible related terms to the searched name
    tag_value <- eval(rlang::parse_expr(paste0("find_tag_", type, "('", name,"')")))
    api_call <- httr::GET(paste0("https://api.jikan.moe/v3/", type, "/", tag_value))
    # Checking to see if the API call went through
    check_status(api_call)
    # reformatting the API result into a dataframe
    list_form <- httr::content(api_call)
    # separating out more complicated variables (temporary)
    data.frame(rbind(unlist(list_form[!names(list_form) %in% c("related", "producers", "licensors", "studios", "genres")])))
  }
}

info_anime <- function(name, type = 'anime') {
  info_general(name, type)
}

info_manga <- function(name, type = 'manga') {
  info_general(name, type)
}

info_character <- function(name , type = 'character') {
  info_general(name = type)
}

info_people <- function(name , type = 'people') {
  info_general(name = type)
}
