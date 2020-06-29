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
#' @rdname general_info
#'
#' @return the results from the search in a dataframe format
#' @export
#' @examples
#' \dontrun{
#' general_info('naruto', type = 'anime')
#' anime_info('bakemonogatari')
#' manga_info('Opus')
#' character_info('Tomie')
#' people_info('hana kana')
#'}
general_info <- function(name = NULL, type = NULL, auto = FALSE) {
  # make sure that an actual variable name was supplied
  attempt::stop_if_all(name, is.null, "You need to specify a name to search")
  # Check if internet connection exists
  check_internet()
  if(!is.null(type)) {
    # quick scrape to return all possible related terms to the searched name
    tag_value <- eval(rlang::parse_expr(paste0("find_tag_", type,
                                               "(name = '", name,
                                               "', auto = ", auto, ")")))
    api_call <- httr::GET(paste0("https://api.jikan.moe/v3/", type, "/", tag_value))
    # Checking to see if the API call went through
    check_status(api_call)
    # reformatting the API result into a dataframe
    list_form <- httr::content(api_call)
    # separating out more complicated variables (temporary)
    data.frame(rbind(unlist(list_form[!names(list_form) %in% c("related", "producers", "licensors", "studios", "genres")])))
  }
}

#' @rdname general_info
#' @export
anime_info <- function(name, type = 'anime', ...) {
  general_info(name, type, ...)
}

#' @rdname general_info
#' @export
manga_info <- function(name, type = 'manga', ...) {
  general_info(name, type, ...)
}

#' @rdname general_info
#' @export
character_info <- function(name , type = 'character', ...) {
  general_info(name = type, ...)
}

#' @rdname general_info
#' @export
people_info <- function(name , type = 'people', ...) {
  general_info(name = type, ...)
}
