info_general <- function(name, type = NULL) {
  if(!is.null(type)) {
    tag_value <- eval(rlang::parse_expr(paste0("find_tag_", type, "('", name,"')")))
    list_form <- httr::content(httr::GET(paste0("https://api.jikan.moe/v3/", type, "/", tag_value)))
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
