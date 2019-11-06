#' Write a stderr message with a leading date/time stamp
#'
#' @param x a character object with the message to display
#'
#' @return no return
#' @export
stm <- function(x) {
  assertthat::assert_that(class(x) == "character")
  assertthat::assert_that(length(x) == 1)

  write(paste0("[",Sys.time(),"] ",x), stderr())
}

#' Set a list value using path-style targeting
#'
#' @param l a list object
#' @param target a character object specifying the "path" to the target.
#' @param value an object or value to insert at the target location
#'
#' @return
#' @export
#'
#' @examples
#'
#' l <- list(forest = list(country = "USA",
#'                         maple = list(height = 100)))
#'
#' l <- set_list_path(l,
#'                    target = "/forest/spruce/height",
#'                    value = 30)
#'
#' l <- set_list_path(l,
#'                    target = "/forest/maple/diameter",
#'                    value = 6)
#'
#' l <- set_list_path(l,
#'                    target = "/valley/country",
#'                    value = "Canada")
#'
set_list_path <- function(l,
                          target,
                          value) {
  if(target == "/") {
    l <- list(value)
  } else {
    target <- sub("^/","",target)
    if(grepl("/",target)) {
      parent_target <- sub("/.+","",target)
      nest_target <- sub("^[^/]+", "", target)
      l[[parent_target]] <- set_list_path(l[[parent_target]],
                                          target = nest_target,
                                          value)
    } else {
      l <- list(value)
      names(l) <- target
    }
  }

  l
}

#' Retrieve an object from a list using path-style targeting
#'
#' @param l a list object
#' @param target  a character object specifying the "path" to the target.
#'
#' @return
#' @export
#'
#' @examples
#'
#' l <- list(forest = list(country = "USA",
#'                         maple = list(height = 100)))
#'
#' maple_height <- get_list_path(l,
#'                               target = "/forest/maple/height")
#'
#' forest_list <- get_list_path(l,
#'                              target = "/forest")
#'
#' forest_country <- get_list_path(l,
#'                                 target = "/forest/country")
#'
get_list_path <- function(l,
                          target) {
  if(target == "/") {
    return(l)
  } else {
    target <- sub("^/","",target)
    if(grepl("/",target)) {
      parent_target <- sub("/.+","",target)
      nest_target <- sub("^[^/]+", "", target)
      l <- get_list_path(l[[parent_target]],
                         target = nest_target)
    } else {
      return(l[[target]])
    }
  }

  l
}
