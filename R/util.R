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
