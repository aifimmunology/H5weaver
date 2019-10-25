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
