#' notlo
#'
#' Not hello, but added to check how export works
#'

notlo <- function() {
  print("this is not hello")
}


#' notlo 2 - calls hello
#'
#' Check if can call a funtion in another file without \code{source}ing it
#'
#' @export

notlo2 <- function() {
  hello()
}
