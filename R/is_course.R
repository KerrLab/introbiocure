#' @export
is_course <- function(x) {
    assertthat::assert_that(is.numeric(x), length(x) == 1)
    x %in% c(180, 200)
}

assertthat::on_failure(is_course) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid course number")
}


# TODO: is_quarter
# TODO: is_?
