
#' Uniquefy a character vector
#' @description Fix duplicated values in a character vector, useful in column names
#' and some ID structures that requires unique identifiers. If any duplicated string
#' is found in the vector, a numeric index will be added after the these strings.
#' @param x character vector
#' @param sep_b string, separator before the number index
#' @param sep_a string, separator after the number index
#' @param mark_first bool, if duplicated values are found, do you want to add the
#' numeric index starting from the first copy? `FALSE` means starting from the second
#' copy.
#' @return returns a character vector
#' @export
#' @details
#' The input can also be a numeric vector, but the return will always be character.
#' @examples
#' strUniquefy(c(1,1,1,2,3))
#' strUniquefy(c(1,1,1,2,3), mark_first = FALSE)
#' strUniquefy(c(1,1,1,2,3), sep_b = "(", sep_a = ")")
#' strUniquefy(c("a","b","c","a","d","b"))
strUniquefy <- function(x, sep_b="_", sep_a = "", mark_first = TRUE) {
    stopifnot(is.character(x) || is.numeric(x))
    stopifnot(is.character(sep_b) && length(sep_b) == 1)
    stopifnot(is.character(sep_a) && length(sep_a) == 1)
    stopifnot(is.logical(mark_first) && length(mark_first) == 1)
    ave(x, x, FUN=function(i) if (length(i) > 1) {
        if(!mark_first) c(i[1], paste0(i[1], sep_b, seq_along(i)[-1] - 1, sep_a))
        else paste0(i[1], sep_b, seq_along(i), sep_a)
    } else {
        i[1]
    }) %>% as.character()
}



