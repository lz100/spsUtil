#' In-place operations
#' @description In-place operations like `i += 1`, `i -= 1` is not support in
#' R. These functions implement these operations in R.
#' @param e1 object, most likely a numeric object
#' @param e2 the operation value, the value to add, subtract, multiply, divide of.
#'
#' @return No return, directly assign the value back to `e1`
#' @seealso If you want [shiny::reactiveVal]  version of these operators,
#' check [spsComps](https://systempipe.org/sps/funcs/spscomps/reference/). [shiny::reactiveValues]
#' operation will be the same as normal values.
#' @export
#' @details
#' `inc(i)` is the same as `i <- i + 1`.
#' `inc(i, -1)` is the same as `i <- i - 1`.
#' `mult(i)` is the same as `i <- i * 2`.
#' `divi(i)` is the same as `i <- i / 2`.
#' @examples
#' i <- 0
#' inc(i) # add 1
#' i
#' inc(i) # add 1
#' i
#' inc(i, -1) # minus 1
#' i
#' inc(i, -1) # minus 1
#' i
#' x <- 1
#' mult(x) # times 2
#' x
#' mult(x) # times 2
#' x
#' divi(x) # divide 2
#' x
#' divi(x) # divide 2
#' x
inc <- function(e1,e2 = 1){eval.parent(substitute(e1 <- e1 + e2))}

#' @rdname inc
#' @export
mult <- function(e1,e2 = 2){eval.parent(substitute(e1 <- e1 * e2))}

#' @rdname inc
#' @export
divi <- function(e1,e2 = 2){eval.parent(substitute(e1 <- e1 / e2))}
