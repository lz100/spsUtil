### Utility functions, can be run outside SPS

#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_split str_remove_all str_replace_all str_which
#' @importFrom stringr str_remove str_which str_extract str_replace str_sort
#' @importFrom stringr str_detect str_pad
#' @importFrom magrittr %>%
#' @importFrom utils installed.packages
#' @importFrom R6 R6Class
#' @importFrom stats ave
NULL

#' Suppress cat, print, message and warning
#' @description Useful if you want to suppress  cat, print, message and warning.
#' You can choose what to mute. Default all four methods are muted.
#'
#' @param print_cat bool, mute `print` and `cat`?
#' @param message bool, mute `messages`?
#' @param warning bool, mute `warnings`?
#' @param x function or expression or value assignment expression
#'
#' @export
#' @return If your original functions has a return, it will return in
#' `invisible(x)`
#' @examples
#' quiet(warning(123))
#' quiet(message(123))
#' quiet(print(123))
#' quiet(cat(123))
#' quiet(warning(123), warning = FALSE)
#' quiet(message(123), message = FALSE)
#' quiet(print(123), print_cat = FALSE)
#' quiet(cat(123), print_cat = FALSE)
quiet <- function (
    x,
    print_cat = TRUE,
    message = TRUE,
    warning = TRUE
) {
    stopifnot(is.logical(print_cat) && length(print_cat) == 1)
    stopifnot(is.logical(message) && length(message) == 1)
    stopifnot(is.logical(warning) && length(warning) == 1)

    if (print_cat) sink(tempfile(), type = "out")
    on.exit(if (print_cat) sink())
    if (warning && message) invisible(force(suppressMessages(suppressWarnings(x))))
    else if (warning && !message) invisible(suppressWarnings(force(x)))
    else if (!warning && message) invisible(suppressMessages(force(x)))
    else invisible(force(x))
}


#' check namespace
#' @description  Help you to check if you have certain packages and
#' return missing package names
#'
#' @param packages vector of strings
#' @param quietly bool, give you warning on fail?
#' @param from  string, where this package is from like, "CRAN", "GitHub", only
#' for output message display purpose
#' @param time_out numeric, how long to wait before reaching the time limit. Sometimes
#' there are too many pkgs installed and takes too long to scan the whole list.
#' Set this timeout in seconds to prevent the long waiting.
#' @param on_timeout expressions, expressions to run when reaches timeout time.
#' Default is to return all packages as missing
#' @return vector of strings, of missing package names, `character(0)` if no missing
#' @export
#' @examples
#' if(!identical(Sys.getenv("NOT_CRAN"), "false")){
#'     checkNameSpace("ggplot2")
#'     checkNameSpace("random_pkg")
#'     checkNameSpace("random_pkg", quietly = TRUE)
#' }
checkNameSpace <- function(
    packages,
    quietly = FALSE,
    from = "CRAN",
    time_out = 1,
    on_timeout = {""}
    ){
    stopifnot(is.numeric(time_out) && length(time_out) == 1)
    if (!emptyIsFalse(packages)) return(NULL)
    pkg_ls <- timeout(.packages(TRUE), on_timeout = on_timeout, time_out = time_out)
    missing_pkgs <- packages[!packages %in% pkg_ls]
    if (!quietly & assertthat::not_empty(missing_pkgs)) {
        msg(glue("These packages are missing from ",
                 "{from}: {glue_collapse(missing_pkgs, sep = ',')}"), "warning")
    }
    return(missing_pkgs)
}


#' @title pretty logging message
#' @description If
#'
#'   1. `use_color = TRUE` or
#'   2. under SPS main package `use_crayon`option is `TRUE`
#'   3. In a console that supports colors
#'
#' Then the message will be colorful, other wise no color.
#'
#' "INFO" level spawns `message`, "WARNING" is `warning`, "ERROR" spawns `stop`,
#' other levels use `cat`.
#'
#'`spsinfo`, `spswarn`, `spserror` are higher level wrappers of `msg`. The
#' only difference is they have `SPS-` prefix.
#'
#' `spsinfo` has an additional
#' arg `verbose`. This arg works similarly to all other `verbose` args in
#' SPS:
#' 1. if not specified, it follows the project option. If SPS option `verbose` is
#' set to `TRUE`, message will be displayed; if `FALSE`, mute the message.
#' 2. It can be be forced to `TRUE` and `FALSE`. `TRUE` will forcibly generate the msg, and `FALSE`
#' will mute the message.
#' @importFrom crayon blue make_style red
#' @param msg a character string of message or a vector of character strings,
#' each item in the vector presents one line of words
#' @param level typically, one of "INFO", "WARNING", "ERROR", not case sensitive.
#' Other custom levels will work too.
#' @param .other_color hex color code or named colors, when levels are not in
#' "INFO", "WARNING", "ERROR", this value will be used
#' @param info_text info level text prefix, use with "INFO" level
#' @param warning_text warning level text prefix, use with "WARNING" level
#' @param error_text error level text prefix, use with "ERROR" level
#' @param use_color bool, default `TRUE`, to use color if supported?
#' @return see description and details
#' @details
#' 1. If `use_color` is `TRUE`, output message will forcibly use color if the console has color
#' support, ignore SPS `use_crayon` option.
#' 2. If `use_color` is `FALSE`, but you are using within SPS framework, the `use_crayon` option
#' is set to `TRUE`, color will be used.
#' 3. Otherwise message will be no color.
#' @export
#' @examples
#' msg("this is info")
#' msg("this is warning", "warning")
#' try(msg("this is error", "error"))
#' msg("this is another level", "my level", "green")
#' spsinfo("some msg, verbose false", verbose = FALSE) # will not show up
#' spsinfo("some msg, verbose true", verbose = TRUE)
#' spswarn("sps warning")
#' try(spserror("sps error"))
msg <- function(msg,
                level = "INFO",
                .other_color=NULL,
                info_text = "INFO",
                warning_text = "WARNING",
                error_text = "ERROR",
                use_color = TRUE){
    msg <- paste0(msg, collapse = "")
    info <- warn <- err <- other <- function(msg){return(msg)}
    if(use_color || (!is.null(getOption('sps')[['use_crayon']]) && getOption('sps')[['use_crayon']])){
        info <- crayon::blue$bold
        warn <- crayon::make_style("orange")$bold
        err <- crayon::red$bold
        other <- if(is.null(.other_color)) crayon::chr else crayon::make_style(.other_color)$bold
    }
    level_text <- switch(toupper(level),
        "WARNING" = warning_text,
        "ERROR" = error_text,
        "INFO" = info_text,
        level
    )
    msg <- if(str_detect(msg, "\\[.*\\] [0-9]{4}-[0-9]{2}")) msg
           else glue("[{level_text}] {Sys.time()} {msg}")

    switch(toupper(level),
        "WARNING" = warning("\r", warn(msg), call. = FALSE, immediate. = TRUE),
        "ERROR" = stop("\r", err(msg), call. = FALSE),
        "INFO" = message(info(msg)),
        cat(other(msg), sep = "\n")
    )
}

#' @param verbose bool, default get from sps project options, can be overwritten
#' @rdname msg
#' @export
spsinfo <- function(msg, verbose=NULL) {
    verbose <- if(is.null(verbose)) spsOption('verbose')
    else {assertthat::assert_that(is.logical(verbose)); verbose}
    if(verbose) msg(msg, "info", info_text =  "SPS-INFO", use_color = FALSE)
}

#' @rdname msg
#' @export
spswarn <- function(msg) msg(msg, "warning", warning_text = "SPS-WARNING", use_color = FALSE)

#' @rdname msg
#' @export
spserror <- function(msg) msg(msg, "error", error_text = "SPS-ERROR", use_color = FALSE)


#' Judgement of falsy value
#' @description judge if an object is or not a falsy value. This includes:
#' empty value, empty string `""`, `NULL`, `NA`, length of 0 and `FALSE` itself
#' @details
#' R does not have good built-in methods to judge falsy values and these kind
#' of values often cause errors in `if` conditions, for example
#' `if(NULL) 1 else 2` will cause error. So this function will be useful to
#' handle this kind of situations: `if(notFalsy(NULL)) 1 else 2`.
#'
#' 1. not working on S4 class objects.
#' 2. `isFalsy` is the reverse of `notFalsy`: `isFalsy(x)` = !`notFalsy(x)`
#' 3. `emptyIsFalse` is the old name for `notFalsy`
#'
#' Useful for if statement. Normal empty object in if will spawn error. Wrap the
#' expression with `emptyIsFalse` can avoid this. See examples
#' @param x any R object
#'
#' @export
#' @return `NA`, `""`, `NULL`, `length(0)`, `nchar == 0` and `FALSE` will return
#' `FALSE`, otherwise `TRUE` in `notFalsy` and the opposite in `isFalsy`
#' @examples
#' notFalsy(NULL)
#' notFalsy(NA)
#' notFalsy("")
#' try(`if(NULL) "not empty" else "empty"`) # this will generate error
#' if(notFalsy(NULL)) "not falsy" else "falsy" # but this will work
#' # Similar for `NA`, `""`, `character(0)` and more
#' isFalsy(NULL)
#' isFalsy(NA)
#' isFalsy("")
notFalsy <- function (x) {
    if (is.function(x)) return(TRUE)
    if (is.environment(x)) return(TRUE)
    if (length(x) < 1 || all(is.na(x)) || is.null(x)) return(FALSE)
    if (nchar(x[1]) == 0) return(FALSE)
    if (isFALSE(x)) return(FALSE)
    else TRUE
}

#' @rdname notFalsy
#' @export
isFalsy <- function(x) {
    !notFalsy(x)
}

#' @rdname notFalsy
#' @export
emptyIsFalse <- notFalsy



#' check if an URL can be reached
#' @importFrom httr GET stop_for_status
#' @param url string, the URL to request
#' @param timeout seconds to wait before return FALSE
#' @description check if a URL can be reached, return TRUE if yes and FALSE if
#' cannot or with other status code
#' @export
#' @return `TRUE` if url is reachable, `FALSE` if not
#' @examples
#' checkUrl("https://google.com")
#' try(checkUrl("https://randomwebsite123.com", 1))
checkUrl <- function(url, timeout = 5){
    assertthat::assert_that(length(url) == 1)
    if(!stringr::str_detect(url, "^http")) stop("url need to start with 'http(s)'")
    if(!is.numeric(timeout) || timeout < 0) stop("timeout need to a great than 0 number")
    tryCatch({
        httr::GET(url, httr::timeout(timeout)) %>%
            httr::stop_for_status()
        TRUE
            },
        error = function(e){
            msg(glue("Bad url {url}"), "warning")
            msg(e$message, "warning")
            return(FALSE)
        }
    )
}


#' Remove ANSI color code
#' @description Remove ANSI pre-/suffix-fix in a character string.
#' @param strings strings, a character vector
#' @export
#' @return strings with out ANSI characters
#' @examples
#' remove_ANSI("\033[34m\033[1ma\033[22m\033[39m")
#' remove_ANSI(c("\033[34m\033[1ma\033[22m\033[39m",
#'               "\033[34m\033[1mb\033[22m\033[39m"))
remove_ANSI <- function(strings) {
    assertthat::assert_that(is.character(strings))
    ANSI <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:",
                   "(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]")
    gsub(ANSI, "", strings, perl = TRUE)
}


#' Get or set SPS options
#' @description  Some functions in {spsUtil}, {spsComps} and {systempPipeShiny} will behave
#' differently if some SPS options are changed, but it is optional. All functions
#' have a default value. If SPS options are not changed, they will just use the
#' default setting. Read help files of individual functions for detail.
#' @param opt string, length 1, what option you want to get or set
#' @param value if this is not `NULL`, this function will set the
#' option you choose to this value
#' @param empty_is_false bool, when trying to get an option value, if the
#' option is `NULL`, `NA`, `""` or length is 0, return `FALSE`?
#' @param .list list, set many SPS options together at once by passing a
#' list to this function.
#' @return return the option value if value exists; return `FALSE` if the value
#' is empty, like `NULL`, `NA`, `""`; return `NULL` if `empty_is_false = FALSE`;
#'  see [notFalsy]
#'
#'  If `value != NULL` will set the option to this new value, no returns.
#' @export
#' @examples
#' spsOption("test1") # get a not existing option
#' spsOption("test1", 1) # set the value
#' spsOption("test1") # get the value again
#' spsOption("test2")
#' spsOption("test2", empty_is_false = FALSE)
#' spsOption(.list = list(
#'     test1 = 123,
#'     test2 = 456
#' ))
#' spsOption("test1")
#' spsOption("test2")
spsOption <- function(opt, value = NULL, .list = NULL, empty_is_false = TRUE){
    if (!is.null(.list)) {
        lapply(seq_along(.list), function(x) {
            if(is.null(.list[[x]])) spserror(c("In `spsOption`: option ", names(.list)[x], " is NULL, not allowed"))
        })
        old_opts <- getOption('sps')
        old_opts[names(.list)] <- .list
        return(options(sps = old_opts))
    }
    assertthat::assert_that(is.character(opt) && length(opt) == 1)
    if(assertthat::not_empty(value))
        options(sps = getOption('sps') %>% {.[[opt]] <- value; .})
    else {
        get_value <- getOption('sps')[[opt]]
        if(!emptyIsFalse(get_value)){
            if(empty_is_false) FALSE
            else get_value
        }
        else get_value
    }
}



#' Run expressions with a timeout limit
#' @description Add a time limit for R expressions
#' @param expr expressions, wrap them inside `{}`
#' @param time_out numeric, timeout time, in seconds
#' @param on_timeout expressions, callback expressions to run it the time out limit
#' is reached but expression is still running. Default is to return an error.
#' @param on_final expressions, callback expressions to run in the end regardless
#' the state and results
#' @param env environment, which environment to evaluate the expressions. Default is
#' the same environment as where the `timeout` function is called.
#'
#' @return default return, all depends on what return the `expr` will have
#' @export
#' @details
#' Expressions will be evaluated in the parent environment by default, for example
#' if this function is called at global level, all returns, assignments inside
#' `expr` will directly go to global environment as well.
#' @examples
#' # The `try` command in following examples are here to make sure the
#' # R CMD check will pass on package check. In a real case, you do not
#' # need it.
#'
#' # default
#' try(timeout({Sys.sleep(0.1)}, time_out = 0.01))
#' # timeout is evaluating expressions the same level as you call it
#' timeout({abc <- 123})
#' # so you should get `abc` even outside the function call
#' abc
#' # custom timeout callback
#' timeout({Sys.sleep(0.1)}, time_out = 0.01, on_timeout = {print("It takes too long")})
#' # final call back
#' try(timeout({Sys.sleep(0.1)}, time_out = 0.01, on_final = {print("some final words")})) # on error
#' timeout({123}, on_final = {print("runs even success")})  # on success
#' # assign to value
#' my_val <- timeout({10 + 1})
#' my_val
timeout <- function(
    expr,
    time_out = 1,
    on_timeout = {stop("Timout reached", call. = FALSE)},
    on_final = {},
    env = parent.frame()
    ){
    stopifnot(is.numeric(time_out) && length(time_out) == 1)
    expr <- substitute(expr)
    on_timeout <- substitute(on_timeout)
    on_final <- substitute(on_final)
    on.exit(setTimeLimit(), TRUE)
    setTimeLimit(elapsed = time_out, transient = TRUE)
    tryCatch({
        eval(expr, envir=env)
        },
        message = function(e) {
            message(e$message)
        },
        warning = function(e) {
            warning(e$message, call. = FALSE, immediate. = TRUE)
        },
        error = function(e) {
            if(grep("reached elapsed time limit", e$message)) {
                return(eval(on_timeout, envir=env))
            }
            stop(e$message, call. = FALSE)
        },
        finally = eval(on_final, envir=env)
    )
}













