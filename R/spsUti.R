### Utility functions, can be run outside SPS

#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_split str_remove_all str_replace_all str_which
#' @importFrom stringr str_remove str_which str_extract str_replace str_sort
#' @importFrom stringr str_detect str_pad
#' @importFrom magrittr %>%
NULL

#' Suppress cat print output
#' @description Useful if you want to suppress cat or print
#' @param x function or expression or value assignment expression
#' @export
#' @return If your original fucntions has a return, it will return in
#' `invisible(x)`
#' @examples
#' quiet(print(1))
#' quiet(cat(1))
quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}


#' check namespace
#' @description  Help you to check if you have certain packages and
#' return missing package names
#' @param packages vector of strings
#' @param quietly bool, give you warning on fail?
#' @param from  string, where this package is from like, "CRAN", "GitHub", only
#' for output message display purpose
#' @return vector strings, of missing package names
#' @export
#' @examples
#' checkNameSpace("ggplot2")
#' checkNameSpace("random_pkg")
#' checkNameSpace("random_pkg", quietly = TRUE)
checkNameSpace <- function(packages, quietly = FALSE, from = "CRAN") {
    if (!emptyIsFalse(packages)) return(NULL)
    missing_pkgs <- lapply(packages, function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) pkg
    }) %>% unlist()
    if (!quietly & assertthat::not_empty(missing_pkgs)) {
        msg(glue("These packages are missing from ",
                 "{from}: {glue_collapse(missing_pkgs, sep = ',')}"), "warning")
        }
    return(missing_pkgs)
}


#' @title pretty logging message
#' @description If
#'   1. `use_color = TRUE` or
#'   2. under SPS main package `use_crayon`option is `TRUE`
#'   3. In a console that supports colors
#'   Then the message will be colorful, other wise no color
#'
#' "INFO" level spawns `message`, "WARNING" is `warning`, "ERROR" spawns `stop`,
#' other levels use `cat`.
#'
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
#' @return see description
#' @examples
#' msg("this is info")
#' msg("this is warning", "warning")
#' try(msg("this is error", "error"))
#' msg("this is another level", "my level", "green")
#'
#' @export
msg <- function(msg,
                level = "INFO",
                .other_color="white",
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
        other <- crayon::make_style(.other_color)$bold
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


#' Empty objects and FALSE will return FALSE
#' @description judge if an object is empty or FALSE, and return FALSE if it is
#' @details  not working on S4 class objects.
#'
#' Useful for if statement. Normal empty object in if will spawn error. Wrap the
#' expression with `emptyIsFalse` can avoid this. See examples
#' @param x any R object
#'
#' @export
#' @return `NA`, `""`, `NULL`, `length(0)`, `nchar == 0` and `FALSE` will return
#' `FALSE`, otherwise `TRUE`.
#' @examples
#' emptyIsFalse(NULL)
#' emptyIsFalse(NA)
#' emptyIsFalse("")
#' try(`if(NULL) "not empty" else "empty"`) # will generate error
#' if(emptyIsFalse(NULL)) "not empty" else "empty" # this will work
#' # similar for `NA`, `""`, `character(0)` and more
emptyIsFalse <- function(x){
    if(is.function(x)) return(TRUE)
    if(length(x) > 1)  return(TRUE)
    if(length(x) == 0) return(FALSE)
    if(is.na(x)) return(FALSE)
    if(nchar(x) == 0) return(FALSE)
    if(isFALSE(x)) return(FALSE)
    else TRUE
}


#' check if an URL can be reached
#' @importFrom httr GET timeout stop_for_status
#' @param url string, the URL to request
#' @param timeout seconds to wait before return FALSE
#' @description check if a URL can be reached, return TRUE if yes and FALSE if
#' cannot or with other status code
#' @export
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



