#' @title Extract name from filepath
#' @param filepath filepath
#' @return name of file without extension
#'
#' @examples
#' \dontrun{
#' get_name("R/my_script.R") returns 'my_script'
#' }
#' @export
#'
get_name <- function(filepath) {
    return(tools::file_path_sans_ext(fs::path_file(filepath)))
}

#' @title Format large numbers with scientific notation
#' @param value the string or number to be formatted
#' @references https://stackoverflow.com/a/24241954
#' @export
format_scientific <- function(value) {
    # turn in to character string in scientific notation
    value <- format(value, scientific = TRUE)
    value <- gsub("0e\\+00", "0", value)
    # quote the part before the exponent to keep all the digits
    value <- gsub("^(.*)e", "'\\1'e", value)
    # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2)
    value <- gsub("e\\+", "e", value)
    # convert 1x10^ or 1.000x10^ -> 10^
    value <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", value)

    # turn the 'e+' into plotmath format
    value <- gsub("e", "%*%10^", value)
    # return this as an expression
    parse(text = value)
}

#' @title Obtain current date
#' @description Obtain current date in format YYYYMMDD
#' @return current date
#' @export
get_current_date <- function() {
    return(format(Sys.time(), "%Y%m%d"))
}
