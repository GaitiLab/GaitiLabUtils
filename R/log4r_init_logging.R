#' @title Set up logging
#' @param log_level level of logging (1-5)
#' @param log_file path to log file (optional)
#' @return logger object
#'
#' @examples
#' \dontrun{
#' logr <- init_logging(3)
#' }
#' @export
#' @importFrom log4r console_appender default_log_layout logger
init_logging <- function(log_level = 5, log_file = NULL) {
    log_level_options <- c(
        `1` = "FATAL",
        `2` = "ERROR",
        `3` = "WARN",
        `4` = "INFO",
        `5` = "DEBUG"
    )
    if (!is.null(log_file)) {
        create_dir(dirname(log_file))
        console_appender <- log4r::console_appender(
            layout = log4r::default_log_layout()
        )
        file_appender <- log4r::file_appender(
            log_file,
            append = FALSE,
            layout = log4r::default_log_layout()
        )
        return(log4r::logger(
            threshold = log_level_options[as.character(log_level)],
            appenders = list(console_appender, file_appender)
        ))
    }
    return(log4r::logger(
        threshold = log_level_options[as.character(log_level)],
        appenders = log4r::console_appender(
            layout = log4r::default_log_layout()
        )
    ))
}


#' @title Set up object logging
#' @param log_file path to log file (optional)
#' @return logger object
#'
#' @examples
#' \dontrun{
#' logobj <- init_obj_logger()
#' }
#' @export
#' @importFrom log4r console_appender bare_log_layout logger
init_obj_logging <- function(log_file = NULL) {
    if (!is.null(log_file)) {
        create_dir(dirname(log_file))
        log_object_appender <- log4r::file_appender(
            log_file,
            append = TRUE,
            layout = bare_log_layout()
        )

        return(log4r::logger(
            threshold = 1,
            appenders = list(log_object_appender)
        ))
    }
    return(log4r::logger(
        threshold = 1,
        appenders = log4r::console_appender(layout = bare_log_layout())
    ))
}

#' @title Logging functions: log_info
#'
#' @param ... message
#'
#' @examples
#' \dontrun{
#' log_info("Hello world!")
#' }
#' @export
#' @importFrom log4r console_appender default_log_layout logger
log_info <- function(...) {
    log4r::info(logr, paste0(...))
}

#' @title Logging functions: log_error
#'
#' @param ... message
#'
#' @examples
#' \dontrun{
#' log_error("Hello world!")
#' }
#' @importFrom log4r console_appender default_log_layout logger
#' @export
log_error <- function(...) {
    log4r::error(logr, paste0(...))
}

#' @title Logging functions: log_fatal
#'
#' @param logr logger object
#' @param ... message
#'
#' @examples
#' \dontrun{
#' log_fatal("Hello world!")
#' }
#' @importFrom log4r console_appender default_log_layout logger
#' @export
log_fatal <- function(...) {
    log4r::fatal(logr, paste0(...))
}

#' @title Logging functions: log_debug
#' @param ... message

#' @examples
#' \dontrun{
#' log_debug("Hello world!")
#' }
#' @importFrom log4r console_appender default_log_layout logger
#' @export
log_debug <- function(...) {
    log4r::debug(logr, paste0(...))
}

#' @title Logging functions: log.warn
#'
#' @param ... message
#'
#' @examples
#' \dontrun{
#' log_warn("Hello world!")
#' }
#' @export
#' @importFrom log4r console_appender default_log_layout logger

log_warn <- function(...) {
    log4r::warn(logr, paste0(...))
}

#' @title Log an object
#' @param ... message
#' @export
log_object <- function(...) {
    log4r::info(obj_logger, paste0("\n"))
    log4r::info(obj_logger, paste0(capture.output(print(...)), "\n"))
    log4r::info(obj_logger, paste0("\n"))
    print(...)
}
