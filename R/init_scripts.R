#' @title Set working directory automatically
#' @export
set_wd <- function() {
  	# Default to repo directory in interactive mode, where .git is
    if (interactive()) {
        return(here::here())
    }
    cmd_args <- commandArgs(trailingOnly = FALSE)
    has_script_filepath <- startsWith(cmd_args, "--file=")
    if (sum(has_script_filepath)) {
        setwd(dirname(unlist(strsplit(
            cmd_args[has_script_filepath],
            "="
        )))[2])
    }
}

#' @title Create timestamped logfile path
#' @description Create a logfile path of format <log_dir>/<timestamp>__<log_file>.log
#' @param log_file (default = 'log')
#' @param log_dir (default = 'logs')
#' @return string with path to logfile that needs to be created
#' @export
create_timestamped_logfile <- function(log_file = NULL, log_dir = "logs") {
    if ((is.null(log_file)) && (interactive())) {
        stop("Please use a valid filename...")
    } else if (!interactive() && is.null(log_file)) {
        cmd_args <- commandArgs(trailingOnly = FALSE)
        has_script_filepath <- startsWith(cmd_args, "--file=")
        if (sum(has_script_filepath)) {
            # Use filename from current as log_file name if run from terminal
            log_file <- tools::file_path_sans_ext(
                basename(unlist(strsplit(cmd_args[has_script_filepath], "="))[2])
            )
        }
    }
    return(file.path(log_dir, paste0(
        # Add a timestamp as prefix for filename
        format(
            lubridate::now(),
            "%Y%m%d_%H%M%S__"
        ),
        glue::glue("{log_file}.log")
    )))
}

#' Setup default argparser
#'
#' Set up a default argument parser, with two default arguments: log_level and output_dir
#' @title initialize argparser
#' @param description Description for script
#' @param default_output Default output directory
#' @param default_log_file name of logfile without extension (default=NULL)
#' @param default_log_dir directory where logfile needs to be saved (default="logs")
#' @return parser object
#'
#' @examples
#' \dontrun{
#' setup_default_argparser(description = "Example")
#' }
#' @importFrom argparse ArgumentParser
#' @export
setup_default_argparser <- function(description = "", default_output = "output", default_log_file = NULL, default_log_dir = "logs") {
    parser <- argparse::ArgumentParser(
        description = description, python_cmd = NULL
    )
    parser$add_argument("-ll", "--log_level",
        type = "integer",
        default = "4",
        help = "Log level: 1=FATAL, 2=ERROR, 3=WARN, 4=INFO, 5=DEBUG"
    )
    parser$add_argument("-o", "--output_dir",
        type = "character",
        default = default_output, help = "Directory to save output"
    )
    parser$add_argument("--log_file",
        type = "character", default = default_log_file, help = "Name of logfile without extension (default=NULL)"
    )
    parser$add_argument("--log_dir",
        type = "character",
        default = default_log_dir, help = "Directory where logfile needs to be saved (default='logs')"
    )
    return(parser)
}

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
        `1` = "FATAL", `2` = "ERROR", `3` = "WARN", `4` = "INFO",
        `5` = "DEBUG"
    )
    if (!is.null(log_file)) {
        console_appender <- log4r::console_appender(layout = log4r::default_log_layout())
        file_appender <- log4r::file_appender(log_file,
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
        appenders = log4r::console_appender(layout = log4r::default_log_layout())
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
        log_object_appender <- log4r::file_appender(log_file,
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
