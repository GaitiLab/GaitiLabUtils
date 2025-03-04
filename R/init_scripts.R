#' @title Set working directory automatically
#' @export
set_wd <- function() {
    # Assuming project folder is a git/gitub repo or is an Rproject
    setwd(here::here())
    wd_path <- getwd()
    if (stringr::str_detect(basename(wd_path), "script?(s)")) {
        # if wd is a folder called Rscripts/scripts, then use the parent directory as working directory
        setwd(dirname(wd_path))
    }
}

#' @title Generate name for log
#' @description Generate name for logfile either returning default <timestamp>__log.log or <timestamp>__<Rscript>.log if run from command line
#' @param logfile_name filename to save log, without file extension (default=NULL)
#' @return string with filename
#' @export
generate_logfile_name <- function(logfile_name = NULL) {
    if (!is.null(logfile_name)) {
        return(paste0(format(lubridate::now(), "%Y%m%d_%H%M%S__"), logfile_name, ".log"))
    } else if (interactive()) {
        return(format(lubridate::now(), "%Y%m%d_%H%M%S__log.log"))
    } else {
        # If run from the command line use name of Rscript as basename for logfile
        cmd_args <- commandArgs(trailingOnly = FALSE)
        has_script_filepath <- startsWith(cmd_args, "--file=")
        script_filepath <- cmd_args[has_script_filepath]
        # remove extension from name
        scriptname <- get_name(stringr::str_remove(script_filepath, "--file="))
        return(paste0(format(lubridate::now(), "%Y%m%d_%H%M%S__"), scriptname, ".log"))
    }
}

#' Setup default argparser
#'
#' Set up a default argument parser, with two default arguments: log_level and output_dir
#' @title initialize argparser
#' @param description Description for script
#' @param default_output Default output directory (default="output")
#' @param default_log_file name of logfile without extension (default=NULL)
#' @param default_log_dir directory where logfile needs to be saved (default="output")
#' @return parser object
#'
#' @examples
#' \dontrun{
#' setup_default_argparser(description = "Example")
#' }
#' @importFrom argparse ArgumentParser
#' @export
setup_default_argparser <- function(
    description = "",
    default_output_dir = "output",
    default_log_file = NULL,
    default_log_dir = "output") {
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
        default = default_output_dir, help = "Directory to save output"
    )
    parser$add_argument("--log_file",
        type = "character", default = default_log_file,
        help = "Name of logfile without extension (default=NULL)"
    )
    parser$add_argument("--log_dir",
        type = "character",
        default = default_log_dir,
        help = "Directory where logfile needs to be saved (default='output')"
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
        create_dir(dirname(log_file))
        console_appender <- log4r::console_appender(
            layout = log4r::default_log_layout()
        )
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
