#' @title Generate name for log
#' @description Generate name for logfile either returning default <timestamp>.log or <timestamp>__<Rscript>.log if run from command line
#' @param logfile_name filename to save log, without file extension (default=NULL)
#' @return character string with filename
#' @export
generate_logfile_name <- function(logfile_name = NULL) {
    time_format <- "%Y%m%d_%H%M%S"
    if (!is.null(logfile_name)) {
        return(paste0(
            format(lubridate::now(), paste0(time_format, "__")),
            logfile_name,
            ".log"
        ))
    } else if (is.null(logfile_name)) {
        return(format(lubridate::now(), paste0(time_format, ".log")))
    } else if (!interactive()) {
        # If run from the command line use name of Rscript as basename for logfile
        cmd_args <- commandArgs(trailingOnly = FALSE)
        has_script_filepath <- startsWith(cmd_args, "--file=")
        script_filepath <- cmd_args[has_script_filepath]
        # remove extension from name
        scriptname <- get_name(stringr::str_remove(script_filepath, "--file="))
        return(paste0(
            format(lubridate::now(), paste0(time_format, "__")),
            scriptname,
            ".log"
        ))
    }
}
