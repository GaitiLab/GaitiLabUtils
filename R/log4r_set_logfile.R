#' @title Set logfile
#' @param args list of arguments from argparser, should contain at least `log_dir`, `output_dir` and `log_file`
#' @param use_logs_subdir use a "logs" subdirectory
#' @export
set_logfile <- function(args, use_logs_subdir = TRUE) {
    log_dir <- ifelse(is.null(args$log_dir), args$output_dir, args$log_dir)
    log_file_path <- file.path(log_dir, generate_logfile_name(args$log_file))

    if (use_logs_subdir && !stringr::str_detect(dirname(log_dir), "logs")) {
        log_file_path <- file.path(
            log_dir,
            "logs",
            generate_logfile_name(args$log_file)
        )
    }
    create_dir(dirname(log_file_path))
    return(log_file_path)
}
