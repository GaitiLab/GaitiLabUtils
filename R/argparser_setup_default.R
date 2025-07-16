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


#' @title params_ls_to_df
#' @description Convert list of parameters to dataframe
#' @param params_list list of parameters
#' @return dataframe of parameters (2 columns)
#' @importFrom dplyr %>%
params_ls_to_df <- function(params_list) {
    return(
        data.frame(lapply(params_list, function(param) {
            if (is.null(param)) {
                return("NULL")
            }
            return(param)
        })) %>%
            t() %>%
            data.frame() %>%
            dplyr::rename(value = colnames(.))
    )
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
    default_log_dir = NULL
) {
    parser <- argparse::ArgumentParser(
        description = description,
        python_cmd = NULL
    )
    parser$add_argument(
        "-ll",
        "--log_level",
        type = "integer",
        default = "4",
        help = "Log level: 1=FATAL, 2=ERROR, 3=WARN, 4=INFO, 5=DEBUG"
    )
    parser$add_argument(
        "-o",
        "--output_dir",
        type = "character",
        default = default_output_dir,
        help = "Directory to save output"
    )
    parser$add_argument(
        "--log_file",
        type = "character",
        default = default_log_file,
        help = "Name of logfile without extension (default=NULL)"
    )
    parser$add_argument(
        "--log_dir",
        type = "character",
        default = default_log_dir,
        help = "Directory where logfile needs to be saved (default='output/logs')"
    )
    parser$add_argument(
        "--nf-process-id",
        dest = "nf_process_id",
        type = "character",
        help = "Task ID from Nextflow, only needed in Nextflow pipeline",
        default = NULL
    )
    return(parser)
}
