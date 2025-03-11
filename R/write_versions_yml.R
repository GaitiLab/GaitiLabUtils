#' @title Write versions yml
#' @description Creates a yml file with the R version and versions of the packages of interest, e.g. loaded packages in a script. Useful for Nextflow pipelines
#' @param list_of_pkgs list (vector) of packages of interest (default=NULL). If Null uses pacman::p_loaded()  as list_of_pkgs
#' @param task_id (default=NULL)
#' @param outdir directory to save the 'versions.yml' (default=NULL)
#' @return NULL
#' @export
write_versions_yml <- function(
    list_of_pkgs = NULL,
    task_id = NULL,
    outdir = NULL) {
    r_version <- paste0(
        "    R: ",
        strsplit(version[["version.string"]], " ")[[1]][3]
    )

    if (is.null(list_of_pkgs)) {
        list_of_pkgs <- pacman::p_loaded()
    }

    out_file <- ifelse(
        is.null(outdir),
        "versions.yml",
        file.path(outdir, "versions.yml")
    )
    pkgs <- paste0(
        "    ",
        list_of_pkgs,
        ": ",
        lapply(list_of_pkgs, function(x) {
            as.character(packageVersion(x))
        })
    )

    writeLines(
        c(paste0(task_id, ": "), r_version, pkgs),
        out_file
    )
}
