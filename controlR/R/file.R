#' Check if file exists
#'
#' @param file character string, name of file(s).
#' @param ... further arguments passed to or from other methods.
# @importFrom fs file_exists path_file path_dir dir_ls
#' @importFrom stringr str_c str_sub
#' @return logical
#' @export file_exists
#' @examples
#' # specify file (which exists)
#' file <- system.file("extdata/control.inp", package = "controlR")
#' file_exists(file = file)
#'
#' # in case a file does not exists
#' file <- system.file("extdata/CoNtRoL.inp", package = "controlR")
#' file_exists(file = file, case_sensitive = FALSE)
#' file_exists(file = file, case_sensitive = TRUE)
file_exists <- function(file, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("case_sensitive", "quiet")
  case_sensitive <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(case_sensitive)) case_sensitive <- TRUE
  if (is.null(quiet)) quiet <- FALSE

  # ---- main part of procedure ----

  # Check if file exists
  #exists <- fs::file_exists(path = file)
  exists <- file.exists(file)
  if (!all(exists)) {
    #if (!quiet) message(str_c("\nfile does not exists '", path_file(file)[!exists], "'"))
    if (!quiet) message(str_c("\nfile does not exists '", basename(file)[!exists], "'"))
    exists <- FALSE
  } else {

    # check case (only filename and not directory)
    if (case_sensitive) {
      for (rec in 1:length(file)) {
        file_tmp <- file[rec]
        if (exists[rec]) {
          #glob <- path_file(file_tmp)
          glob <- basename(file_tmp)
          pos <- regexpr(pattern = "[[:punct:]]", text = glob)
          if (pos > 0) glob <- str_c("*", str_sub(string = glob, start = 1, end = pos - 1), "*")
          #list_file <- dir_ls(path = path_dir(file_tmp), glob = glob)
          list_file <- list.files(path = dirname(file_tmp), pattern = glob)

          #if (is.na(match(x = path_file(file_tmp), table = path_file(list_file)))) {
          if (is.na(match(x = basename(file_tmp), table = basename(list_file)))) {
            exists[rec] <- FALSE
          }
        }
      }

      if (!all(exists)) {
        #if (!quiet) message(str_c("\nfile does not exists '", path_file(file[!exists]), "' (R is case-sensitive)"))
        if (!quiet) message(str_c("\nfile does not exists '", basename(file[!exists]), "' (R is case-sensitive)"))
        exists <- FALSE
      } else {
        exists <- TRUE
      }
    } else {
      exists <- TRUE
    }
  }

  # ---- return of procedure ----

  return(exists)
}
