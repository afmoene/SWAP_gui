#' Download data from URL
#'
#' @param file character string, name of file
#' @param url character string, name of url
#' @param ... further arguments passed to or from other methods
#' @importFrom curl new_handle curl_download
#' @importFrom stringr str_c
#' @export download_data
download_data <- function(file, url, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("handle", "min_size", "max_attempt", "quiet")
  for (name in opt_param) assign(name, NULL)

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(handle)) handle <- new_handle()
  if (is.null(min_size)) min_size <- 2000
  if (is.null(max_attempt)) max_attempt <- 5
  if (is.null(quiet)) quiet <- TRUE


  # ---- main part of procedure ----

  attempt    <- 1
  done <- FALSE
  while (!done) {

    # download requested data
    error <- try(curl_download(url = url, destfile = file, mode = "wb", quiet = TRUE, handle = handle))

    # check if download was succesful
    if (!inherits(error, "try-error")) {

      # download complete (but maybe there was a hickup in the network connection)
      if (file.size(file) > min_size) {

        # download complete
        done <- TRUE
        if (!quiet) message(str_c("file '", basename(file), "' downloaded"))

      } else {

        # delete incomplete file
        unlink(x = file)

        # download failed (try again)
        attempt <- attempt + 1
        Sys.sleep(1)

        # Download failed after maximum attempts
        if (attempt > max_attempt) {
          done <- TRUE
          message(str_c("file '", basename(file), "' failed to download after ", max_attempt, " attemps"))
        }

      }

    }
  }

  if (!done) str_c(stop("\nfailed to download requested data"))
}
