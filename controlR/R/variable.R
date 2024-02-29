#' Get variable(s) from text
#'
#' @param ... further arguments passed to or from other methods.
#' @importFrom stringr str_c str_which str_replace str_sub
#' @importFrom readr read_lines
#' @export get_variable
#' @details either \code{text} or \code{file} should be specified.
#' @examples
#' # specify controlfile
#' file <- system.file("extdata/control.inp", package = "controlR")
#'
#' # collect variable in controlfile
#' get_variable(file = file)
get_variable <- function(...) {

  # ---- initial part of procedure ----

  variable <- NULL

  # set optional arguments
  opt_param <- c("text", "file", "sign_open", "sign_close")
  text <- file <- sign_open <- sign_close <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(text) & is.null(file)) stop("either 'text' or 'file' should be specified")
  if (!is.null(text) & !is.null(file)) message("both 'text' and 'file' are specified, only 'text' is used")
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"

  # ---- main part of procedure ----

  # read file (optional)
  if (is.null(text)) {
    if (file_exists(file = file)) text <- read_lines(file = file, progress = FALSE, lazy = FALSE)
  }

  # select all variables
  lns <- str_which(string = text, pattern = sign_open)

  # loop over lines
  for (s_lns in lns) {
    done <- FALSE
    while(!done) {

      # get start and position of variable
      start <- as.numeric(regexpr(pattern = sign_open, text = text[s_lns]))
      if (start == -1) done <- TRUE

      if (!done) {

        # get end position of variable
        pos <- regexpr(pattern = sign_close, text = text[s_lns])
        end <- as.numeric(pos) + (attr(pos,"match.length") - 1)

        # add variable
        s_variable <- str_replace_all(string = str_sub(string = text[s_lns], start = start, end = end), pattern = str_c(sign_open, "|", sign_close), replacement = "")
        variable <- c(variable, s_variable)

        # delete variable from text
        pattern <- str_c(sign_open, s_variable, sign_close)
        text[s_lns] <- str_replace(string = text[s_lns], pattern = pattern, replacement = "")
      }
    }
  }

  # set unique variable
  variable <- unique(variable)

  # ---- return of procedure ----

  return(variable)
}
