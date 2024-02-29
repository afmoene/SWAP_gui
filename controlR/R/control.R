#' Get line in text containing item
#'
#' @param text vector of character strings.
#' @param item character string, keyword of which linenumber should be returned.
#' @param ... further arguments passed to or from other methods.
#' @importFrom stringr str_which str_replace_all str_c
#' @return integer, line in text with containing \code{item}.
#' @export get_line
#' @keywords internal
#' @examples
#' # specify controlfile
#' file <- system.file("extdata/control.inp", package = "controlR")
#' text <- readLines(con = file)
#'
#' # get line containing 'FILE' in controlfile
#' get_line(text = text, item = "FILE")
get_line <- function(text, item, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("item_exists", "multi_lines")
  item_exists <- multi_lines <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(item_exists)) item_exists <- TRUE
  if (is.null(multi_lines)) multi_lines <- FALSE

  # ---- main part of procedure ----

  # replace tabs in text
  text <- str_replace_all(string = text, pattern = "\t", replacement = " ")

  # search item in text
  line <- str_which(string = text, pattern = str_c("^ *", item, "$|^ *", item, " +.+$"))

  # ---- return of procedure ----

  if (item_exists & length(line) == 0) stop(paste0("item not found in file, item: ", item))
  if (!multi_lines & length(line) > 1) stop(paste0("multiple records found with item: ", item))
  if (length(line) == 0) line <- NULL

  return(line)
}

#' Get record specified by item
#'
#' @param item character string, keyword to specify record.
#' @param ... further arguments passed to or from other methods.
#' @importFrom stringr str_split str_trim str_replace_all str_c
#' @importFrom readr read_lines
#' @importFrom lubridate as_date parse_date_time
#' @return character string, date or value specified by \code{item}.
#' @export get_record
#' @description Get record from controlfile. The controlfile is specified by either \code{text} or \code{file}.
#' The record to extract from the controlfile is specified by keyword \code{item}.
#' @details either \code{text} or \code{file} should be specified.
#' @examples
#' # specify controlfile
#' file <- system.file("extdata/control.inp", package = "controlR")
#'
#' # get record specified by 'FILE' in controlfile
#' get_record(file = file, item = "FILE")
#'
#' # get record specified by 'FILE' in controlfile
#' text <- readLines(con = file)
#' get_record(text = text, item = "FILE")
#'
#' # get record specified by 'VALUE' in controlfile
#' get_record(file = file, item = "VALUE")
#'
#' # get record specified by 'DATE' in controlfile
#' get_record(file = file, item = "DATE")
get_record <- function(item, ...) {

  # ---- initial part of procedure ----

  record <- NULL

  # set optional arguments
  opt_param <- c("text", "file", "item_exists", "opt")
  text <- file <- item_exists <- opt <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(text) & is.null(file)) stop("either 'text' or 'file' should be specified")
  if (!is.null(text) & !is.null(file)) message("both 'text' and 'file' are specified, only 'text' is used")
  if (is.null(item_exists)) item_exists <- TRUE

  # ---- main part of procedure ----

  # read file (optional)
  if (is.null(text)) {
    if (file_exists(file = file)) text <- read_lines(file = file, progress = FALSE, lazy = FALSE)
  }

  # get line
  line <- get_line(text = text, item = item, item_exists = item_exists, multi_lines = FALSE)
  if (!is.null(line)) {

    # filter line
    s_text <- text[line]

    # remove comment
    s_text <- str_split(string = s_text, pattern = "[#\\*!]", simplify = TRUE)[, 1]

    # catch record
    record <- str_trim(string = str_split(string = s_text, pattern = item, n = 2, simplify = TRUE)[, 2])

    # check record
    if (!is.null(opt)) {
      if (!record %in% opt) {
        stop(str_c("item: '",item,"' should have one of the following options: '", str_c(opt, collapse = "' '"), "'"))
      }
    }
  }

  # ---- return of procedure ----

  # set class
  if (!is.null(record)) {
    record <- str_replace_all(string = record, pattern = "\"", replacement = "")
    if (is_numeric(text = record)) record <- as.numeric(record)
    if (is_date(text = record)) record <- as_date(parse_date_time(x = record, orders = c("ymd" , "dmy")))
  }

  return(record)
}

#' Get directory specified by item
#'
#' @param item character string, keyword to specify record.
#' @param ... further arguments passed to or from other methods.
#' @importFrom stringr str_replace str_sub
# @importFrom fs dir_create
#' @return Character string, name of directory
#' @export get_dir
#' @description Get directory from controlfile. The controlfile is specified by either \code{text} or \code{file}.
#' The directory to extract from the controlfile is specified by keyword \code{item}. Optionally the directory is created (\code{create}).
#' @details either \code{text} or \code{file} should be specified.
#' @examples
#' # specify controlfile
#' file <- system.file("extdata/control.inp", package = "controlR")
#'
#' # get directory specified by 'DIROUT' in controlfile
#' get_dir(file = file, item = "DIROUT", create = FALSE)
get_dir <- function(item, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("text", "file", "item_exists", "create")
  text <- file <- item_exists <- create <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(text) & is.null(file)) stop("either 'text' or 'file' should be specified")
  if (is.null(item_exists)) item_exists <- TRUE
  if (is.null(create)) create <- TRUE

  # ---- main part of procedure ----

  dir <- get_record(text = text, file = file, item = item, item_exists = item_exists)

  # check dir (if exists)
  if (!is.null(dir)) {

    # replace backslash
    dir <- str_replace(string = dir, pattern = "\\\\", replacement = "/")

    # remove laste backslash
    if (str_sub(string = dir, start = nchar(dir), end = nchar(dir)) == "/") {
      dir <- str_sub(string = dir, start = 1, end = (nchar(dir) - 1))
    }

    # create directory
    #if (create) dir_create(path = dir)
    if (create) dir.create(path = dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- return of procedure ----

  return(dir)
}

#' Get table specified by item
#'
#' @param item character string, keyword to specify table.
#' @param ... further arguments passed to or from other methods.
#' @importFrom stringr str_detect str_split str_trim str_replace_all str_c str_locate_all
#' @importFrom readr read_lines
#' @return \code{\link{data.frame}}, table specified by \code{item}.
#' @export get_table
#' @description Get table from controlfile. The controlfile is specified by either \code{text} or \code{file}.
#' The table to extract from the controlfile is specified by keyword \code{item}.
#' @details either \code{text} or \code{file} should be specified.
#' @examples
#' # specify controlfile
#' file <- system.file("extdata/control.inp", package = "controlR")
#'
#' # get table specified by 'VALUES' in controlfile
#' get_table(file = file, item = "VALUES", header = "VALUES")
get_table <- function(item, ...) {

  # ---- initial part of procedure ----

  table <- NULL

  # set optional arguments
  opt_param <- c("text", "file", "item_exists", "opt", "header")
  text <- file <- item_exists <- opt <- header <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(text) & is.null(file)) stop("either 'text' or 'file' should be specified")
  if (is.null(item_exists)) item_exists <- TRUE

  # ---- main part of procedure ----

  # read file (optional)
  if (is.null(text)) {
    if (file_exists(file = file)) text <- read_lines(file = file, progress = FALSE, lazy = FALSE)
  }

  line_start <- get_line(text = text, item = str_c("Table ", item), item_exists = item_exists)
  if (!is.null(line_start)) {

    line_end <- get_line(text = text, item = "End Table", multi_lines = TRUE)
    if (is.null(line_end)) stop(str_c("item 'Table ",item,"' should end with 'End Table'"))
    line_end <- line_end[line_end > line_start] [1]
    if (is.na(line_end)) stop(str_c("item 'Table ",item,"' should end with 'End Table'"))

    s_text <- text[(line_start + 1):(line_end - 1)]
    s_text <- s_text[!str_detect(string = s_text, pattern = "^ *[#\\*!]+|^$")]
    s_text <- str_split(string = s_text, pattern = "[#\\*!]", n = 2, simplify = TRUE)[, 1]
    s_text <- str_trim(string = s_text)
    s_text <- str_replace_all(string = s_text, pattern = "\t", replacement = " ")

    # replace spaces within "" by #
    locate <- str_locate_all(string = s_text, pattern = "\"|\'")
    for (line in 1:length(s_text)) {
      pos <- locate[[line]][, "start"]
      if (length(pos) != 0) {
        if (length(pos) %% 2 != 0) stop("unclosed character string")
        for (x in 1:(length(pos) / 2)) {
          pos_start <- pos[(x - 1) * 2 + 1]
          pos_end <- pos[(x - 1) * 2 + 2]
          replace <- str_replace_all(string = str_sub(string = s_text[line], start = pos_start + 1, end = pos_end - 1), pattern = " ", replacement = "#")
          s_text[line] <- str_c(str_sub(string = s_text[line], start = 1, end = pos_start), replace, str_sub(string = s_text[line], start = pos_end, end = nchar(s_text[line])))
        }
      }
    }

    table <- as.data.frame(str_split(string = s_text, pattern = " +", simplify = TRUE), stringsAsFactors = FALSE)

    # reset spaces within ""
    for (s_col in 1:ncol(table)) {
      table[, s_col] <- str_replace_all(string = table[, s_col], pattern = "#", replacement = " ")
      table[, s_col] <- str_replace_all(string = table[, s_col], pattern = "\"", replacement = "")
    }

    # check record
    if (!is.null(opt)) {
      if (!all(table$V1 %in% opt)) {
        stop(str_c("table: '",item,"' should contain one of the following options: '", str_c(opt, collapse = "' '"), "'"))
      }
    }

    # add header
    if (!is.null(header)) {
      names(table) <- header
    }
  }

  # ---- return of procedure ----

  # set class
  if (!is.null(table)) {
    for (s_col in 1:ncol(table)) {
      text <- table[1, s_col]
      if (is_numeric(text = text)) table[, s_col] <- as.numeric(table[, s_col])
      if (is_date(text = text)) table[, s_col] <- as_date(parse_date_time(x = table[, s_col], orders = c("ymd" , "dmy")))
    }
  }

  return(table)
}


