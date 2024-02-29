#' Get value of SWAP-variable
#'
#' @param variable character string, name of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_split str_which str_trim str_replace_all str_detect
#' @importFrom fs path_package path_file
#' @importFrom readr read_rds read_lines
#' @importFrom controlR is_numeric is_date
#' @importFrom lubridate parse_date_time
#' @export get_value_SWAP
#' @examples
#' # specify swp-file
#' file_swp <- paste0(tempfile(pattern = "swap"), "/swap.swp")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(file_swp), exdir = dirname(file_swp))
#'
#' # extract name of project
#' get_value_SWAP(file = file_swp, variable = "PROJECT")
#'
#' # extract number of layers
#' max(get_value_SWAP(file = file_swp, variable = "SOILPROFILE::ISOILLAY"))
#'
#' # extract saturated conductivity
#' get_value_SWAP(file = file_swp, variable = "SOILHYDRFUNC::OSAT")
#'
#' # clean example
#' unlink(x = dirname(file_swp), recursive = TRUE)
get_value_SWAP <- function(variable, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("text", "file", "variable_swap", "item_exists", "case_sensitive", "quiet")
  for (name in opt_param) assign(name, NULL)

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(text) & is.null(file)) stop("either 'text' or 'file' should be specified")
  if (!is.null(text) & !is.null(file)) message("both 'text' and 'file' are specified, only 'text' is used")
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(item_exists)) item_exists <- TRUE
  if (is.null(case_sensitive)) case_sensitive <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # extract list variable
  s_variable_swap <- variable_swap[[variable]]

  # load template
  if (is.null(text)) {
    if (!quiet) message(str_c("reading file: ", path_file(file)))
    text <- read_lines(file = file, lazy = FALSE, progress = FALSE)
    if (!case_sensitive) text <- toupper(text)
  }

  # remove comments
  text <- str_split(string = text, pattern = "[#\\*!]", simplify = TRUE)[, 1]

  # ---- main part of procedure ----

  # get type of variable
  type <- s_variable_swap$type
  if (is.null(type)) type <- "single"

  # strip name variable in case of array
  string <- variable
  if (type == "array") {
    table <- str_split(string = variable, pattern = "::", simplify = TRUE)[, 1]
    header <- variable_swap[[table]]$header
    if (header) {
      string <- str_split(string = variable, pattern = "::", simplify = TRUE)[, 2]
    } else {
      string <- str_split(string = variable, pattern = "::", simplify = TRUE)[, 1]
      variable <- str_split(string = variable, pattern = "::", simplify = TRUE)[, 2]
    }
  }

  # get line with variable
  lns <- str_which(string = text, pattern = str_c(" ", string, " | ", string, "=|^", string, " |^", string, "=| ", string, "$"))
  if (length(lns) == 0) {
    if (item_exists) {
      stop(str_c("unable to find SWAP variable: '", string, "'"))
    } else {
      value <- NULL
      return(value)
    }
  }
  s_text <- str_trim(string = text[lns])

  # get setting in case of single value
  if (type == "single") {

    # get variable setting
    value <- str_trim(str_split(string = s_text, pattern = "=", simplify = TRUE)[, 2])
  }

  # get setting in case of vector
  if (type == "vector") {

    # check if written as a vector
    if (str_detect(string = s_text, pattern = "=")) {

      # get variable setting
      values <- str_split(string = s_text, pattern = "=", simplify = TRUE)[, 2]
      values <- str_trim(string = values)

      # load variables
      value <- str_split(string = values, pattern = " +, +| +,|, +|,| +", simplify = TRUE)[1, ]

    } else {

      # read as array
      type <- "array"
      header <- TRUE

    }
  }

  # get setting in case of array
  if (type == "array") {

    # in case of header
    if (header) {

      # check maximum number of column
      column <- str_split(string = s_text, pattern = " +", simplify = TRUE) [1, ]
      n_column <- length(column)

      # check column number
      i_column <- match(x = string, table = column)

    } else {

      # check maximum number of column
      column <- variable_swap[[table]]$column
      n_column <- length(column)

      # check column number
      i_column <- match(x = variable, table = column)
    }

    # determine end of array
    lns <- lns + 1
    value <- NULL
    found <- FALSE
    while (!found) {

      # trim line
      s_text <- str_trim(string = text[lns])

      # load variables
      values <- str_split(string = s_text, pattern = " +| +,|,", simplify = TRUE)[1, ]

      # check end of array
      if (length(values) != n_column | values[1] == "") {
        found <- TRUE
      } else {
        value <- c(value, values[i_column])
        lns <- lns + 1
      }

      # check end of text
      if (lns > length(text)) found <- TRUE
    }
  }

  # convert format
  if (s_variable_swap$format == "date") {

    if (!s_variable_swap$unit %in% c("yyyy-mm-dd", "dd mm")) stop("unknown date format, check SWAP-variables!")

    if (s_variable_swap$unit == "yyyy-mm-dd") {
      if (!is_date(text = value[1])) stop(str_c("unknown date format found for variable '", string, "'"))
      value <- as_date(parse_date_time(x = value, orders = c("ymd" , "dmy")))
    }

    if (s_variable_swap$unit == "dd mm") {
      splitvalue <- str_split(string = value, pattern = " +", simplify = TRUE)[,1:2]
      value <- str_c(splitvalue[1], " ", splitvalue[2])
    }

  }

  if (s_variable_swap$format == "string") {
    value <- str_replace_all(string = value, pattern = "'|\"", replacement = "")
  }

  if (s_variable_swap$format %in% c("float", "integer", "switch")) {

    # check number of values specified
    if (type == "single" & length(value) > 1) {
      stop(str_c(s_variable_swap$format, " '", string, "' is specified multiple times"))
    }

    # convert to numeric
    if (is_numeric(text = value)[1]) {
      value <- as.numeric(value)
    } else {
      splitvalue <- str_split(string = value, pattern = "d|D", simplify = TRUE)[,1:2]
      value <- as.numeric(splitvalue[1]) * 10^as.numeric(splitvalue[2])
    }
  }

  # ---- return of procedure ----

  return(value)
}

#' Get settings of SWAP-variable(s)
#'
#' @param variable character string, name of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_split
#' @importFrom fs path_package path_file
#' @importFrom readr read_rds read_lines
#' @export get_settings_SWAP
#' @examples
#' # specify swp-file
#' file_swp <- paste0(tempfile(pattern = "swap"), "/swap.swp")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(file_swp), exdir = dirname(file_swp))
#'
#' # extract name of project
#' get_settings_SWAP(file = file_swp, variable = c("PROJECT", "OUTFIL"))
#'
#' # extract number of layers
#' get_settings_SWAP(file = file_swp, variable = "SOILPROFILE::ISOILLAY")
#'
#' # extract saturated conductivity
#' get_settings_SWAP(file = file_swp, variable = c("SOILHYDRFUNC::OSAT", "SOILHYDRFUNC::ORES"))
#'
#' # clean example
#' unlink(x = dirname(file_swp), recursive = TRUE)
get_settings_SWAP <- function(variable, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("text", "file", "variable_swap", "item_exists", "case_sensitive", "quiet")
  text <- file <- variable_swap <- item_exists <- case_sensitive <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(text) & is.null(file)) stop("either 'text' or 'file' should be specified")
  if (!is.null(text) & !is.null(file)) message("both 'text' and 'file' are specified, only 'text' is used")
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(item_exists)) item_exists <- FALSE
  if (is.null(case_sensitive)) case_sensitive <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # load template
  if (is.null(text)) {
    if (!quiet) message(str_c("reading file: ", path_file(file)))
    text <- read_lines(file = file, lazy = FALSE, progress = FALSE)
    if (!case_sensitive) text <- toupper(text)
  }

  # save settings
  settings <- list()
  for (s_variable in variable) {

    # get type of swap-variable
    s_variable_swap <- variable_swap[[s_variable]]
    type <- s_variable_swap$type
    if (is.null(type)) type <- "single"

    # store settings
    name <- s_variable
    if (type == "array") name <- str_split(string = s_variable, pattern = "::", simplify = TRUE)[, 2]
    settings[[name]] <- get_value_SWAP(text = text, variable = s_variable, variable_swap = variable_swap, item_exists = item_exists)
  }

  # ---- return of procedure ----

  return(settings)
}

#' Get variable(s) to set in SWAP-template
#'
#' @param tmplt character string. text of SWAP-template from which names of variable(s) should be extracted.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_which str_replace str_sub
#' @export get_variable_SWAP
get_variable_SWAP <- function(tmplt, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("sign_open", "sign_close")
  sign_open <- sign_close <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"


  # ---- main part of procedure ----

  # select all variables
  lns <- str_which(string = tmplt, pattern = sign_open)

  variable  <- NULL
  for (s_lns in lns) {
    done <- FALSE
    while(!done) {

      # get start and position of variable
      start <- as.numeric(regexpr(pattern = sign_open, text = tmplt[s_lns]))
      if (start == -1) done <- TRUE

      if (!done) {

        # get end position of variable
        pos <- regexpr(pattern = sign_close, text = tmplt[s_lns])
        end <- as.numeric(pos) + (attr(pos,"match.length") - 1)

        # add variable
        s_variable <- str_replace_all(string = str_sub(string = tmplt[s_lns], start = start, end = end), pattern = str_c(sign_open, "|", sign_close), replacement = "")
        variable <- c(variable, s_variable)

        # delete variable from tmplt
        pattern <- str_c(sign_open, s_variable, sign_close)
        tmplt[s_lns] <- str_replace(string = tmplt[s_lns], pattern = pattern, replacement = "")
      }
    }
  }

  # filter switch variables
  done <- FALSE
  while (!done) {

    switch <- str_replace(string = variable[str_sub(string = variable, start = 1, end = 8) == "#SWITCH_"], pattern = "#SWITCH_", replacement = "")[1]
    if (is.na(switch)) done <- TRUE

    if (!done) {
      start <- match(x = str_c("#SWITCH_", switch), table = variable)
      end <- match(x = str_c("/SWITCH_", switch), table = variable)
      if (is.na(end)) stop(str_c("missing end of switch '", switch, "' in template!!!"))
      variable <- c(if (start != 1) variable[1:(start - 1)], if (end != length(variable)) variable[(end + 1):length(variable)])
    }
  }

  # filter tables variables
  table <- str_replace(string = variable[str_sub(string = variable, start = 1, end = 7) == "#TABLE_"], pattern = "#TABLE_", replacement = "")
  for (s_table in table) {
    start <- match(x = str_c("#TABLE_", s_table), table = variable)
    end <- match(x = str_c("/TABLE_", s_table), table = variable)
    if (is.na(end)) stop(str_c("missing end of table '", s_table, "' in template!!!"))
    variable <- c(if (start != 1) variable[1:(start - 1)], s_table, if (end != length(variable)) variable[(end + 1):length(variable)])
  }

  # set unique variable
  variable <- unique(variable)

  # ---- return of procedure ----

  return(variable)
}

#' Get variable(s) in table to set in SWAP-template
#'
#' @param tmplt character string, text of SWAP-template from which names of variable(s) should be extracted.
#' @param table character string, name of table in SWAP-template from which names of variables should be extracted.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_which
#' @keywords internal
#' @export get_table_variable_SWAP
get_table_variable_SWAP <- function(tmplt, table, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("sign_open", "sign_close")
  sign_open <- sign_close <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"

  # ---- main part of procedure ----

  # get start and stop of table
  start  <- str_which(string = tmplt, pattern = str_c(sign_open, "#TABLE_", table, sign_close)) + 1
  stop   <- str_which(string = tmplt, pattern = str_c(sign_open, "/TABLE_", table, sign_close)) - 1

  # get columns
  variable <- get_variable_SWAP(tmplt = tmplt[start:stop], sign_open = sign_open, sign_close = sign_close)

  # ---- return of procedure ----

  return(variable)
}

#' Check value of SWAP-variable
#'
#' @param value value of SWAP-variable.
#' @param variable character string, name of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @keywords internal
check_value_SWAP <- function(value, variable, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- "variable_swap"
  variable_swap <- item_exists <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))

  # ---- main part of procedure ----

  # extract variable
  s_variable_swap <- variable_swap[[variable]]

  # check minimum or maximum date
  if (s_variable_swap$format == "date") {

    if (!s_variable_swap$unit %in% c("yyyy-mm-dd", "dd mm")) stop("unknown date format, check SWAP-variables!")

    if (s_variable_swap$unit == "yyyy-mm-dd") {

      value <- as.Date(value)

      check <- s_variable_swap$start
      if (!is.na(check)) {
        if (!all(value >= check)) {
          stop(str_c("\n", variable, ": date '", value[!is.na(value) & value < check], "' exceeds lower boundary '", check, "'"))
        }
      }

      check <- s_variable_swap$end
      if (!is.na(check)) {
        if (!all(value <= check)) {
          stop(str_c("\n", variable, ": date '", value[!is.na(value) & value > check], "' exceeds upper boundary '", check, "'"))
        }
      }
    }

    if (s_variable_swap$unit == "dd mm") {

      splitvalue <- as.numeric(str_split(string = value, pattern = " +", simplify = TRUE)[,1:2])
      if (splitvalue[1] < 1 | splitvalue[2] < 1) stop(str_c("\n", variable, ": date '", value, "' exceeds lower boundary"))
      if (splitvalue[1] > 31 | splitvalue[2] > 12) stop(str_c("\n", variable, ": date '", value, "' exceeds upper boundary"))

    }
  }

  # check minimum or maximum value
  if (s_variable_swap$format %in% c("float", "integer")) {

    value <- as.numeric(value)

    check <- s_variable_swap$min
    if (!is.na(check)) {
      if (!all(value[!is.na(value)] >= check)) {
        stop(str_c("\n", variable, ": value '", unique(value[value < check]), "' exceeds lower boundary '", check, "'"))
      }
    }
    check <- s_variable_swap$max
    if (!is.na(check)) {
      if (!all(value[!is.na(value)] <= check)) {
        stop(str_c("\n", variable, ": value '", unique(value[value > check]), "' exceeds upper boundary '", check, "'"))
      }
    }
  }

  # check options switch
  if (s_variable_swap$format == "switch") {

    value <- as.numeric(value)

    check <- as.numeric(s_variable_swap$option)
    if (!all(value %in% check)) {
      stop(str_c("\n", variable, ": wrong switch value: '", value[!value %in% check], "'"))
    }
  }

  # ---- return of procedure ----

  return(value)
}

#' Set value of SWAP-variable
#'
#' @param value Value (numeric, date or character string) of SWAP-variable.
#' @param variable character string, name of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_split
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @importFrom controlR get_digits
#' @keywords internal
#' @export set_value_SWAP
set_value_SWAP <- function(value, variable, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- "variable_swap"
  variable_swap <- item_exists <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))


  # ---- main part of procedure ----

  # extract variable
  s_variable_swap <- variable_swap[[variable]]

  # get type of variable
  type <- s_variable_swap$type
  if (is.null(type)) type <- "single"

  # set Digits
  if (s_variable_swap$format == "float") {
    digits <- 1
    digits <- max(digits, get_digits(value = value))
  }

  # set text
  if (s_variable_swap$format == "date")    text <- as.character(value)
  if (s_variable_swap$format == "string")  text <- tolower(formatC(x = str_c("'", value, "'"), format = "s"))
  if (s_variable_swap$format == "switch")  text <- formatC(x = value, format = "d")
  if (s_variable_swap$format == "float")   text <- formatC(x = value, format = "f", digits = digits)
  if (s_variable_swap$format == "integer") text <- formatC(x = value, format = "d")

  # combine values of vector
  if (type == "vector") {
    text <- str_c(text, collapse = " ")
  }

  # set width in case of table
  if (type == "array") {

    # extract table and variable
    table <- str_split(string = variable, pattern = "::", simplify = TRUE) [, 1]
    variable <- str_split(string = variable, pattern = "::", simplify = TRUE) [, 2]

    if (variable_swap[[table]]$header) {
      text <- c(variable, as.character(text))
      width <- max(nchar(text)) + 1
      text <- formatC(x = text, format = "s", width = width)
      text <- str_replace_all(string = text, pattern = str_c(formatC(x = " ", format = "s", width = width - 2), "NA"), replacement = formatC(x = "", format = "s", width = width))
    }
  }

  # ---- return of procedure ----

  return(text)
}

#' Set variable(s) in SWAP-template
#'
#' @param tmplt character string, text of SWAP-template from which names of variable(s) should be extracted.
#' @param variable character string, name of SWAP variable.
#' @param value numeric, date or string, value of variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_replace
#' @importFrom readr read_rds
#' @keywords internal
#' @export set_variable_SWAP
set_variable_SWAP <- function(tmplt, variable, value, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("variable_swap", "sign_open", "sign_close")
  variable_swap <- sign_open <- sign_close <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"


  # ---- main part of procedure ----

  # set index
  lns <- 1:length(tmplt)

  # filter tables variables
  start <- str_which(string = tmplt, pattern = "#TABLE_")
  if (length(start) > 0) {
    end <- str_which(string = tmplt, pattern = "/TABLE_")
    for (tbl in 1:length(start)) {
      lns <- lns[!lns %in% seq(from = start[tbl], to = end[tbl], by = 1)]
    }
  }

  # set variable
  value <- check_value_SWAP(value = value, variable = variable, variable_swap = variable_swap)
  replacement <- set_value_SWAP(value = value, variable = variable, variable_swap = variable_swap)
  tmplt[lns] <- str_replace(string = tmplt[lns], pattern = str_c(sign_open, variable, sign_close), replacement = replacement)

  # ---- return of procedure ----

  return(tmplt)
}

#' Set table in SWAP-template
#'
#' @param tmplt character string, text of SWAP-template from which names of variable(s) should be extracted.
#' @param table character string, name of table in SWAP-template from which names of variables should be extracted.
#' @param value table, value of variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_which
#' @importFrom readr read_rds
#' @keywords internal
#' @export set_table_SWAP
set_table_SWAP <- function(tmplt, table, value, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("variable_swap", "sign_open", "sign_close")
  variable_swap <- sign_open <- sign_close <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"

  # ---- main part of procedure ----

  # get columns to select
  table_variable <- get_table_variable_SWAP(tmplt = tmplt, table = table, sign_open = sign_open, sign_close = sign_close)

  # convert to table text
  replacement <- NULL
  if (length(table_variable) == 1) {

    # extract values and check boundaries
    s_value <- value[,table_variable]
    s_value <- check_value_SWAP(value = s_value, variable = str_c(table, "::", table_variable), variable_swap = variable_swap)

    # set vector
    string <- set_value_SWAP(value = s_value, variable = str_c(table, "::", table_variable), variable_swap = variable_swap)
    replacement <- str_c("  ", table_variable, " = ", str_c(string[2:length(string)], collapse = " "))

  } else {
    replacement <- NULL
    for (s_table_variable in table_variable) {

      # extract values
      s_value <- value[,s_table_variable]
      if (!all(is.na(s_value))) {

        # check boundaries
        s_value <- check_value_SWAP(value = s_value, variable = str_c(table, "::", s_table_variable), variable_swap = variable_swap)

        # set table
        string <- set_value_SWAP(value = s_value, variable = str_c(table, "::", s_table_variable), variable_swap = variable_swap)

        # combine text
        if (is.null(replacement)) {
          replacement <- string
        } else {
          replacement <- str_c(replacement, string, sep = " ")
        }
      }
    }
  }

  # set text
  start <- str_which(string = tmplt, pattern = str_c(sign_open, "#TABLE_", table, sign_close))
  end <- str_which(string = tmplt, pattern = str_c(sign_open, "/TABLE_", table, sign_close))
  tmplt <- c(if (start != 1) tmplt[1:start - 1], replacement, if (end != length(tmplt)) tmplt[(end + 1):length(tmplt)])

  # ---- return of procedure ----

  return(tmplt)
}

#' Set switch in SWAP-template
#'
#' @param tmplt character string, text of SWAP-template from which names of variable(s) should be extracted.
#' @param switch character string, name of switch in SWAP-template.
#' @param value integer, value of switch.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_which str_replace_all
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @keywords internal
#' @export set_switch_SWAP
set_switch_SWAP <- function(tmplt, switch, value, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("variable_swap", "sign_open", "sign_close")
  variable_swap <- sign_open <- sign_close <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"

  # extract switch information
  s_variable_swap <- variable_swap[[switch]]
  option <- as.numeric(s_variable_swap$option)


  # ---- main part of procedure ----

  # set switch
  value <- check_value_SWAP(value = value, variable = switch, variable_swap = variable_swap)
  replacement <- set_value_SWAP(value = value, variable = switch, variable_swap = variable_swap)
  tmplt <- str_replace_all(string = tmplt, pattern = str_c(sign_open, switch, sign_close), replacement = replacement)

  # delete unused options
  for (s_option in option) {

    done <- FALSE
    while (!done) {

      # get lines of start switch
      start <- str_which(string = tmplt, pattern = str_c(sign_open, "#SWITCH_", switch, "_OPTION_", s_option, sign_close))[1]
      if (is.na(start[1])) done <- TRUE

      if (!done) {

        # get lines of end switch
        end <- str_which(string = tmplt, pattern = str_c(sign_open, "/SWITCH_", switch, "_OPTION_", s_option, sign_close))[1]
        if (is.na(end[1])) stop(str_c("unable to delete option: '", s_option, "' of SWITCH '", switch, "'"))

        # clean or delete option from template
        if (s_option == value) {

          # clean option
          tmplt <- c(tmplt[1:(start - 1)], tmplt[(start + 1):(end - 1)],if (end != length(tmplt)) tmplt[(end + 1):length(tmplt)])

        } else {

          # delete option
          tmplt <- c(tmplt[1:(start - 1)],if (end != length(tmplt)) tmplt[(end + 1):length(tmplt)])
        }
      }
    }
  }

  # ---- return of procedure ----

  return(tmplt)
}

#' Set SWAP-template
#'
#' @param template character string, name of SWAP-template.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_sub
#' @importFrom fs path_package path_file
#' @importFrom readr read_rds read_lines
#' @importFrom RSQLite dbConnect SQLite dbDisconnect SQLITE_RO
#' @keywords internal
#' @export set_template_SWAP
set_template_SWAP <- function(template, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("run_info", "file", "variable_swap", "sign_open", "sign_close", "case_sensitive", "quiet")
  run_info <- file <- variable_swap <- sign_open <- sign_close <- case_sensitive <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(sign_open)) sign_open <- "\\{\\{"
  if (is.null(sign_close)) sign_close <- "\\}\\}"
  if (is.null(case_sensitive)) case_sensitive <- TRUE
  if (is.null(quiet)) quiet <- FALSE

  # check arguments
  if (!is.null(run_info) & !is.null(file)) stop("either 'run_info' or 'file' should be specified")
  if (is.null(run_info) & is.null(file)) stop("'run_info' or 'file' should be specified")

  # open connection
  if (!is.null(run_info)) conn <- dbConnect(RSQLite::SQLite(), dbname = run_info$file_sql, flags = RSQLite::SQLITE_RO)
  if (!is.null(file)) {
    text <- read_lines(file = file, lazy = FALSE, progress = FALSE)
    if (!case_sensitive) text <- toupper(text)
  }

  # ---- main part of procedure ----

  # load template
  if (!quiet) message(str_c("reading file: ", path_file(template)))
  tmplt <- read_lines(file = template, lazy = FALSE, progress = FALSE)

  # get variables
  variable <- get_variable_SWAP(tmplt = tmplt, sign_open = sign_open, sign_close = sign_close)

  # loop over switches
  done <- ifelse(is.null(variable), TRUE, FALSE)
  while (!done) {

    done <- TRUE

    for (s_variable in variable) {

      # extract variable
      s_variable_swap <- variable_swap[[s_variable]]
      if (is.null(s_variable_swap)) stop(str_c("unknown variable found in template '", s_variable,"'"))
      format <- s_variable_swap$format

      # set value (replacement)
      if (format == "table") {

        # get columns of table
        table_variable <- get_table_variable_SWAP(tmplt = tmplt, table = s_variable, sign_open = sign_open, sign_close = sign_close)

        # load settings
        value <- NULL
        for (s_table_variable in table_variable) {
          if (!is.null(run_info)) {
            values <- get_value_SQL(conn = conn, run_info = run_info, variable = str_c(s_variable, "::", s_table_variable), variable_swap = variable_swap)
          } else {
            values <- get_value_SWAP(text = text, variable = str_c(s_variable, "::", s_table_variable), variable_swap = variable_swap, case_sensitive = case_sensitive)
          }
          if (is.null(value)) {
            value <- data.frame(Values = values)
            names(value) <- s_table_variable
          } else {
            value[, s_table_variable] <- values
          }
        }

      } else {

        # load settings
        if (!is.null(run_info)) {
          value <- get_value_SQL(conn = conn, run_info = run_info, variable = s_variable, variable_swap = variable_swap)
        } else {
          value <- get_value_SWAP(text = text, variable = s_variable, variable_swap = variable_swap, case_sensitive = case_sensitive)
        }
      }

      # in case of switch
      if (format == "switch") {

        done <- FALSE

        # clear option
        tmplt <- set_switch_SWAP(tmplt = tmplt, switch = s_variable, value = value, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close)
      }

      # in case of table
      if (format == "table") {
        tmplt <- set_table_SWAP(tmplt = tmplt, table = s_variable, value = value, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close)
      }

      # in case of variable
      if (format %in% c("date", "float", "integer", "string")) {
        tmplt <- set_variable_SWAP(tmplt = tmplt, variable = s_variable, value = value, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close)
      }
    }

    # update variables
    variable <- get_variable_SWAP(tmplt = tmplt, sign_open = sign_open, sign_close = sign_close)
  }

  # adjust comment to minimum of 30
  pos <- regexpr(text = tmplt, pattern = "!")
  lns <- grep(x = pos > 1 & pos < 30, pattern = TRUE)
  for (s_lns in lns) {
    start <- str_sub(string = tmplt[s_lns], start = 1, end = pos[s_lns] - 1)
    add <- str_c(rep(x = " ", times = 30 - pos[s_lns]), collapse = "")
    end <- str_sub(string = tmplt[s_lns], start = pos[s_lns] + 1, end = nchar(tmplt[s_lns]))
    tmplt[s_lns] <- str_c(start, add, "!", end)
  }

  # ---- return of procedure ----

  # close connection
  if (!is.null(run_info)) dbDisconnect(conn = conn)

  return(tmplt)
}
