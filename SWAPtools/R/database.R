#' Create sql-database from xls-file (SWAP-variables)
#'
#' @param file_xls character string, name of xls-file.
#' @param file_sql character string, name of sql-file.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_split str_detect
#' @importFrom fs path_package file_exists file_delete
#' @importFrom readr read_rds
#' @importFrom RSQLite dbConnect SQLite dbGetQuery dbWriteTable dbExecute dbDisconnect SQLITE_RWC
#' @importFrom readxl excel_sheets read_excel cell_rows
#' @importFrom tibble deframe
#' @importFrom dplyr %>%
#' @importFrom progress progress_bar
#' @export create_database_SWAP
create_database_SWAP <- function(file_xls, file_sql, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("variable_input", "variable_output", "quiet")
  variable_input <- variable_output <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_input)) variable_input <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(variable_output)) variable_output <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(quiet)) quiet <- FALSE

  # ---- main part of procedure ----

  # collect SWAP input and output variables
  variable_inp <- names(variable_input)
  for (rec in str_which(string = variable_inp, pattern = "::")) {
    variable_inp[rec] <- str_split(string = variable_inp[rec], pattern = "::", simplify = TRUE)[, 2]
  }
  variable_out <- names(variable_output)

  # delete old database
  if (file_exists(path = file_sql)) file_delete(path = file_sql)

  # open connection
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql, flags = RSQLite::SQLITE_RWC)

  # load Excel workbook
  sh <- excel_sheets(path = file_xls)
  s_sh <- sh[str_detect(string = sh, pattern = " ")]
  if (length(s_sh) != 0) stop(str_c("remove whitespace from worksheet: ", str_c("'", s_sh, "'", collapse = " and ")))

  # loop over sheets and collect input variabes
  if (!quiet) pb <- progress_bar$new(total = length(sh), format = "create SQL-database: load sheet :percent (:what)", clear = TRUE)
  for (s_sh in sh) {

    # set progress
    if (!quiet) pb$tick(tokens = list(what = s_sh))

    # load sheet columns
    #dat <- suppressMessages(read_excel(path = file_xls, sheet = s_sh))

    column <- suppressMessages(read_excel(path = file_xls, sheet = s_sh, range = cell_rows(1))) %>%
      names()

    # set variables
    if (s_sh == "Observed") {
      var_SWAP <- variable_output
      variable <- variable_out
    } else {
      var_SWAP <- variable_input
      variable <- variable_inp
    }

    # set format for loading sheet
    n_clm <- length(column)
    col_types <- rep(x = "guess", times = n_clm)
    for (s_clm in 1:n_clm) {

      s_column <- column[s_clm]

      # strip possible depth indication
      t_column <- s_column
      if (str_detect(string = t_column, pattern = "\\[")) {
        t_column <- str_split(string = t_column, pattern = "\\[", simplify = TRUE)[, 1]
      }

      # delete column in if not SWAP variable or index-column
      if (regexpr(pattern = "_id", text = s_column) < 0 & !t_column %in% variable) {
        col_types[s_clm] <- "skip"
      }

      # convert datetime to date
      if (s_column %in% variable) {
        format <- var_SWAP[[match(x = s_column, table = variable)]]$format
        if (format %in% c("float", "integer")) col_types[s_clm] <- "numeric"
        if (format == "date") col_types[s_clm] <- "date"
        if (format == "string") col_types[s_clm] <- "text"
      }
    }

    # load data sheet
    db <- suppressMessages(read_excel(path = file_xls, sheet = s_sh, col_types = col_types))

    # delete empty columns and check date format
    column <- names(db)
    for (s_column in column) {

      # delete column if all records are empty
      if (all(is.na(db[ , s_column]))) {
        db <- db[ , names(db)[names(db) != s_column]]
      }

      # convert datetime to date
      if (s_column %in% variable) {
        format <- var_SWAP[[match(x = s_column, table = variable)]]$format
        if (format == "date") db[, s_column] <- as_date(deframe(db[, s_column]))
      }
    }

    # # delete empty columns
    # column <- names(dat)
    # for (s_column in column) {
    #
    #   # delete column if all records are empty
    #   if (all(is.na(dat[ , s_column]))) {
    #     dat <- dat[ , names(dat)[names(dat) != s_column]]
    #   }
    #
    #   # strip possible depth indication
    #   t_column <- s_column
    #   if (str_detect(string = t_column, pattern = "\\[")) {
    #     t_column <- str_split(string = t_column, pattern = "\\[", simplify = TRUE)[, 1]
    #   }
    #
    #   # delete column in if not SWAP variable or index-column
    #   if (regexpr(pattern = "_id", text = s_column) < 0 & !t_column %in% variable) {
    #     dat <- dat[ , names(dat)[names(dat) != s_column]]
    #   }
    #
    #   # convert datetime to date
    #   if (s_column %in% variable) {
    #     format <- var_SWAP[[match(x = s_column, table = variable)]]$format
    #     if (format == "date") {
    #       dat[, s_column] <- as_date(deframe(dat[, s_column]))
    #     }
    #   }
    # }

    # upload sheet
    if (ncol(db) != 0) {
      dbWriteTable(conn = conn, name = s_sh, value = db)

      # create index
      index <- names(db)[regexpr(pattern = "_id", text = names(db)) > 0]
      if (length(index) == 0) stop(str_c("upload sheet '", s_sh, "' failed!!!\nNo key variables specified"))
      statement <- str_c("CREATE INDEX swap_idx_", s_sh, " ON ", s_sh, " (", str_c(index, collapse = ", "), ")")
      dbExecute(conn = conn, statement = statement)
    }
  }

  # close connection
  dbDisconnect(conn = conn)
}

#' Get run_id from SQL
#'
#' @param file_sql character string, name of sql-file.
#' @importFrom RSQLite dbConnect SQLite dbGetQuery dbDisconnect SQLITE_RO
#' @importFrom stringr str_c
#' @importFrom fs path_file
#' @export get_run_id_SQL
get_run_id_SQL <- function(file_sql) {

  # ---- main part of procedure ----

  # open connection SQL
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql, flags = RSQLite::SQLITE_RO)

  # check for table 'Runs'
  table <- dbListTables(conn = conn)
  if (!"Runs" %in% table) stop(str_c("unable to extract run information\nTable 'Runs' not found in database ('", path_file(file_sql), "')"))

  # extract run information
  statement <- str_c("SELECT run_id FROM Runs")
  dat <- dbGetQuery(conn = conn, statement = statement)
  run_id <- sort(dat$run_id)

  # close connection SQL
  dbDisconnect(conn = conn)

  # ---- return part of procedure ----

  return(run_id)
}

#' Filter run_id from sql-database
#'
#' @param file_sql character string, name of sql-file.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom fs file_exists
#' @importFrom RSQLite dbConnect dbGetQuery dbDisconnect
#' @export filter_run_id_SQL
filter_run_id_SQL <- function(file_sql, ...) {

  # ---- initial part of procedure ----

  # load additional arguments
  param <- list(...)
  if (length(param) == 0) stop("no additional arguments are specified")
  for (name in names(param)) assign(name, param[[name]])

  # ---- main part of procedure ----

  if (!file_exists(path = file_sql)) stop(str_c("sql-file '", file_sql,"' does not exists"))

  # create statement
  statement <- "SELECT run_id FROM Runs"
  first <- TRUE
  for (name in names(param)) {

    if (first) {
      first <- FALSE
      statement <- str_c(statement, " WHERE ", name, " in ('", str_c(param[[name]], collapse = "','"), "')")
    } else {
      statement <- str_c(statement, " AND ", name, " in ('", str_c(param[[name]], collapse = "','"), "')")
    }
  }

  # select variants from wwl-database
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  db_run <- dbGetQuery(conn = conn, statement = statement)
  dbDisconnect(conn = conn)

  # set run_id
  if (nrow(db_run) != 0) {
    run_id <- db_run$run_id
  } else {
    run_id <- NULL
  }

  # ---- return of procedure ----

  return(run_id)
}

#' Extract swap-files from zip
#'
#' @param run_id vector, run id.
#' @param dir_run character string, directory to extract file.
#' @param dir_out character string, directory to extract file.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_replace
#' @importFrom fs file_exists
#' @importFrom controlR get_variable
#' @importFrom utils unzip
#' @export extract_run_id
extract_run_id <- function(run_id, dir_run, dir_out, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("file_sql", "sign_open", "sign_close")
  file_sql <- sign_open <- sign_close <- NULL

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

  # check run-directory
  key_id <- get_variable(text = dir_run, sign_open = sign_open, sign_close = sign_close)
  if (!is.null(key_id) & is.null(file_sql)) stop(str_c("in case of {{key}} in dir_run, file_sql should be specified"))

  # loop over run_id
  for (s_run_id in run_id) {

    # set run-directory
    dir_tmp <- dir_run
    if (!is.null(key_id)) {
      run_info <- get_run_info_SQL(file_sql = file_sql, run_id = s_run_id)
      for (s_key_id in key_id) {
        dir_tmp <- str_replace(string = dir_tmp, pattern = str_c(sign_open, s_key_id, sign_close), replacement = as.character(run_info[[s_key_id]]))
      }
    }

    # set zip-file
    file_zip <- str_c(dir_tmp, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"), ".zip")

    # extract all files if exists
    if (file_exists(path = file_zip)) {
      unzip(zipfile = file_zip, overwrite = TRUE, exdir = str_c(dir_out, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0")))
    }
  }
}

#' Get run_info from SQL
#'
#' @param file_sql character string, name of sql-file.
#' @param run_id numeric, id of run.
#' @importFrom RSQLite dbConnect SQLite dbGetQuery dbDisconnect SQLITE_RO
#' @importFrom stringr str_c
#' @importFrom fs path_file
#' @export get_run_info_SQL
get_run_info_SQL <- function(file_sql, run_id) {

  # ---- main part of procedure ----

  # open connection SQL
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql, flags = RSQLite::SQLITE_RO)

  # check for table 'Runs'
  table <- dbListTables(conn = conn)
  if (!"Runs" %in% table) stop(str_c("unable to extract run information\nTable 'Runs' not found in database ('", path_file(file_sql), "')"))
  field <- dbListFields(conn = conn, name = "Runs")

  # extract run information
  statement <- str_c("SELECT ", str_c(field, collapse = ","), " FROM Runs WHERE run_id in ('", run_id, "')")
  dat <- dbGetQuery(conn = conn, statement = statement)

  # close connection SQL
  dbDisconnect(conn = conn)

  # check results query
  if (nrow(dat) == 0) stop(str_c("no record found for run_id '", run_id, "'"))
  if (nrow(dat) > 1) stop(str_c("multiple records found for run_id '", run_id, "'"))

  # store sql-file used
  run_info <- list()
  run_info[["modus"]] <- "SQL"
  run_info[["file_sql"]] <- file_sql

  # store run information
  column <- names(dat)
  for (s_column in column) {
    if (!is.na(dat[, s_column])) run_info[[s_column]] <- dat[, s_column]
  }

  # ---- return part of procedure ----

  return(run_info)
}

#' Get value of variable from SQL
#'
#' @param conn connection to sql-file.
#' @param run_info list, index of run_id.
#' @param variable character string, name of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds
#' @importFrom fs path_package
#' @importFrom stringr str_c str_split
#' @importFrom RSQLite dbListTables dbListFields dbGetQuery
#' @importFrom lubridate as_date
get_value_SQL <- function(conn, run_info, variable, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("variable_swap" ,"item_exists")
  variable_swap <- item_exists <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(variable_swap)) variable_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_variables.rds"))
  if (is.null(item_exists)) item_exists <- TRUE

  # extract list variable
  s_var_SWAP <- variable_swap[[variable]]

  # ---- main part of procedure ----

  # extract names of tables
  table_SQL <- dbListTables(conn = conn)

  # get type of variable
  type <- s_var_SWAP$type
  if (is.null(type)) type <- "single"

  # set index of variable (needed for vector or array)
  index <- NULL
  decreasing <- NULL
  if (type == "vector") {

    # set variable index
    index <- s_var_SWAP$index
    decreasing <- FALSE
    if (!is.null(s_var_SWAP$order)) {
      decreasing <- ifelse(s_var_SWAP$order == "decreasing", TRUE, FALSE)
    }

  }
  table <- NULL
  if (type == "array") {

    # extract table and variable
    table <- str_split(string = variable, pattern = "::", simplify = TRUE) [, 1]
    variable <- str_split(string = variable, pattern = "::", simplify = TRUE) [, 2]

    # set variable index
    index <- variable_swap[[table]]$index
    decreasing <- FALSE
    if (!is.null(variable_swap[[table]]$order)) {
      decreasing <- ifelse(variable_swap[[table]]$order == "decreasing", TRUE, FALSE)
    }
  }

  # set columns to extract (set dummy variable in case of non-unique index)
  if (is.null(index)) {
    column <- variable
  } else {
    if (index != variable) {
      column <- c(index, variable)
    } else {
      if (!is.null(table)) {
        dummy_variable <- variable_swap[[table]]$column[variable_swap[[table]]$column != index][1]
        column <- c(index, dummy_variable)
      } else {
        column <- variable
      }
    }
  }

  # search for table in SQL database
  tbl <- 0
  done  <- FALSE
  while (!done) {

    tbl <- tbl + 1

    # extract fields of table
    field <- dbListFields(conn = conn, name = table_SQL[tbl])
    if (all(column %in% field)) {

      # check table index
      table_index <- field[regexpr(pattern = "_id", text = field) > 0]
      if (all(table_index %in% names(run_info))) {

        # extract variable from table
        where <- NULL
        for (rec in 1:length(table_index)) where <- c(where, str_c(table_index[rec], " in ('", run_info[[table_index[rec]]], "')"))
        statement <- str_c("SELECT ",str_c(column, collapse = ","), " FROM ", table_SQL[tbl], " WHERE ", str_c(where, collapse = " AND "))
        value <- dbGetQuery(conn = conn, statement = statement)
        if (nrow(value) > 0) done <- TRUE
      }
    }

    # check if done
    if (!done & tbl >= length(table_SQL)) {
      if (item_exists) {
        stop(str_c("unable to find variable(s) ", str_c(str_c("'", column, "'"), collapse = " ")), " in SQL database")
      } else {
        done <- TRUE
        value <- NULL
      }
    }

  }

  # order or adjust format of variable if needed
  if (!is.null(value)) {

    # set variable
    if (is.null(index)) {
      value <- value[,variable]
    } else {
      value <- value[order(value[, index], decreasing = decreasing), variable]
    }

    # set format variable
    if (s_var_SWAP$format == "date") value <- as_date(x = value)
  }

  # ---- return part of procedure ----

  return(value)
}
