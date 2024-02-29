#' Calculate model performance
#'
#' @param actual numeric vector, observed values
#' @param predicted numeric vector, modelled values
#' @param performance character vector, model performance to be calculated
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom Metrics mae mse rmse
#' @importFrom stats cor var
#' @export get_modelperformance
get_modelperformance <- function(actual, predicted, performance, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("index", "digits")
  index <- digits <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(digits)) digits <- 3

  # set possible model performance indexes
  opt_prf <- c("avgobs", "avgmdl", "varobs", "varmdl", "rpearson", "syserr", "rnderr", "mse", "rmse", "mae", "mdleff")

  # ---- main part of procedure ----

  # set index
  if (is.null(index)) {
    idx <- rep(x = 1, times = length(actual))
  } else {
    idx <- index
  }

  # check user-specified input
  s_prf <- performance[!performance %in% opt_prf]
  if (length(s_prf) > 0) stop(str_c("unknown model performance index selected '", s_prf, "'", collapse = "\n"))

  # check length values
  if (length(predicted) != length(actual)) stop("number of observations is not equal to number of modelled values")
  if (length(predicted) != length(idx)) stop("length of index is not equal to number of modelled values")

  # select only modelled values with observations
  rec <- !is.na(predicted) & !is.na(actual) & !is.na(idx)
  predicted <- predicted[rec]
  actual <- actual[rec]
  idx <- idx[rec]

  # set performance
  db_prf <- NULL
  for (s_idx in unique(idx)) {

    rec <- s_idx == idx

    db_tmp <- data.frame(index = s_idx)
    if ("avgobs" %in% performance) db_tmp$avgobs <- mean(actual[rec])
    if ("varobs" %in% performance) db_tmp$varobs <- var(actual[rec])
    if ("avgmdl" %in% performance) db_tmp$avgmdl <- mean(predicted[rec])
    if ("varmdl" %in% performance) db_tmp$varmdl <- var(predicted[rec])
    if ("mae" %in% performance) db_tmp$mae <- mae(actual = actual[rec], predicted = predicted[rec])
    if ("mse" %in% performance) db_tmp$mse <- mse(actual = actual[rec], predicted = predicted[rec])
    if ("rmse" %in% performance) db_tmp$rmse <- rmse(actual = actual[rec], predicted = predicted[rec])
    if ("rpearson" %in% performance) db_tmp$rpearson <- cor(x = actual[rec], y = predicted[rec], method = "pearson")
    if ("syserr" %in% performance) db_tmp$syserr <- mean(actual[rec]) - mean(predicted[rec])
    if ("rnderr" %in% performance) db_tmp$rnderr <- ((actual[rec] - predicted[rec]) - (mean(actual[rec]) - mean(predicted[rec])))^2
    if ("mdleff" %in% performance) db_tmp$mdleff <- 1.0 - (mse(actual = actual[rec], predicted = predicted[rec]) / var(actual[rec]))

    db_prf <- rbind(db_prf, db_tmp)
  }

  # order columns based on input
  db_prf <- db_prf[, c("index", performance)]

  # remove index (if not specified)
  if (is.null(index)) db_prf <- db_prf %>% select(-index)

  # round performance to significant digits
  db_prf[, performance] <- signif(x = db_prf[, performance], digits = digits)

  # ---- return of procedure ----

  return(db_prf)
}

#' Extract output variables of SWAP-model
#'
#' @param file_swp character string, name of swp-file (to create).
#' @param run_info list, index of run_id.
#' @importFrom stringr str_c str_squish str_replace_all str_locate str_sub str_split
#' @importFrom RSQLite dbConnect SQLite dbDisconnect SQLITE_RO
#' @keywords internal
get_INLIST_SWAP <- function(file_swp = NULL, run_info = NULL) {

  message("function get_INLIST_SWAP is deprecated; use extract_variable_inlist instead!")

  # ---- initial part of procedure ----

  # check arguments
  if (!is.null(run_info) & !is.null(file)) stop("either 'run_info' or 'file' should be specified")
  if (is.null(run_info) & is.null(file)) stop("'run_info' or 'file' should be specified")

  # ---- main part of procedure ----

  # get INLIST
  if (!is.null(file_swp)) {

    # extract INLIST from swp-file
    INLIST_CSV <- NULL
    SWCSV <- get_value_SWAP(file = file_swp, variable = "SWCSV")
    if (SWCSV == 1) INLIST_CSV <- get_value_SWAP(file = file_swp, variable = "INLIST_CSV")
    INLIST_CSV_TZ <- NULL
    SWCSV_TZ <- get_value_SWAP(file = file_swp, variable = "SWCSV_TZ")
    if (SWCSV_TZ == 1) INLIST_CSV_TZ <- get_value_SWAP(file = file_swp, variable = "INLIST_CSV_TZ")
    INLIST <- str_c(INLIST_CSV, INLIST_CSV_TZ, sep = ",")
  }
  if (!is.null(run_info)) {

    # extract INLIST from database
    conn <- dbConnect(RSQLite::SQLite(), dbname = run_info$file_sql, flags = RSQLite::SQLITE_RO)
    INLIST_CSV <- get_value_SQL(conn = conn, run_info = run_info, variable = "INLIST_CSV", item_exists = FALSE)
    INLIST_CSV_TZ <- get_value_SQL(conn = conn, run_info = run_info, variable = "INLIST_CSV_TZ", item_exists = FALSE)
    dbDisconnect(conn = conn)
    INLIST <- str_c(INLIST_CSV, INLIST_CSV_TZ, sep = ",")
  }
  INLIST <- str_replace_all(string = str_squish(string = INLIST), pattern = " ", replacement = "")

  # extract variables from INLIST
  variable <- NULL
  done <- FALSE
  while (!done) {
    pos <- str_locate(string = INLIST, pattern = ",|\\[")[,"start"]
    if (is.na(pos)) {
      done <- TRUE
      var <- INLIST
    } else {
      sign <- str_sub(string = INLIST, start = pos, end = pos)
      var <- str_sub(string = INLIST, start = 1, end = pos - 1)
      if (sign == ",") {
        if (pos + 1 >= nchar(INLIST)) {
          done <- TRUE
        } else {
          INLIST <- str_sub(string = INLIST, start = pos + 1, end = nchar(INLIST))
        }
      } else {
        end <- as.numeric(regexpr(pattern = "\\]", text = INLIST))
        depth <- str_split(string = str_sub(string = INLIST, start = pos + 1, end = end - 1), pattern = ",", simplify = TRUE)[1,]
        var <- str_c(var, "[", depth, "]")
        if (end + 1 >= nchar(INLIST)) {
          done <- TRUE
        } else {
          INLIST <- str_sub(string = INLIST, start = end + 2, end = nchar(INLIST))
        }
      }
    }
    variable <- c(variable, var)
  }

  # ---- return of procedure ----

  return(variable)
}

#' Prepare observations belonging to SWAP simulation
#'
#' @param file_swp character string, name of swp-file.
#' @param run_info list, index of run_id.
#' @importFrom fs path_dir file_exists dir_create
#' @importFrom stringr str_c
#' @importFrom lubridate as_date as_datetime
#' @importFrom readr write_csv
#' @importFrom RSQLite dbConnect SQLite dbGetQuery dbDisconnect SQLITE_RO dbListTables dbListFields
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter all_of
#' @export prepare_observed_SWAP
prepare_observed_SWAP <- function(file_swp, run_info) {

  # ---- initial part of procedure ----

  # set and create run-directory
  dir_obs <- str_c(path_dir(file_swp),"/observed")

  # set output variable of SWAP simulation
  variable <- get_INLIST_SWAP(file_swp = file_swp)

  # ---- main part of procedure ----

  # open connection SQL
  conn <- dbConnect(RSQLite::SQLite(), dbname = run_info$file_sql, flags = RSQLite::SQLITE_RO)

  # check for table 'Runs'
  table <- dbListTables(conn = conn)
  if ("Observed" %in% table) {

    # extract fields of table
    field <- dbListFields(conn = conn, name = "Observed")
    if (!all(!variable %in% field)) {

      # check table index
      table_index <- field[regexpr(pattern = "_id", text = field) > 0]
      if (all(table_index %in% names(run_info))) {

        # extract variable from table
        where <- NULL
        for (rec in 1:length(table_index)) where <- c(where, str_c(table_index[rec], " in ('", run_info[[table_index[rec]]], "')"))
        statement <- str_c("SELECT * FROM Observed WHERE ", str_c(where, collapse = " AND "))
        dat <- as_tibble(dbGetQuery(conn = conn, statement = statement))

        # convert to date
        if ("DATE" %in% names(dat)) dat$DATE <- as_date(dat$DATE)
        if ("DATETIME" %in% names(dat)) dat$DATETIME <- format(x = as_datetime(dat$DATETIME), format = "%Y-%m-%d %H:%M:%S")

        index <- c("DATE", "DATETIME", "DEPTH")
        index <- index[index %in% names(dat)]

        # write observations
        variable <- variable[variable %in% names(dat)]
        for (s_variable in variable) {

          # create subset
          s_dat <- select(dat, c(index, all_of(s_variable)))
          names(s_dat) <- c(tolower(index), "value")
          s_dat <- filter(s_dat, !is.na(s_dat$value))

          # write file
          if (nrow(s_dat) > 0) {
            file <- str_c(dir_obs, "/", s_variable, ".csv")
            if (!file_exists(path_dir(file))) dir_create(path = path_dir(file))
            write_csv(x = s_dat, file = file, progress = FALSE, quote = "none", append = FALSE)
          }
        }
      }
    }
  }

  # ---- return of procedure ----

  # close connection
  dbDisconnect(conn = conn)
}

#' Add observations to data
#'
#' @param db tibble of model results.
#' @param dir_obs character string of directory with observations.
#' @param variable character string of SWAP variable.
#' @importFrom readr read_csv
#' @importFrom fs file_exists
#' @importFrom stringr str_c str_to_lower
#' @importFrom lubridate as_date
#' @importFrom dplyr %>% mutate select rename all_of left_join rename_all
#' @export add_observed
#' @keywords internal
add_observed <- function(db, dir_obs, variable) {

  datetime <- value <- observed <- NULL

  # ---- main part of procedure ----

  # load observations
  observations <- FALSE
  if (!is.null(dir_obs)) {
    db_obs <- text <- NULL
    file_obs <- str_c(dir_obs, "/", variable, ".csv")
    if (file_exists(path = file_obs)) {
      db_obs <- read_csv(file = file_obs, col_types = "Dd", comment = "*", lazy = TRUE, progress = FALSE) %>%
        rename_all(.funs = str_to_lower) %>%
        rename(observed = value) %>%
        mutate(date = as_date(datetime)) %>%
        select(date, observed)

      # add observations (if possible)
      if (nrow(db_obs) > 0) {

        observations <- TRUE
        db <- left_join(x = db, y = db_obs, by = "date")

      }
    }
  }

  if (!observations) {
    db <- db %>%
      mutate(observed = NA_real_)
  }

  # ---- return of procedure ----

  return(db)

}

