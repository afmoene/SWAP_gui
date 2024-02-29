#' Create SWAP-model using sqlite-database
#'
#' @param file_swp character string, name of swp-file (to create).
#' @param file_sql character string, name of sql-file.
#' @param run_id numeric, id of run (sql-datebase \code{file_sql}).
#' @param tmplt_swp character string, name of swp-template.
#' @param dir_met character string, directory with meteo-file(s).
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom fs path_dir dir_create file_copy file_exists path_file
#' @importFrom readr read_rds write_lines
#' @importFrom lubridate year
#' @export create_SWAP
create_SWAP <- function(file_swp, file_sql, run_id, tmplt_swp, dir_met, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("dir_ini", "dir_smu", "dir_crp", "dir_atm", "file_irg", "tmplt_dra", "tmplt_bbc", "variable_swap", "sign_open", "sign_close", "quiet")
  dir_ini <- dir_smu <- dir_crp <- dir_atm <- file_irg <- tmplt_dra <- tmplt_bbc <- variable_swap <- sign_open <- sign_close <- quiet <- NULL

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
  if (is.null(dir_atm)) dir_atm <- dir_met
  if (is.null(quiet)) quiet <- FALSE

  # ---- main part of procedure ----

  # set and create run-directory
  dir_run <- path_dir(file_swp)
  if (!file_exists(path = dir_run)) {
    dir_create(path = dir_run)
  }

  # set run information (optional)
  run_info <- get_run_info_SQL(file_sql = file_sql, run_id = run_id)

  # create swp-file
  text_swp <- set_template_SWAP(template = tmplt_swp, run_info = run_info, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close, quiet = quiet)
  if (!quiet) message(str_c("writing file: ", path_file(file_swp)))
  write_lines(x = text_swp, file = file_swp)

  # copy ini-file
  SWINCO <- get_value_SWAP(text = text_swp, variable = "SWINCO", variable_swap = variable_swap)
  if (SWINCO == 3) {
    if (is.null(dir_ini)) stop("in case of initialisation is based on previous simulation, 'dir_ini' should be specified")
    INIFIL <- get_value_SWAP(text = text_swp, variable = "INIFIL", variable_swap = variable_swap)
    path <- str_c(dir_ini, "/", INIFIL)
    new_path <- str_c(dir_run, "/", tolower(INIFIL))
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy smu-file(s)
  SWSOPHY <- get_value_SWAP(text = text_swp, variable = "SWSOPHY", variable_swap = variable_swap)
  if (SWSOPHY == 1) {
    if (is.null(dir_smu)) stop("in case of soil physical tables, 'dir_smu' should be specified")
    FILENAMESOPHY <- get_value_SWAP(text = text_swp, variable = "FILENAMESOPHY", variable_swap = variable_swap)
    path <- str_c(dir_smu, "/", FILENAMESOPHY)
    new_path <- str_c(dir_run, "/", tolower(FILENAMESOPHY))
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'\n"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # create crp-file(s)
  SWCROP <- get_value_SWAP(text = text_swp, variable = "SWCROP", variable_swap = variable_swap)
  if (SWCROP == 1) {
    if (is.null(dir_crp)) stop("in case of crop growth, 'dir_crp' should be specified")
    CROPFIL <- unique(get_value_SWAP(text = text_swp, variable = "CROPROTATION::CROPFIL", variable_swap = variable_swap))
    for (s_CROPFIL in CROPFIL) {
      text <- set_template_SWAP(template = str_c(dir_crp, "/", s_CROPFIL, ".crp"), run_info = run_info, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close, quiet = quiet)
      FILCRP <- str_c(dir_run, "/", s_CROPFIL, ".crp")
      if (!quiet) message(str_c("writing file: ", path_file(FILCRP)))
      write_lines(x = text, file = FILCRP)
    }
  }

  # create irg-file (optional)
  SWIRFIX <- get_value_SWAP(text = text_swp, variable = "SWIRFIX", variable_swap = variable_swap)
  if (SWIRFIX == 1) {
    SWIRGFIL <- get_value_SWAP(text = text_swp, variable = "SWIRGFIL", variable_swap = variable_swap)
    if (SWIRGFIL == 1) {
      if (is.null(file_irg)) stop("in case of fixed irrigation, 'file_irg' should be specified")
      IRGFIL <- get_value_SWAP(text = text_swp, variable = "IRGFIL", variable_swap = variable_swap)
      path_new <- str_c(dir_run, "/", tolower(IRGFIL), ".irg")
      if (!quiet) message(str_c("copy file: '", path_file(file_irg), "' to '", path_file(new_path), "'"))
      file_copy(path = file_irg, new_path = new_path, overwrite = TRUE)
    }
  }

  # create dra-file (optional)
  SWDRA <- get_value_SWAP(text = text_swp, variable = "SWDRA", variable_swap = variable_swap)
  if (SWDRA %in% 1:2) {
    if (is.null(tmplt_dra)) stop("in case of lateral drainage to surface water, 'tmpl_dra' should be specified")
    text <- set_template_SWAP(template = tmplt_dra, run_info = run_info, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close, quiet = quiet)
    DRFIL <- get_value_SWAP(text = text_swp, variable = "DRFIL", variable_swap = variable_swap)
    FILDRA <- str_c(dir_run, "/", DRFIL, ".dra")
    if (!quiet) message(str_c("writing file: ", path_file(FILDRA)))
    write_lines(x = text, file = FILDRA)
  }

  # create bbc-file (optional)
  SWBBCFILE <- get_value_SWAP(text = text_swp, variable = "SWBBCFILE", variable_swap = variable_swap)
  if (SWBBCFILE == 1) {
    if (is.null(tmplt_bbc)) stop("in case bottom boundary is specified in separate file, 'tmpl_bbc' should be specified")
    text <- set_template_SWAP(template = tmplt_bbc, run_info = run_info, variable_swap = variable_swap, sign_open = sign_open, sign_close = sign_close, quiet = quiet)
    BBCFIL <- get_value_SWAP(text = text_swp, variable = "BBCFIL", variable_swap = variable_swap)
    FILBBC <- str_c(dir_run, "/", BBCFIL, ".bbc")
    if (!quiet) message(str_c("writing file: ", path_file(FILBBC)))
    write_lines(x = text, file = FILBBC)
  }

  # copy meteorology
  TSTART <- get_value_SWAP(text = text_swp, variable = "TSTART", variable_swap = variable_swap)
  TEND <- get_value_SWAP(text = text_swp, variable = "TEND", variable_swap = variable_swap)
  METFIL <- get_value_SWAP(text = text_swp, variable = "METFIL", variable_swap = variable_swap)
  if (tolower(path_ext(METFIL)) != "met") {
    METFIL <- str_c(METFIL, ".", formatC(x = year(TSTART):year(TEND) %% 1000, width = 3, flag = "0"))
  }
  path <- str_c(dir_met, "/", METFIL)
  new_path <- str_c(dir_run, "/", tolower(METFIL))
  if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
  file_copy(path = path, new_path = new_path, overwrite = TRUE)

  # copy rain
  SWRAIN <- get_value_SWAP(text = text_swp, variable = "SWRAIN", variable_swap = variable_swap)
  if (SWRAIN == 3) {
    RAINFIL <- get_value_SWAP(text = text_swp, variable = "RAINFIL", variable_swap = variable_swap)
    if (tolower(path_ext(RAINFIL)) != "rain") {
      RAINFIL <- str_c(RAINFIL, ".", formatC(x = year(TSTART):year(TEND) %% 1000, width = 3, flag = "0"))
    }
    path <- str_c(dir_met, "/", RAINFIL)
    new_path <- str_c(dir_run, "/", tolower(RAINFIL))
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy atm-file(s)
  if (SWCROP == 1) {
    CROPFIL <- get_value_SWAP(text = text_swp, variable = "CROPROTATION::CROPFIL", variable_swap = variable_swap)
    CROPTYPE <- get_value_SWAP(text = text_swp, variable = "CROPROTATION::CROPTYPE", variable_swap = variable_swap)
    CROPFIL <- unique(CROPFIL[CROPTYPE %in% 2:3])
    for (s_CROPFIL in CROPFIL) {
      FILCRP <- str_c(dir_run, "/", s_CROPFIL, ".crp")
      SWCO2 <- get_value_SWAP(file = FILCRP, variable = "SWCO2", variable_swap = variable_swap)
      if (SWCO2 == 1) {
        ATMOFIL <- get_value_SWAP(file = FILCRP, variable = "ATMOFIL", variable_swap = variable_swap)
        path <- str_c(dir_atm, "/", ATMOFIL, ".co2")
        new_path <- str_c(dir_run, "/", tolower(ATMOFIL), ".co2")
        if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'"))
        file_copy(path = path, new_path = new_path, overwrite = TRUE)
      }
    }
  }
}

#' Run SWAP-model
#'
#' @param file_swp character string, name of swp-file.
#' @param command character string, name of SWAP-program.
#' @param stop logical, execute R-script after SWAP crash.
#' @param quiet logical.
#' @importFrom stringr str_c
#' @importFrom fs path_dir file_copy file_exists path_file
#' @importFrom controlR is_windows
#' @export run_SWAP
run_SWAP <- function(file_swp, command, stop = TRUE, quiet = FALSE) {

  # ---- initial part of procedure ----

  # set start of program
  timprg_start <- Sys.time()

  # set directories
  dir_wrk <- getwd()
  dir_run <- path_dir(file_swp)

  # ---- main part of procedure ----

  # copy exe SWAP
  path <- command
  new_path <- str_c(dir_run, "/", path_file(command))
  if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'"))
  file_copy(path = path, new_path = new_path, overwrite = TRUE)

  # run SWAP
  setwd(dir_run)
  if (is_windows()) {
    system2(command = path_file(command), args = path_file(file_swp), stdout = FALSE)
  } else {
    Sys.chmod(path_file(command), mode = "0777")
    system2(command = str_c("./", path_file(command)), args = path_file(file_swp), stdout = FALSE)
  }
  setwd(dir_wrk)

  # check if SWAP was successfully ended
  if (!file_exists(str_c(dir_run, "/swap.ok"))) {
    if (stop) {
      stop("!!! SWAP crashed !!!")
    } else {
      message("!!! SWAP crashed !!!")
    }
  } else {
    if (!quiet) {
      timprg_end <- Sys.time()
      timprg_clc <- as.numeric(difftime(time1 = timprg_end, time2 = timprg_start, units = "secs"))
      message(str_c("SWAP successfully ended in ", floor(timprg_clc / 60), " minutes and ", ceiling(timprg_clc %% 60), " seconds"))
    }
  }
}

#' Zip SWAP-model results
#'
#' @param file_swp character string, name of swp-file.
#' @param glob pattern to filter files, optional.
#' @importFrom utils zip
#' @importFrom stringr str_replace str_c
#' @importFrom fs path_dir file_exists dir_ls path_file
#' @export zip_SWAP
zip_SWAP <- function(file_swp, glob = NULL) {

  # ---- initial part of procedure ----

  # set directory
  dir_wrk <- getwd()
  dir_run <- path_dir(file_swp)

  # ---- main part of procedure ----

  # set run-directory and zip-file
  file_zip <- str_replace(string = file_swp, pattern = ".swp", replacement = ".zip")

  if (file_exists(str_c(dir_run, "/swap.ok"))) {

    file_swp <- str_c(dir_run, "/", path_file(file_swp))

    # set pattern based on OUTFIL if needed
    if (is.null(glob)) {
      OUTFIL <- get_value_SWAP(file = file_swp, variable = "OUTFIL")
      glob <- str_c("*", OUTFIL, "*")
    }

    FILES  <- dir_ls(path = dir_run, glob = glob)
    zip(zipfile = file_zip, files = FILES, flags = "-9Xqj")
  }
}

#' Copy SWAP-model
#'
#' @param file_swp character string, name of swp-file.
#' @param dir_run character string, run-directory to copy \code{file_swp} to.
#' @param quiet logical.
#' @importFrom fs path_dir file_copy path_file
#' @importFrom lubridate year
#' @export copy_SWAP
copy_SWAP <- function(file_swp, dir_run, quiet = FALSE) {

  # ---- initial part of procedure ----

  # set directory
  dir_wrk <- getwd()

  # ---- main part of procedure ----

  # extract inp-directory
  dir_inp <- path_dir(file_swp)

  # create run-directory
  dir_create(path = dir_run)

  # copy swp-file
  path <- str_c(file_swp)
  new_path <- str_c(dir_run, "/", path_file(file_swp))
  if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
  file_copy(path = path, new_path = new_path, overwrite = TRUE)


  # copy ini-file
  SWINCO <- get_value_SWAP(file = file_swp, variable = "SWINCO")
  if (SWINCO == 3) {
    INIFIL <- get_value_SWAP(file = file_swp, variable = "INIFIL")
    path <- str_c(dir_inp, "/", INIFIL)
    new_path <- str_c(dir_run, "/", INIFIL)
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy smu-file(s)
  SWSOPHY <- get_value_SWAP(file = file_swp, variable = "SWSOPHY")
  if (SWSOPHY == 1) {
    FILENAMESOPHY <- get_value_SWAP(file = file_swp, variable = "FILENAMESOPHY")
    path <- str_c(dir_inp, "/", FILENAMESOPHY)
    new_path <- str_c(dir_run, "/", FILENAMESOPHY)
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy crp-file(s)
  SWCROP <- get_value_SWAP(file = file_swp, variable = "SWCROP")
  if (SWCROP == 1) {
    CROPFIL <- unique(get_value_SWAP(file = file_swp, variable = "CROPROTATION::CROPFIL"))
    path <- str_c(dir_inp, "/", CROPFIL, ".crp")
    new_path <- str_c(dir_run, "/", CROPFIL, ".crp")
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy meteorology
  TSTART <- get_value_SWAP(file = file_swp, variable = "TSTART")
  TEND <- get_value_SWAP(file = file_swp, variable = "TEND")
  METFIL <- get_value_SWAP(file = file_swp, variable = "METFIL")
  if (tolower(path_ext(METFIL)) != "met") {
    METFIL <- str_c(METFIL, ".", formatC(x = year(TSTART):year(TEND) %% 1000, width = 3, flag = "0"))
  }
  path <- str_c(dir_inp, "/", METFIL)
  new_path <- str_c(dir_run, "/", METFIL)
  if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
  file_copy(path = path, new_path = new_path, overwrite = TRUE)

  # copy rain
  SWRAIN <- get_value_SWAP(file = file_swp, variable = "SWRAIN")
  if (SWRAIN == 3) {
    RAINFIL <- get_value_SWAP(file = file_swp, variable = "RAINFIL")
    if (tolower(path_ext(RAINFIL)) != "rain") {
      RAINFIL <- str_c(RAINFIL, ".", formatC(x = year(TSTART):year(TEND) %% 1000, width = 3, flag = "0"))
    }
    path <- str_c(dir_inp, "/", RAINFIL)
    new_path <- str_c(dir_run, "/", RAINFIL)
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy atm-file(s)
  CROPFIL  <- get_value_SWAP(file = file_swp, variable = "CROPROTATION::CROPFIL")
  CROPTYPE <- get_value_SWAP(file = file_swp, variable = "CROPROTATION::CROPTYPE")
  CROPFIL  <- unique(CROPFIL[CROPTYPE %in% 2:3])
  for (s_CROPFIL in CROPFIL) {
    FILCRP <- str_c(dir_run, "/", s_CROPFIL, ".crp")
    SWCO2  <- get_value_SWAP(file = FILCRP, variable = "SWCO2")
    if (SWCO2 == 1) {
      ATMOFIL <- get_value_SWAP(file = FILCRP, variable = "ATMOFIL")
      path <- str_c(dir_inp, "/", ATMOFIL, ".co2")
      new_path <- str_c(dir_run, "/", ATMOFIL, ".co2")
      if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
      file_copy(path = path, new_path = new_path, overwrite = TRUE)
    }
  }

  # copy dra-file (optional)
  SWDRA  <- get_value_SWAP(file = file_swp, variable = "SWDRA")
  if (SWDRA %in% 1:2) {
    DRFIL <- get_value_SWAP(file = file_swp, variable = "DRFIL")
    path <- str_c(dir_inp, "/", DRFIL, ".dra")
    new_path <- str_c(dir_run, "/", DRFIL, ".dra")
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy bbc-file (optional)
  SWBBCFILE  <- get_value_SWAP(file = file_swp, variable = "SWBBCFILE")
  if (SWBBCFILE == 1) {
    BBCFIL <- get_value_SWAP(file = file_swp, variable = "BBCFIL")
    path <- str_c(dir_inp, "/", BBCFIL, ".bbc")
    new_path <- str_c(dir_run, "/", BBCFIL, ".bbc")
    if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
    file_copy(path = path, new_path = new_path, overwrite = TRUE)
  }

  # copy irg-file (optional)
  SWIRFIX    <- get_value_SWAP(file = file_swp, variable = "SWIRFIX")
  if (SWIRFIX == 1) {
    SWIRGFIL  <- get_value_SWAP(file = file_swp, variable = "SWIRGFIL")
    if (SWIRGFIL == 1) {
      IRGFIL <- get_value_SWAP(file = file_swp, variable = "IRGFIL")
      path <- str_c(dir_inp, "/", IRGFIL, ".irg")
      new_path <- str_c(dir_run, "/", IRGFIL, ".irg")
      if (!quiet) message(str_c("copy file: '", path_file(path), "' to '",path_file(new_path),"'", collapse = "\n"))
      file_copy(path = path, new_path = new_path, overwrite = TRUE)
    }
  }
}


