#' Create graph label
#'
#' @param label character string, label description.
#' @param ... further arguments passed to or from other methods.
#' @importFrom stringr str_c str_replace_all str_detect str_locate_all str_sub
#' @details An unit can optionally be added to the label by specifying \code{unit}.
#' In case \code{linebreak} is set to TRUE the unit is placed underneath the label text (default: FALSE).
#' @export create_label
create_label <- function(label, ...) {

  # ---- initial part of procedure ----

  label_graph <- NULL

  # set optional arguments
  opt_param <- c("unit", "linebreak")
  unit <- linebreak <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(unit)) unit <- NULL
  if (is.null(linebreak)) linebreak <- FALSE

  # ---- main part of procedure ----

  # replace spaces label
  label <- str_c("'", str_replace_all(string = label, pattern = " ", replacement = "'~'"), "'")


  if (!is.null(unit)) {

    # replace spaces unit
    unit <- str_replace_all(string = unit, pattern = " ", replacement = "~")

    # special case unit kgds
    if (str_detect(string = unit, pattern = "kgds")) {
      unit <- str_replace_all(string = unit, pattern = "kgds", replacement = "kg[ds]")
    }

    # special case unit oC
    if (str_detect(string = unit, pattern = "oC")) {
      unit <- str_replace_all(string = unit, pattern = "oC", replacement = "degree*'C'")
    }

    # special case unit %
    if (str_detect(string = unit, pattern = "%")) {
      unit <- str_replace_all(string = unit, pattern = "%", replacement = "'%'")
    }

    # special case unit -
    if (unit == "-") {
      unit <- str_replace_all(string = unit, pattern = "-", replacement = "'-'")
    }

    # add superscripts in unit
    if (!is.null(unit)) {
      done <- !str_detect(string = unit, pattern = "1|2|3")
      if (!done) {
        pos <- str_locate_all(string = unit, pattern = "1|2|3")[[1]][, "start"]
        n_rec <- length(pos)
        for (i_rec in 1:n_rec) {
          pos_add <- ifelse(str_sub(string = unit, start = pos[i_rec] - 1, end = pos[i_rec] - 1) == "-", pos[i_rec] - 1, pos[i_rec])
          unit <- str_c(str_sub(string = unit, start = 1, end = pos_add - 1), "^", str_sub(string = unit, start = pos_add, end = nchar(unit)))
          pos <- pos + 1
        }
      }
    }
  }

  # combine label and unit
  if (!is.null(unit)) {
    if (linebreak) {
      string_expression <- str_c("atop(bold(", label, "),'['*", unit, "*']')")
    } else {
      string_expression <- str_c("bold(", label, ")~'['*", unit, "*']'")
    }
  } else {
    string_expression <- str_c("bold(", label, ")")
  }

  label_graph <- parse(text = string_expression)

  # ---- return of procedure ----

  return(label_graph)
}

#' Create graph label with unit
#'
#' @param label_text character string, label decription.
#' @param label_unit character string. unit of \code{label_text}.
#' @return label \code{label_text} with unit \code{label_unit}.
#' @export create_label_graph
#' @keywords internal
create_label_graph <- function(label_text, label_unit = NULL) {

  message("function: 'create_label_graph' is deprecated, use create_label instead!")

  # ---- initial part of procedure ----

  label_graph <- NULL

  # ---- main part of procedure ----

  if (!is.null(label_unit)) {
    if (label_unit == "m2")            label_graph <- bquote(bold(.(label_text)) ~ paste("[m"^{2}, "]"))
    if (label_unit == "m3")            label_graph <- bquote(bold(.(label_text)) ~ paste("[m"^{3}, "]"))
    if (label_unit == "m3 s-1")        label_graph <- bquote(bold(.(label_text)) ~ "[m"^{3} ~ paste("s"^{-1}, "]"))
    if (label_unit == "m3 hr-1")       label_graph <- bquote(bold(.(label_text)) ~ "[m"^{3} ~ paste("hr"^{-1}, "]"))
    if (label_unit == "m3 d-1")        label_graph <- bquote(bold(.(label_text)) ~ "[m"^{3} ~ paste("d"^{-1}, "]"))
    if (label_unit == "mm d-1")        label_graph <- bquote(bold(.(label_text)) ~ paste("[mm d"^{-1}, "]"))
    if (label_unit == "cm d-1")        label_graph <- bquote(bold(.(label_text)) ~ paste("[cm d"^{-1}, "]"))
    if (label_unit == "m d-1")         label_graph <- bquote(bold(.(label_text)) ~ paste("[m d"^{-1}, "]"))
    if (label_unit == "m2 m-2")        label_graph <- bquote(bold(.(label_text)) ~ "[m"^{2} ~ paste("m"^{-2}, "]"))
    if (label_unit == "cm2 cm-2")      label_graph <- bquote(bold(.(label_text)) ~ "[cm"^{2} ~ paste("cm"^{-2}, "]"))
    if (label_unit == "m3 m-3")        label_graph <- bquote(bold(.(label_text)) ~ "[m"^{3} ~ paste("m"^{-3}, "]"))
    if (label_unit == "cm3 cm-3")      label_graph <- bquote(bold(.(label_text)) ~ "[cm"^{3} ~ paste("cm"^{-3}, "]"))
    if (label_unit == "kg ha-1")       label_graph <- bquote(bold(.(label_text)) ~ paste("[kg ha"^{-1}, "]"))
    if (label_unit == "kgds ha-1")     label_graph <- bquote(bold(.(label_text)) ~ "[kg"[ds] ~ paste("ha"^{-1}, "]"))
    if (label_unit == "kgds ha-1 d-1") label_graph <- bquote(bold(.(label_text)) ~ "[kg"[ds] ~"ha"^{-1} ~ paste("d"^{-1}, "]"))
    if (label_unit == "stuks ha-1")    label_graph <- bquote(bold(.(label_text)) ~ paste("[stuks ha"^{-1}, "]"))
    if (label_unit == "kg m-3")        label_graph <- bquote(bold(.(label_text)) ~ paste("[kg m"^{-3}, "]"))
    if (label_unit == "kg l-1")        label_graph <- bquote(bold(.(label_text)) ~ paste("[kg l"^{-1}, "]"))
    if (label_unit == "g cm-3")        label_graph <- bquote(bold(.(label_text)) ~ paste("[g cm"^{-3}, "]"))
    if (label_unit == "g l-1")         label_graph <- bquote(bold(.(label_text)) ~ paste("[g l"^{-1}, "]"))
    if (label_unit == "mg cm-3")       label_graph <- bquote(bold(.(label_text)) ~ paste("[mg cm"^{-3}, "]"))
    if (label_unit == "mg l-1")        label_graph <- bquote(bold(.(label_text)) ~ paste("[mg l"^{-1}, "]"))
    if (label_unit == "kJ m-2")        label_graph <- bquote(bold(.(label_text)) ~ paste("[kJ m"^-2, "]"))
    if (label_unit == "kJ m-2 d-1")    label_graph <- bquote(bold(.(label_text)) ~ paste("[kJ m"^-2, " d"^-1,"]"))
    if (label_unit == "oC")            label_graph <- bquote(bold(.(label_text)) ~ paste("[", degree, "C]"))
    if (label_unit == "ha kg-1")       label_graph <- bquote(bold(.(label_text)) ~ paste("[ha kg"^{-1}, "]"))
    if (label_unit == "tillers m-2")   label_graph <- bquote(bold(.(label_text)) ~ paste("[tillers m"^{-2}, "]"))
    if (label_unit == "")              label_graph <- bquote(bold(.(label_text)))
    if (is.null(label_graph))          label_graph <- bquote(bold(.(label_text)) ~ paste("[", .(label_unit), "]"))
  } else {
    label_graph <- bquote(bold(.(label_text)))
  }

  # ---- return of procedure ----

  return(label_graph)
}

#' Create unit expression
#'
#' @param unit character string, unit decription.
#' @return unit expression \code{unit}.
#' @export unit_expression
#' @keywords internal
unit_expression <- function(unit) {

  # ---- main part of procedure ----

  expression <- switch(
    unit,
    "m2" = "'m'^{'3'}",
    "m3" = "'m'^{'3'}",
    "cm3" = "'cm'^{'3'}",
    "mm3" = "'mm'^{'3'}",
    "m3 m-3" = "'m'^{'3'},' m'^{'-3'}",
    "cm3 cm-3" = "'cm'^{'3'},' cm'^{'-3'}",
    "mm3 mm-3" = "'mm'^{'3'},' mm'^{'-3'}",
    "m3 s-1" = "'m'^{'3'},' s'^{'-1'}",
    "m3 hr-1" = "'m'^{'3'},' hr'^{'-1'}",
    "m3 d-1" = "'m'^{'3'},' d'^{'-1'}",
    "m s-1" = "'m s'^{'-1'}",
    "m hr-1" = "'m hr'^{'-1'}",
    "m d-1" = "'m d'^{'-1'}",
    "cm s-1" = "'cm s'^{'-1'}",
    "cm hr-1" = "'cm hr'^{'-1'}",
    "cm d-1" = "'cm d'^{'-1'}",
    "mm s-1" = "'mm s'^{'-1'}",
    "mm hr-1" = "'mm hr'^{'-1'}",
    "mm d-1" = "'mm d'^{'-1'}",
    "t ha-1" = "'t ha'^{'-1'}",
    "kg ha-1" = "'kg ha'^{'-1'}",
    "kgds ha-1" = "'kg'[ds]*' ha'^{'-1'}",
    "stuks ha-1" = "'stuks ha'^{'-1'}",
    "kg m-3" = "'kg m'^{'-3'}",
    "kg l-1" = "'kg l'^{'-1'}",
    "g cm-3" = "'g cm'^{'-3'}",
    "g l-1" = "'g l'^{'-1'}",
    "mg cm-3" = "'mg cm'^{'-3'}",
    "mg l-1" = "'mg l'^{'-1'}",
    "mg cm-2 d-1" = "'mg cm'^{'-2'},' d'^{-1}",
    "mg m-2 d-1" = "'mg m'^{'-2'},' d'^{-1}",
    "kg m-2 d-1" = "'kg m'^{'-2'},' d'^{-1}",
    "kJ m-2" = "'kJ m'^{'-2'}",
    "kJ m-2 d-1" = "'kJ m'^{'-2'},' d'^{-1}",
    "oC" = "degree, 'C'",
    unit
  )

  # ---- return of procedure ----

  return(expression)
}

#' Create graph title
#'
#' @param title_text character string, main title.
#' @param title_unit character string, unit.
#' @param title_sub character string, subtitle.
#' @return label \code{title_text} with unit \code{title_unit} and substitle \code{title_sub}.
#' @export create_title_graph
#' @keywords internal
create_title_graph <- function(title_text, title_unit = NULL, title_sub = NULL) {

  message("function: 'create_title_graph' is deprecated, use create_label instead!")

  # ---- main part of procedure ----

  if (is.null(title_unit)) {
    title_main <- paste0("bold('", title_text, " ')")
  } else {
    title_unit <- unit_expression(unit = title_unit)
    title_main <- paste0("paste(bold('", title_text, " '), plain(paste('[',", title_unit, ",']')))")
  }

  if (is.null(title_sub)) {
    title_graph <- paste0("atop(", title_main, ")")
  } else {
    title_graph <- paste0("atop(", title_main, ", atop(italic('", title_sub, "')))")
  }

  # ---- return of procedure ----

  return(title_graph)
}

#' Get theme for plotting
#'
#' @param theme character string, theme decription.
#' @return mytheme.
#' @importFrom ggplot2 theme element_blank element_line element_text rel unit
#' @export get_my_theme
get_my_theme <- function(theme) {

  # ---- main part of procedure ----

  if (theme == "figure") {
    my_theme <- theme(
      panel.background = element_blank(),
      panel.grid.minor = element_line(colour = "grey95", size = 0.25),
      panel.grid.major = element_line(colour = "grey"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = rel(1.25), face = "bold"),
      strip.text.y = element_text(size = rel(1.25), face = "bold")
    )
  }

  if (theme == "map") {

    my_theme <- theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(colour = "transparent"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = rel(1.25), face = "bold"),
      strip.text.y = element_text(size = rel(1.25), face = "bold"),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "lines")
    )
  }

  if (theme == "legend") {
    my_theme <- theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      strip.text.x = element_text(size = rel(1.25), face = "bold"),
      strip.text.y = element_text(size = rel(1.25), face = "bold"),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  }

  # ---- return of procedure ----

  return(my_theme)
}
