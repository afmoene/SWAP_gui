#' Convert string to numeric sequence
#'
#' @param string character string specifying numeric sequence.
#' @importFrom stringr str_replace_all str_split
#' @return Numeric vector
#' @export string_sequence
#' @examples
#' string_sequence(string = "1,2,3-5,10")
#' string_sequence(string = "1,2,3-5,AVG")
string_sequence <- function(string) {

  # ---- main part of procedure ----

  # remove all tabs and spaces
  string <- str_replace_all(string = string, pattern = " |\t", replacement = "")

  # split by comma
  string <- str_split(string = string, pattern = ",", simplify = TRUE)[1,]

  # loop over parts
  sequence <- NULL
  for (s_string in string) {
    s_sequence <- str_split(string = s_string, pattern = "-|:", simplify = TRUE)[1,]
    if (length(s_sequence) == 1) {
      sequence <- c(sequence, s_sequence)
    } else {
      if (length(s_sequence) == 2) {
        sequence <- c(sequence, seq(from = s_sequence[1], to = s_sequence[2], by = 1))
      } else {
        stop("option not implemented")
      }
    }
  }

  # convert to numeric (opional)
  if (all(is_numeric(text = sequence))) {
    sequence <- as.numeric(x = sequence)
  }

  # ---- return of procedure ----

  return(sequence)

}

#' Convert numeric sequence to string
#'
#' @param sequence numeric sequence.
#' @importFrom stringr str_c
#' @return Character string
#' @export sequence_string
#' @examples
#' sequence_string(sequence = c(1,2,3:5,9,11))
#' sequence_string(sequence = c(1,2,3:5,9,11,"AVG"))
#' sequence_string(sequence = c(1))
sequence_string <- function(sequence) {

  # ---- main part of procedure ----

  if (is_numeric(sequence[1])) {
    string <- formatC(x = sequence[1], format = "d")
  } else {
    string <- sequence[1]
  }

  if (length(sequence) > 1) {
    for (rec in 2:length(sequence)) {
      if (all(is_numeric(text = sequence[c(rec, rec - 1)]))) {
        if (as.numeric(sequence[rec]) - as.numeric(sequence[rec - 1]) == 1) {
          if (str_sub(string = string, start = nchar(string), end = nchar(string)) != "-") {
            string <- str_c(string, "-")
          }
          if (rec == length(sequence)) {
            string <- str_c(string, formatC(x = sequence[rec], format = "d"))
          }
        } else {
          if (str_sub(string = string, start = nchar(string), end = nchar(string)) == "-") {
            string <- str_c(string, formatC(x = sequence[rec-1], format = "d"))
          }
          string <- str_c(string, ",", formatC(x = sequence[rec], format = "d"))
        }
      } else {
        if (str_sub(string = string, start = nchar(string), end = nchar(string)) == "-") {
          string <- str_c(string, sequence[rec-1])
        }
        string <- str_c(string, ",", sequence[rec])
      }
    }
  }

  # ---- return of procedure ----

  return(string)
}
