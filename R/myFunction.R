#' My function
#'
#' `report_p` does something.
#' @param p A value to return
#' @param digits Rounding of p
#' @import stringr
#'
#' @export

report_p <- function(p, digits = 3) {
  if (p < .001) return("p < .001")

  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace("0.", ".") %>%
    # pad right with zeros
    stringr::str_pad(digits+1, "right", 0)

  p_string <- paste("p =", p_round)

  return(p_string)
}
