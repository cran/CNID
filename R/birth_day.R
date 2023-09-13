#' @title Get day of birth information from ID number
#'
#' @description
#' Get day of birth information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Day of birth vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' birth_day(id)
#'
#' @export

#------------------------------------------------------------------------------#

birth_day = function(id) {
  cnid_info(id)$birth_day
}

#------------------------------------------------------------------------------#