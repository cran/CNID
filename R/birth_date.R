#' @title Get date of birth information from ID number
#'
#' @description
#' Get date of birth information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Date of birth vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' birth_date(id)
#'
#' @export

#------------------------------------------------------------------------------#

birth_date = function(id) {
  cnid_info(id)$birth_date
}

#------------------------------------------------------------------------------#