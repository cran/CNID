#' @title Get month of birth information from ID number
#'
#' @description
#' Get month of birth information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Month of birth vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' birth_month(id)
#'
#' @export

#------------------------------------------------------------------------------#

birth_month = function(id) {
  cnid_info(id)$birth_month
}

#------------------------------------------------------------------------------#