#' @title Get year of birth information from ID number
#'
#' @description
#' Get year of birth information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Year of birth vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' birth_year(id)
#'
#' @export

#------------------------------------------------------------------------------#

birth_year = function(id) {
  cnid_info(id)$birth_year
}

#------------------------------------------------------------------------------#