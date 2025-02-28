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
#' "653127198503161793",
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178",
#' "12345678",
#' "65312a198204181793"
#' )
#' birth_date(id)
#'
#' @export

#------------------------------------------------------------------------------#

birth_date = function(id) {
  cnid_info(id)$birth_date
}

#------------------------------------------------------------------------------#