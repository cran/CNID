#' @title Get age information from ID number
#'
#' @description
#' Get age information from ID number, only by year, not the specific date.
#'
#' @param id A vector of ID numbers.
#'
#' @return Age vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#'   "653127198503161793",
#'   "652801197305161555",
#'   "130206202202291545", 
#'   "110101841125178",
#'   "12345678",
#'   "65312a198204181793"
#' )
#' age_by_year(id)
#'
#' @export

#------------------------------------------------------------------------------#

age_by_year = function(id) {
  cnid_info(id)$age_by_year
}

#------------------------------------------------------------------------------#