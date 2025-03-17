#' @title Check whether the ID number is valid
#'
#' @description
#' Check whether the ID number is valid.
#'
#' @param id A vector of ID numbers.
#'
#' @return A vector of TRUE or FALSE.
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
#' check_id(id)
#'
#' @export

#------------------------------------------------------------------------------#

check_id = function(id) {
  cnid_info(id)$valid
}

#------------------------------------------------------------------------------#