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
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' check_id(id)
#'
#' @export

#------------------------------------------------------------------------------#

check_id = function(id) {
  cnid_info(id)$valid
}

#------------------------------------------------------------------------------#