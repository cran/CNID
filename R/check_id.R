#' @title Check the ID number for logical errors
#'
#' @description
#' Check the ID number for logical errors.
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
  cnid_info(id)$check_id
}

#------------------------------------------------------------------------------#