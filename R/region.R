#' @title Get region information from ID number
#'
#' @description
#' Get region information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Region vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' region(id)
#'
#' @export

#------------------------------------------------------------------------------#

region = function(id) {
  cnid_info(id)$region
}

#------------------------------------------------------------------------------#