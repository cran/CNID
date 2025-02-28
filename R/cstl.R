#' @title Get constellation information from ID number
#'
#' @description
#' Get constellation information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Constellation vector obtained from ID numbers.
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
#' cstl(id)
#'
#' @export

#------------------------------------------------------------------------------#

cstl = function(id) {
  cnid_info(id)$cstl
}

#------------------------------------------------------------------------------#